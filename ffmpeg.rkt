#lang at-exp racket/gui

(require ffi/unsafe
         ffi/unsafe/define
         ffi/vector
         data/gvector
         opengl
         opengl/util
         sdl
         video/private/ffmpeg
         video/private/video-canvas)

(SDL_Init (bitwise-ior SDL_INIT_VIDEO SDL_INIT_AUDIO SDL_INIT_TIMER))

(define (get-codec type)
  (for/fold ([codec #f]
             [index #f])
            ([i strs]
             [i* (in-naturals)])
    (define c (avstream-codec i))
    (cond [codec (values codec index)]
          [(equal? (avcodec-context-codec-type* c) type) (values c i*)]
          [else (values codec index)])))

(define testfile "/Users/leif/demo2.mp4")
(av-register-all)
(define avformat (avformat-open-input testfile #f #f))
(avformat-find-stream-info avformat #f)
(av-dump-format avformat 0 testfile 0)
(define strs (avformat-context-streams avformat))
(define-values (codec-ctx codec-index) (get-codec 'video))
(define-values (audio-codec-ctx audio-codec-index) (get-codec 'audio))
(define codec-id (avcodec-context-codec-id codec-ctx))
(define audio-codec-id (avcodec-context-codec-id audio-codec-ctx))
(define codec (avcodec-find-decoder codec-id))
(define audio-codec (avcodec-find-decoder audio-codec-id))
(define new-ctx (avcodec-copy-context codec codec-ctx))
(define audio-new-ctx (avcodec-copy-context audio-codec audio-codec-ctx))
(avcodec-open2 new-ctx codec #f)
(avcodec-open2 audio-new-ctx audio-codec #f)
(define frame (av-frame-alloc))
(define frame-rgb (av-frame-alloc))
(define num-bytes (av-image-get-buffer-size 'rgb24
                                            (avcodec-context-width new-ctx)
                                            (avcodec-context-height new-ctx)
                                            32))
(define buff (av-malloc num-bytes _uint8))
(avpicture-fill (cast frame-rgb _av-frame-pointer _avpicture-pointer)
                (array-ptr buff)
                'rgb24
                (avcodec-context-width new-ctx)
                (avcodec-context-height new-ctx))
(define sws
  (sws-getContext (avcodec-context-width new-ctx)
                  (avcodec-context-height new-ctx)
                  (avcodec-context-pix-fmt new-ctx)
                  (avcodec-context-width new-ctx)
                  (avcodec-context-height new-ctx)
                  'rgb24
                  SWS-BILINEAR
                  #f #f #f))

(define f (new frame%
               [label "Movie"]
               [width (avcodec-context-width new-ctx)]
               [height (avcodec-context-height new-ctx)]))
(define c (new video-canvas%
               [width (avcodec-context-width new-ctx)]
               [height (avcodec-context-height new-ctx)]
               [parent f]))
(send f show #t)

(define packet (av-read-frame avformat))
(define film (make-gvector #:capacity 10000))
(let loop ([data packet]
           [count 0])
  (when (and data (<= count 10000))
    (displayln count)
    (define count-inc 0)
    (when (= (avpacket-stream-index data) codec-index)
      (with-handlers ([exn:ffmpeg:again? void]
                      [exn:ffmpeg:eof? void])
        (avcodec-send-packet new-ctx data)
        (avcodec-receive-frame new-ctx frame)
        (set! count-inc 1)
        (sws-scale sws
                   (array-ptr (av-frame-data frame))
                   (array-ptr (av-frame-linesize frame))
                   0
                   (avcodec-context-height new-ctx)
                   (array-ptr (av-frame-data frame-rgb))
                   (array-ptr (av-frame-linesize frame-rgb)))
        (define linesize (array-ref (av-frame-linesize frame-rgb) 0))
        (send c draw-frame
              (λ ()
                (for ([i (in-range (avcodec-context-height new-ctx))])
                  (glTexSubImage2D GL_TEXTURE_2D
                                   0
                                   0
                                   i
                                   (avcodec-context-width new-ctx)
                                   1
                                   GL_RGB
                                   GL_UNSIGNED_BYTE
                                   (ptr-add (array-ref (av-frame-data frame-rgb) 0)
                                            (* i linesize))))))))
    (loop (av-read-frame avformat data) (+ count count-inc))))
(when packet
  (av-packet-unref packet))
