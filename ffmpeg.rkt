#lang at-exp racket/gui

(require ffi/unsafe
         ffi/unsafe/define
         ffi/vector
         data/gvector
         opengl
         opengl/util
         portaudio
         video/private/ffmpeg
         video/private/audioqueue
         video/private/video-canvas)


(define audio-callback
  (let ()
    (define base-frames 0)
    (λ (setter frames)
      (void))))

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
;(av-dump-format avformat 0 testfile 0)
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
(void
 (av-image-fill-arrays (av-frame-data frame-rgb)
                       (av-frame-linesize frame-rgb)
                 (array-ptr buff)
                 'rgb24
                 (avcodec-context-width new-ctx)
                 (avcodec-context-height new-ctx)
                 1))
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


(define audio-queue (mk-audioqueue))

;;; XXX EWW...too small...
(define audio-buf-size (/ (* 0.15 AVCODEC-MAX-AUDIO-FRAME-SIZE)
                         (avcodec-context-sample-rate audio-new-ctx)))
(stream-play audio-callback audio-buf-size (avcodec-context-sample-rate audio-new-ctx))

(define film (make-gvector #:capacity 10000))
(let loop ()
  (define packet (av-read-frame avformat))
  (when packet
    (with-handlers ([exn:ffmpeg:again? void]
                    [exn:ffmpeg:eof? void])
      (cond
        [(= (avpacket-stream-index packet) codec-index)
         (avcodec-send-packet new-ctx packet)
         (avcodec-receive-frame new-ctx frame)
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
                                             (* i linesize))))))
         (av-packet-unref packet)]
        [(= (avpacket-stream-index packet) audio-codec-index)
         (audioqueue-put audio-queue packet)]
        [else
         (av-packet-unref packet)]))
    (loop)))

(av-frame-free frame)
(av-frame-free frame-rgb)
(avcodec-close codec-ctx)
(avcodec-close new-ctx)
(avcodec-close audio-codec-ctx)
(avcodec-close audio-new-ctx)
(avformat-close-input avformat)
