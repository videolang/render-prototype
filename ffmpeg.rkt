#lang at-exp racket/gui

(require racket/generator
         ffi/unsafe
         ffi/unsafe/define
         ffi/vector
         ffi/cvector
         data/gvector
         opengl
         opengl/util
         portaudio
         video/private/ffmpeg
         video/private/audioqueue
         video/private/video-canvas)

(define audio-decode-frame
  (let ()
    (define frame (av-frame-alloc))
    (define audio-block #f)
    (generator (audio-ctx buffer capacity)
      (let loop ()
        (define packet (audioqueue-get audio-queue audio-block))
        ;(set! audio-block #t)
        (define size (avpacket-size packet))
        (define data-offset 0)
        #|
        (memset buffer 0 capacity)
        (yield capacity)
|#
        (with-handlers ([exn:ffmpeg:again? void]
                        [exn:ffmpeg:eof? void])
          (avcodec-send-packet audio-ctx packet)
          (let loop ()
            (with-handlers ([exn:ffmpeg:again? void]
                            [exn:ffmpeg:eof? void])
              (avcodec-receive-frame audio-ctx frame)
              (define data-size
                (av-frame-nb-samples frame))
              (when (> data-size capacity)
                (error 'audio-decode-frame
                       "Buffer too small (shouldn't happen)"))
              (swr-convert swr
                           (cvector-ptr (cvector _pointer buffer))
                           data-size
                           (av-frame-extended-data frame)
                           data-size)
              (yield (* 2 data-size))
              (loop))))
        (av-packet-unref packet)
        (loop)))))

(define audio-callback
  (let ()
    (define audio-buffer-capacity
      (* 2/3 AVCODEC-MAX-AUDIO-FRAME-SIZE))
    (define audio-buffer
      (malloc _int16 audio-buffer-capacity))
    (define audio-buff-size 0)
    (define audio-buff-index 0)
    (λ (stream len)
      (let loop ([st-offset 0]
                 [len len])
        (when (> len 0)
          ; Buffer used up, got more audio.
          (when (>= audio-buff-index audio-buff-size)
            (with-handlers ([exn:audioqueue-empty?
                             (λ (e)
                               (set! audio-buff-size 1024)
                               (memset audio-buffer 0 audio-buff-size))])
              ;(raise (exn:audioqueue-empty))
              (set! audio-buff-size
                    (audio-decode-frame audio-new-ctx
                                        audio-buffer
                                        audio-buffer-capacity)))
            (set! audio-buff-index 0))
          ;(displayln (cblock->vector audio-buffer _uint8 audio-buff-size))
          (define len* (min (- audio-buff-size audio-buff-index)
                            len))
          (memcpy stream st-offset audio-buffer audio-buff-index len*)
          (set! audio-buff-index (+ audio-buff-index len*))
          (loop (+ st-offset len*) (- len len*))))
      ;(displayln (cblock->vector stream _uint8 len))
      )))

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

(define swr (swr-alloc))
(av-opt-set-int swr "in_channel_layout" (avcodec-context-channel-layout audio-new-ctx) 0)
(av-opt-set-int swr "out_channel_layout" (cast 'sterio _av-ch _int) 0)
(av-opt-set-int swr "in_sample_rate" (avcodec-context-sample-rate audio-new-ctx) 0)
(av-opt-set-int swr "out_sample_rate" (avcodec-context-sample-rate audio-new-ctx) 0)
(av-opt-set-sample-fmt swr "in_sample_fmt" 'fltp 0)
(av-opt-set-sample-fmt swr "out_sample_fmt" 's16 0)
(swr-init swr)

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

(define audio-procs
  (stream-play/unsafe audio-callback 0.2 (avcodec-context-sample-rate audio-new-ctx)))

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
(send f show #f)
