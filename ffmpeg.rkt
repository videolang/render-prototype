#lang at-exp racket/gui

(require racket/generator
         ffi/unsafe
         ffi/unsafe/define
         ffi/vector
         ffi/cvector
         data/gvector
         racket/match
         opengl
         opengl/util
         portaudio
         video/private/ffmpeg
         video/private/ffmpeg-stream
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
                    (audio-decode-frame audio-context
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

;; Video
(define frame (av-frame-alloc))
(define frame-rgb (av-frame-alloc))
(define num-bytes #f)
(define buff #f)
(define sws #f)
(define f #f)
(define c #f)

;; Aduio
(define audio-queue (mk-audioqueue))
(define swr #f)
(define audio-context #f)
(define audio-procs #f)

(stream-file
 "/Users/leif/demo2.mp4"
 #:video-callback (λ (mode obj packet)
                    (match obj
                      [(struct* codec-obj
                                ([codec-context codec-context]))
                       (match mode
                         ['init
                          (set! num-bytes
                                (av-image-get-buffer-size'rgb24
                                                         (avcodec-context-width codec-context)
                                                         (avcodec-context-height codec-context)
                                                         32))
                          (set! buff (av-malloc num-bytes _uint8))
                          (av-image-fill-arrays (av-frame-data frame-rgb)
                                                (av-frame-linesize frame-rgb)
                                                (array-ptr buff)
                                                'rgb24
                                                (avcodec-context-width codec-context)
                                                (avcodec-context-height codec-context)
                                                1)
                          (set! sws
                                (sws-getContext (avcodec-context-width codec-context)
                                                (avcodec-context-height codec-context)
                                                (avcodec-context-pix-fmt codec-context)
                                                (avcodec-context-width codec-context)
                                                (avcodec-context-height codec-context)
                                                'rgb24
                                                SWS-BILINEAR
                                                #f #f #f))
                          
                          (set! f (new frame%
                                       [label "Movie"]
                                       [width (avcodec-context-width codec-context)]
                                       [height (avcodec-context-height codec-context)]))
                          (set! c (new video-canvas%
                                       [width (avcodec-context-width codec-context)]
                                       [height (avcodec-context-height codec-context)]
                                       [parent f]))
                          (send f show #t)]
                         ['loop
                          (with-handlers ([exn:ffmpeg:again? void]
                                          [exn:ffmpeg:eof? void])
                            (avcodec-send-packet codec-context packet)
                            (avcodec-receive-frame codec-context frame)
                            (sws-scale sws
                                       (array-ptr (av-frame-data frame))
                                       (array-ptr (av-frame-linesize frame))
                                       0
                                       (avcodec-context-height codec-context)
                                       (array-ptr (av-frame-data frame-rgb))
                                       (array-ptr (av-frame-linesize frame-rgb)))
                            (define linesize (array-ref (av-frame-linesize frame-rgb) 0))
                            (send c draw-frame
                                  (λ ()
                                    (for ([i (in-range (avcodec-context-height codec-context))])
                                      (glTexSubImage2D
                                       GL_TEXTURE_2D
                                       0
                                       0
                                       i
                                       (avcodec-context-width codec-context)
                                       1
                                       GL_RGB
                                       GL_UNSIGNED_BYTE
                                       (ptr-add (array-ref (av-frame-data frame-rgb) 0)
                                                (* i linesize)))))))
                          (av-packet-unref packet)]
                         ['close (send f show #f)])]))
 #:audio-callback (λ (mode obj packet)
                    (match obj
                      [(struct* codec-obj
                                ([codec-context codec-context]))
                       (match mode
                         ['init
                          (set! audio-context codec-context)
                          (set! swr (swr-alloc))
                          (av-opt-set-int
                           swr "in_channel_layout" (avcodec-context-channel-layout codec-context) 0)
                          (av-opt-set-int swr "out_channel_layout" (cast 'sterio _av-ch _int) 0)
                          (av-opt-set-int
                           swr "in_sample_rate" (avcodec-context-sample-rate codec-context) 0)
                          (av-opt-set-int
                           swr "out_sample_rate" (avcodec-context-sample-rate codec-context) 0)
                          (av-opt-set-sample-fmt swr "in_sample_fmt" 'fltp 0)
                          (av-opt-set-sample-fmt swr "out_sample_fmt" 's16 0)
                          (swr-init swr)
                          (set! audio-procs
                                (stream-play/unsafe
                                 audio-callback 0.2 (avcodec-context-sample-rate codec-context)))]
                         ['loop (audioqueue-put audio-queue packet)]
                         ['close (void)])])))

(av-frame-free frame)
(av-frame-free frame-rgb)
