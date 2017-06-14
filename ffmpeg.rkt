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
         video/private/packetqueue
         video/private/video-canvas)

#|
(define audio-decode-frame
  (let ()
    (define frame (av-frame-alloc))
    (define audio-block #f)
    (generator (audio-ctx buffer capacity)
      (let loop ()
        (define packet (packetqueue-get audio-queue audio-block))
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
            (with-handlers ([exn:packetqueue-empty?
                             (λ (e)
                               (set! audio-buff-size 1024)
                               (memset audio-buffer 0 audio-buff-size))])
              ;(raise (exn:packetqueue-empty))
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

(define (empty-encoder-video-proc mode obj)
  (match obj
    [(struct* codec-obj
              ([codec-context ctx]
               [id codec-id]
               [stream str]))
     (match mode
       ['init
        (set-avcodec-context-codec-id! ctx codec-id)
        (set-avcodec-context-bit-rate! ctx 400000)
        (set-avcodec-context-width! ctx 1920)
        (set-avcodec-context-height! ctx 1080)
        (set-avstream-time-base! str 25)
        (set-avcodec-context-time-base! ctx 25)
        (set-avcodec-context-gop-size! ctx 12)
        (set-avcodec-context-pix-fmt! ctx 'yuv420p)
        (when (eq? codec-id 'mpeg2video)
          (set-avcodec-context-max-b-frames! ctx 2))
        (when (eq? codec-id 'mpeg1video)
          (set-avcodec-context-mb-decision! ctx 2))]
       ['open
        (define (alloc-frame ctx)
          (define frame (av-frame-alloc))
          (set-av-frame-format! frame (avcodec-context-pix-fmt ctx))
          (set-av-frame-width! frame (avcodec-context-width ctx))
          (set-av-frame-height! frame (avcodec-context-height ctx))
          (av-frame-get-buffer frame 32))
        (define frame (alloc-frame ctx))
        (define tmp-frame (and (not (eq? (avcodec-context-pix-fmt ctx) 'yuv420p))
                               (alloc-frame ctx)))]
       ['write (error "TODO")]
       ['close (error "TODO")])]))

(define (empty-encoder-audio-proc mode obj)
  (match mode
    ['init
     (match obj
       [(struct* codec-obj
                 ([codec-context ctx]
                  [codec codec]
                  [stream str]))
        (set-avcodec-context-sample-fmt!
         ctx (if (avcodec-sample-fmts codec)
                 (ptr-ref (avcodec-sample-fmts codec))
                 'fltp))
        (set-avcodec-context-bit-rate! ctx 64000)
        (define supported-samplerates (avcodec-supported-samplerates codec))
        (set-avcodec-context-sample-rate!
         ctx
         (if supported-samplerates
             (let loop ([rate #f]
                        [offset 0])
               (define new-rate (ptr-ref (avcodec-supported-samplerates codec)
                                         offset))
               (cond [(= new-rate 44100) new-rate]
                     [(= new-rate 0) (or rate 44100)]
                     [else (loop (or rate new-rate) (add1 offset))]))
             44100))
        (define supported-layouts (avcodec-channel-layouts codec))
        (set-avcodec-context-channel-layout!
         ctx
         (if supported-layouts
             (let loop ([layout #f]
                        [offset 0])
               (define new-layout (ptr-ref (avcodec-channel-layouts codec)
                                           offset))
               (cond [(set-member? new-layout 'stereo) 'stereo]
                     [(set-empty? new-layout) (or layout 'stereo)]
                     [else (loop (or layout new-layout) (add1 offset))]))
             'stereo))
        (set-avcodec-context-channels!
         ctx (av-get-channel-layout-nb-channels (avcodec-context-channel-layout ctx)))
        (set-avstream-time-base! str (/ 1 (avcodec-context-sample-rate ctx)))]
       ['write (error "TODO")]
       ['close (error "TODO")])]))
|#

;; Video
(define frame (av-frame-alloc))
(define frame-rgb (av-frame-alloc))
(define num-bytes #f)
(define buff #f)
(define sws #f)
(define f #f)
(define c #f)

;; Aduio
(define audio-queue (mk-packetqueue))
(define swr #f)
(define audio-context #f)
(define audio-procs #f)

(define (encode-proc mode obj data)
  (match obj
    [(struct* codec-obj ([type type]
                         [stream stream]
                         [codec-context ctx]
                         [codec codec]
                         [id id]))
     (match* (type mode)
       [('video 'init)
        (set-avcodec-context-codec-id! ctx id)
        (set-avcodec-context-bit-rate! ctx 400000)
        (set-avcodec-context-width! ctx 1280);1920)
        (set-avcodec-context-height! ctx 720);1080)
        (set-avcodec-context-time-base! ctx 25)
        (set-avstream-time-base! stream 25)
        (set-avcodec-context-gop-size! ctx 12)
        (set-avcodec-context-pix-fmt! ctx 'yuv420p)
        (set-avcodec-context-color-range! ctx 'mpeg)
        (when (eq? id 'mpeg2video)
          (set-avcodec-context-max-b-frames! ctx 2))
        (when (eq? id 'mpeg1video)
          (set-avcodec-context-mb-decision! ctx 2))]
       [('audio 'init)
        (set-avcodec-context-sample-fmt!
         ctx (if (avcodec-sample-fmts codec)
                 (ptr-ref (avcodec-sample-fmts codec) _avsample-format)
                 'fltp))
        (set-avcodec-context-bit-rate! ctx 64000)
        (define supported-samplerates (avcodec-supported-samplerates codec))
        (set-avcodec-context-sample-rate!
         ctx
         (if supported-samplerates
             (let loop ([rate #f]
                        [offset 0])
               (define new-rate (ptr-ref (avcodec-supported-samplerates codec)
                                         _int
                                         offset))
               (cond [(= new-rate 44100) new-rate]
                     [(= new-rate 0) (or rate 44100)]
                     [else (loop (or rate new-rate) (add1 offset))]))
             44100))
        (define supported-layouts (avcodec-channel-layouts codec))
        (set-avcodec-context-channel-layout!
         ctx
         (if supported-layouts
             (let loop ([layout #f]
                        [offset 0])
               (define new-layout (ptr-ref (avcodec-channel-layouts codec)
                                           offset))
               (cond [(set-member? new-layout 'stereo) 'stereo]
                     [(set-empty? new-layout) (or layout 'stereo)]
                     [else (loop (or layout new-layout) (add1 offset))]))
             'stereo))
        (set-avcodec-context-channels!
         ctx (av-get-channel-layout-nb-channels (avcodec-context-channel-layout ctx)))
        (set-avstream-time-base! stream (/ 1 (avcodec-context-sample-rate ctx)))]
       [('video 'write)
        data]
       #;
       [('audio 'write)
        eof]
       [(_ _)
        data])]))

(define in-bundle (file->stream-bundle "/Users/leif/demo2.mp4"))
(demux-stream in-bundle
              #:by-index-callback (queue-stream))
(define out-bundle (bundle-for-file "/Users/leif/test.mp4"
                                    in-bundle))
(mux-stream out-bundle
            #:by-index-callback (dequeue-stream
                                 #:passthrough-proc encode-proc))

#|
(demux-stream
 (file->stream-bundle "/Users/leif/demo2.mp4")
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
                         ['close (send f show #f)])])))
#|
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
                          (av-opt-set-int swr
                                          "out_channel_layout"
                                          (cast 'sterio _av-channel-layout _int)
                                          0)
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
                         ['loop (packetqueue-put audio-queue packet)]
                         ['close (void)])])))
|#

(av-frame-free frame)
(av-frame-free frame-rgb)
|#