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

(define out-width 1920)
(define out-height 1080)

(define (alloc-picture fmt w h)
  (define frame (av-frame-alloc))
  (set-av-frame-format/video! frame fmt)
  (set-av-frame-width! frame w)
  (set-av-frame-height! frame h)
  (av-frame-set-color-range frame 'mpeg)
  (av-frame-get-buffer frame 32)
  frame)

;; Video
(define frame (av-frame-alloc))
(define frame-rgb (alloc-picture 'yuv420p out-width out-height))
;(define buff #f)
(define sws #f)

(define (encode-proc mode obj data queue-ctx)
  (match obj
    [(struct* codec-obj ([type type]
                         [stream stream]
                         [codec-context ctx]
                         [codec codec]
                         [id id]
                         [next-pts next-pts]))
     (match* (type mode)
       [('video 'init)
        (set-avcodec-context-codec-id! ctx id)
        (set-avcodec-context-bit-rate! ctx 400000)
        (set-avcodec-context-width! ctx out-width)
        (set-avcodec-context-height! ctx out-height)
        (set-avcodec-context-time-base! ctx 1/25)
        (set-avstream-time-base! stream 1/25)
        (set-avcodec-context-gop-size! ctx 12)
        (set-avcodec-context-pix-fmt! ctx 'yuv420p)
        (set-avcodec-context-color-range! ctx 'mpeg)
        (when (eq? id 'mpeg2video)
          (set-avcodec-context-max-b-frames! ctx 2))
        (when (eq? id 'mpeg1video)
          (set-avcodec-context-mb-decision! ctx 2))
        #|
        (define num-bytes
          (av-image-get-buffer-size 'yuv420p
                                    (avcodec-context-width ctx)
                                    (avcodec-context-height ctx)
                                    32))
        (define buff (av-malloc num-bytes _uint8))
        (av-image-fill-arrays (av-frame-data frame-rgb)
                              (av-frame-linesize frame-rgb)
                              buff ;(array-ptr buff)
                              'yuv420p
                              (avcodec-context-width ctx)
                              (avcodec-context-height ctx)
                              1)
|#
        (set! sws
              (sws-getContext (avcodec-context-width queue-ctx)
                              (avcodec-context-height queue-ctx)
                              (avcodec-context-pix-fmt queue-ctx)
                              (avcodec-context-width ctx)
                              (avcodec-context-height ctx)
                              (avcodec-context-pix-fmt ctx)
                              'bicubic
                              #f #f #f))
        ]
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
        (cond
          [(eof-object? data) data]
          [else
           (avcodec-send-packet queue-ctx data)
           (avcodec-receive-frame queue-ctx frame)
           (av-frame-make-writable frame-rgb)
           (sws-scale sws
                      (array-ptr (av-frame-data frame))
                      (array-ptr (av-frame-linesize frame))
                      0
                      (avcodec-context-height queue-ctx)
                      (array-ptr (av-frame-data frame-rgb))
                      (array-ptr (av-frame-linesize frame-rgb)))
           (av-packet-unref data)
           (set-av-frame-pts! frame-rgb next-pts)
           (set-codec-obj-next-pts!
            obj (add1 (codec-obj-next-pts obj)))
           (avcodec-send-frame ctx frame-rgb)
           (let loop ()
             (with-handlers ([exn:ffmpeg:again? (λ (e) '())])
               (cons (avcodec-receive-packet ctx) (loop))))])]
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

