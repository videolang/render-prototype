#lang racket

(require ffi/unsafe
         ffi/unsafe/define
         ;ffi/unsafe/define/conventions
         video/private/ffmpeg)

(define testfile "/Users/leif/demo2.mp4")
(av-register-all)
(define avformat (avformat-open-input testfile #f #f))
(avformat-find-stream-info avformat #f)
(av-dump-format avformat 0 testfile 0)
(define strs (avformat-context-streams avformat))
(define-values (codec-ctx codec-index)
  (for/fold ([codec #f]
             [index #f])
            ([i strs]
             [i* (in-naturals)])
    (define c (avstream-codec i))
    (if codec
        (values codec index)
        (and (equal? (avcodec-context-codec-type* c) 'video)
             (values c i*)))))
(define codec-id (avcodec-context-codec-id codec-ctx))
(define codec (avcodec-find-decoder codec-id))
(define new-ctx (avcodec-copy-context codec codec-ctx))
(avcodec-open2 new-ctx codec #f)
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
(define packet (av-read-frame avformat))
(let loop ([data packet]
           [count 0])
  (when (and data (<= count 10))
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
        (displayln (cblock->vector (array-ref (av-frame-data frame-rgb) 0) _uint8 (* 3 (avcodec-context-width new-ctx))))
        ;(displayln (array-ref (av-frame-linesize frame-rgb) 0))
        (newline)
        ))
    (loop (av-read-frame avformat data) (+ count count-inc))))
(when packet
  (av-packet-unref packet))
