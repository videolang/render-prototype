#lang racket

(require racket/logging
         video/private/ffmpeg/main
         video/private/ffmpeg-pipeline
         video/private/init)

(struct device-list (video
                     audio)
  #:transparent)

(define video-devices-str "AVFoundation video devices:")
(define audio-devices-str "AVFoundation audio devices:")
(define dev-regexp #rx"\\[[0-9]*\\] (.*)")

(define (list-input-devices)
  (define curr-list (box #f))
  (define video-list (box '()))
  (define audio-list (box '()))
  
  (flush-ffmpeg-log!)
  (with-intercepted-logging
      (λ (l)
        (match l
          [(vector level message data topic)
           (match message
             [(regexp video-devices-str)
              (set-box! curr-list video-list)]
             [(regexp audio-devices-str)
              (set-box! curr-list audio-list)]
             [_
              (define dev-name (cadr (regexp-match dev-regexp message)))
              (set-box! (unbox curr-list)
                        (cons dev-name (unbox (unbox curr-list))))])]))
    (λ ()
      (define fmt (av-find-input-format "avfoundation"))
      (define ctx (avformat-alloc-context))
      (with-handlers ([exn? (λ (e) (void))])
        (avformat-open-input ctx "" fmt (build-av-dict (hash "list_devices" "true"))))
      (flush-ffmpeg-log!))
    #:logger ffmpeg-logger
    'info
    (string->symbol "AVFoundation input device"))
  (device-list (reverse (unbox video-list))
               (reverse (unbox audio-list))))

(list-input-devices)
