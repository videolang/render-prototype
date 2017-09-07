#lang racket

(require racket/logging
         video/private/ffmpeg/main
         video/private/ffmpeg-pipeline
         video/private/init)

(define video-devices-str "AVFoundation video devices:")
(define audio-devices-str "AVFoundation audio devices:")
(define dev-regexp #rx"\\[0-9\\]*(.*)")

(flush-ffmpeg-log!)
(with-intercepted-logging
    (λ (l)
      (match l
        [(vector level message data topic)
         (displayln message)]))
  (λ ()
    (define fmt (av-find-input-format "avfoundation"))
    (define ctx (avformat-alloc-context))
    (with-handlers ([exn? (λ (e) (void))])
      (avformat-open-input ctx "" fmt (build-av-dict (hash "list_devices" "true"))))
    (flush-ffmpeg-log!))
  #:logger ffmpeg-logger
  'info
  (string->symbol "AVFoundation input device"))
