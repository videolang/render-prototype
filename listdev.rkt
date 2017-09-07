#lang racket

(require video/private/ffmpeg/main
         video/private/ffmpeg-pipeline
         video/private/init)

(define fmt (av-find-input-format "avfoundation"))
(define ctx (avformat-alloc-context))
(with-handlers ([exn? (λ (e) (void))])
  (avformat-open-input ctx "" fmt (build-av-dict (hash "list_devices" "true"))))
