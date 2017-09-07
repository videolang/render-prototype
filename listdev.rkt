#lang racket

(require video/private/ffmpeg/main
         video/private/ffmpeg-pipeline
         video/private/init)

(define fmt (av-find-input-format "avfoundation"))
;(set-av-input-format-flags!
; fmt (set-remove (av-input-format-flags fmt) 'nofile))
(define ctx (avformat-alloc-context))
;(set-avformat-context-iformat! ctx fmt)
(avformat-open-input ctx "" fmt (build-av-dict (hash "list_devices" "true")))
(avformat-find-stream-info ctx #f)
(avdevice-list-devices ctx)
