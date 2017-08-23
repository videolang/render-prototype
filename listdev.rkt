#lang racket

(require video/private/ffmpeg/main
         video/private/init)

(define fmt (av-find-input-format "avfoundation"))
;(set-av-input-format-flags!
; fmt (set-remove (av-input-format-flags fmt) 'nofile))
(define ctx (avformat-alloc-context))
(define avio (avio-alloc-context
              (av-malloc 4096) ;; XXX Need to free!
              4096
              #t
              #f #f #f #f))
;(set-avformat-context-iformat! ctx fmt)
(set-avformat-context-pb! ctx avio)
(avformat-open-input ctx "" fmt #f)
(avformat-find-stream-info ctx #f)
(avdevice-list-devices ctx)
