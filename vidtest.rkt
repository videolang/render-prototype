#lang racket

(require video/render
         video/private/video)

(define vid
  (make-playlist
   #:elements (list
               (make-file #:path "/Users/leif/demo2.mp4")
               (make-file #:path "/Users/leif/demo.mp4"))))

(render vid "/Users/leif")
