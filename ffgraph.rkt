#lang racket

(require video/private/video
         (only-in video/private/ffmpeg-pipeline
                  render
                  mk-filter
                  mk-filter-node
                  mk-sink-node
                  stream-bundle->file)
         graph)

(current-render-graph (weighted-graph/directed '()))

(define file
  ;(make-file #:path "/Users/leif/demo2.mp4")
  (make-playlist
   #:elements (list
               (make-file #:path "/Users/leif/demo2.mp4")
               (make-file #:path "/Users/leif/demo.mp4"))))

(define source-node
  (convert file))

(define filter-node-a
  (mk-filter-node
   (hash 'video (mk-filter "pad"
                           (hash "width" 1920
                                 "height" 1080))
         'audio (mk-filter "anull"))
   #:counts (hash 'video 1 'audio 1)))
(add-vertex! (current-render-graph) filter-node-a)
(add-directed-edge! (current-render-graph) source-node filter-node-a)
(define filter-node-b
  (mk-filter-node
   (hash 'video (mk-filter "fps"
                           (hash "fps" 25)))
   #:counts (hash 'video 1 'audio 1)))
(add-vertex! (current-render-graph) filter-node-b)
(add-directed-edge! (current-render-graph) filter-node-a filter-node-b)

(define filter-node-c
  (mk-filter-node
   (hash 'video (mk-filter "format"
                           (hash "pix_fmts"
                                 "yuv420p")))
   #:counts (hash 'video 1 'audio 1)))
(add-vertex! (current-render-graph) filter-node-c)
(add-directed-edge! (current-render-graph) filter-node-b filter-node-c)

(define sink-node
  (mk-sink-node (stream-bundle->file "/Users/leif/test.mp4" 'vid+aud)
                #:counts (hash 'video 1 'audio 1)))
(add-vertex! (current-render-graph) sink-node)
(add-directed-edge! (current-render-graph) filter-node-c sink-node)

(render (current-render-graph))
