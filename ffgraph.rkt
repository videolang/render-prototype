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
  (make-file #:path "/Users/leif/demo2.mp4"))

(define source-node
  (convert file))

(define filter-node
  (mk-filter-node
   (hash 'video (mk-filter "copy")
         'audio (mk-filter "anull"))))
(add-vertex! (current-render-graph) filter-node)
(add-directed-edge! (current-render-graph) source-node filter-node)

(define sink-node
  (mk-sink-node (stream-bundle->file "/Users/leif/test.mp4" 'vid+aud)))
(add-vertex! (current-render-graph) sink-node)
(add-directed-edge! (current-render-graph) filter-node sink-node)

(render (current-render-graph))
