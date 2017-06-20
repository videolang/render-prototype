#lang racket

(require video/private/video
         (only-in video/private/ffmpeg-pipeline
                  render
                  mk-sink-node
                  bundle-for-file)
         graph)

(current-render-graph (weighted-graph/directed '()))

(define file
  (make-file #:path "/Users/leif/demo2.mp4"))

(define source-node
  (convert file))

(define sink-node
  (mk-sink-node (bundle-for-file "/Users/leif/test.mp4" 'vid+aud)))
(add-vertex! (current-render-graph) sink-node)
(add-edge! (current-render-graph) source-node sink-node)

(render (current-render-graph))