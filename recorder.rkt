#lang racket/base

;; A prototype camera recorder for video

(require video/base
         video/devices
         video/private/video-canvas
         racket/gui/base
         racket/class)

(define WIDTH 640)
(define HEIGHT 480)
(define FPS 25)

(define video-capture%
  (class frame%
    (super-new [label "Video Capture"]
               [min-width 700]
               [min-height 600])

    (define devices (list-input-devices))
    
    (define screen-row
      (new horizontal-pane%
           [parent this]
           [alignment '(center center)]
           [spacing 20]))
    (define screen
      (new video-canvas%
           [parent screen-row]
           [width WIDTH]
           [height HEIGHT]))
    (define dev-row
      (new horizontal-pane%
           [parent this]
           [alignment '(center center)]
           [spacing 20]))
    (define cam-source
      (new choice%
           [parent dev-row]
           [choices (cameras devices)]
           [label "Camera"]
           [min-width 200]
           [stretchable-width #t]
           [style '(vertical-label)]))
    (define screen-source
      (new choice%
           [parent dev-row]
           [choices (video-devices devices)]
           [label "Screen Capture"]
           [min-width 200]
           [stretchable-width #t]
           [style '(vertical-label)]))
    (define aud-source
      (new choice%
           [parent dev-row]
           [choices (audio-devices devices)]
           [label "Audio Capture"]
           [min-width 200]
           [stretchable-width #t]
           [style '(vertical-label)]))
    ))

(define vc (new video-capture%))
(send vc show #t)
