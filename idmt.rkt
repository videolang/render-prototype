#lang racket/base

(provide (all-defined-out))
(require file/convertible
         pict
         racket/draw
         racket/class
         racket/gui/base
         racket/serialize
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse))

(define-syntax (~define-idmt stx)
  (syntax-parse stx
    [(_ orig-stx name:id super (interfaces ...) body ...)
     #:with name-deserialize (format-id #f "~a:deserialize" #'name)
     #`(begin
         (provide name-deserialize)
         (define-values (name name-deserialize)
           (class/derived
            orig-stx
            (name super (interfaces ...) #'name-deserialize)
            body ...)))]))

(define-syntax (define-idmt* stx)
  (syntax-parse stx
    [(_ name:id super (interfaces ...) body ...)
     #`(~define-idmt #,stx name super (interfaces ...) body ...)]))

(define-syntax (define-idmt stx)
  (syntax-parse stx
    [(_ name:id super body ...)
     #`(~define-idmt #,stx name super () body ...)]))

(define idmt<$>
  (interface*
   ()
   ([prop:convertible
     (λ (this format default)
       (case format
         [(png-bytes)
          (define-values (w* h*) (send this get-min-extent))
          (define w (max w* 1))
          (define h (max h* 1))
          (define bit (make-object bitmap% w h))
          (send this draw (new bitmap-dc% [bitmap bit]) 0 0 w h)
          (define s (open-output-bytes))
          (send bit save-file s 'png)
          (get-output-bytes s)]
         [else default]))])
   get-min-extent
   get-max-extent
   draw))

(define-idmt* base$ object% (idmt<$>)
  (init-field [font normal-control-font])
  (super-new)
  (define/public (add-data key val)
    (void))
  (define/public (draw dc x y w h)
    (void))
  (define/public (get-min-extent)
    (values 0 0))
  (define/public (get-max-extent)
    (values +inf.0 +inf.0)))

(define-idmt label$ base$
  (init text)
  (super-new)
  (inherit-field font))

(define-idmt button$ base$
  (super-new))

(define-idmt field$ base$
  (super-new))

(serialize (new label$ [text "Hello"]))
