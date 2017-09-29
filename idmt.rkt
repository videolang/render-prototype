#lang racket/base

(provide (all-defined-out))
(require file/convertible
         pict
         (prefix-in pict: pict)
         racket/list
         racket/math
         racket/draw
         racket/class
         racket/gui/base
         racket/serialize
         racket/stxparam
         racket/splicing
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse))

(define serial-key (generate-member-key))
(define deserial-key (generate-member-key))

(begin-for-syntax
  (define-syntax-class defstate
    #:literals (define-state)
    (pattern (define-state name body ...)))
  (define-syntax-class defpubstate
    #:literals (define-public-state)
    (pattern (define-public-state name body ...))))

(define-syntax-parameter defstate-parameter
  (syntax-parser
    [(_ stx who)
     (raise-syntax-error #'who "Use outside of define-idmt is an error" this-syntax)]))

(define-syntax (define-state stx)
  (syntax-parse stx
    [x:defstate
     #`(defstate-parameter #,stx define-state)]))

(define-syntax (define-public-state stx)
  (syntax-parse stx
    [x:defstate
     #`(defstate-parameter #,stx define-public-state)]))

(define-syntax (~define-idmt stx)
 (syntax-parse stx
    [(_ orig-stx name:id sup (interfaces ...)
        (~optional (~seq #:base? b?) #:defaults ([b? #'#f]))
        (~and
         (~seq (~or state:defstate public-state:defpubstate internal-body) ...)
         (~seq body ...)))
     #:with name-deserialize (format-id stx "~a:deserialize" #'name)
     (define serialize-method (gensym 'serialize))
     (define deserialize-method (gensym 'deserialize))
     (define base? (syntax-e (attribute b?)))
     #`(begin
         (provide name-deserialize)
         (define-member-name #,serialize-method serial-key)
         (define-member-name #,deserialize-method deserial-key)
         (define name-deserialize
           (make-deserialize-info
            (λ (sup table public-table)
              (define this (new name))
              (send this #,deserialize-method (vector sup table public-table))
              this)
            (λ ()
              (error "Not implemented yet. :("))))
         (splicing-syntax-parameterize ([defstate-parameter
                                          (syntax-parser
                                            [(_ st:defstate who)
                                             #'(define st.name st.body (... ...))]
                                            [(_ st:defpubstate who)
                                             #'(field [st.name st.body (... ...)])])])
           (define name
             (class/derived
              orig-stx
              (name
               sup
               ((interface* () ([prop:serializable
                                 (make-serialize-info
                                  (λ (this)
                                    (send this #,serialize-method))
                                  #'name-deserialize
                                  #t
                                  (or (current-load-relative-directory) (current-directory)))]))
                interfaces ...)
               #f)
              (define (#,serialize-method)
                (vector #,(if base?
                              #'#f
                              #`(super #,serialize-method))
                        (make-immutable-hash
                         `#,(for/list ([i (in-list (attribute state.name))])
                              #`(#,(syntax->datum i) . ,#,i)))
                        (make-immutable-hash
                         `#,(for/list ([i (in-list (attribute public-state.name))])
                              #`(#,(syntax->datum i) . ,#,i)))))
              (#,(if base? #'public #'override) #,serialize-method)
              (define (#,deserialize-method data)
                (define sup (vector-ref data 0))
                (define table (vector-ref data 1))
                (define public-table (vector-ref data 2))
                #,(if base?
                      #`(void)
                      #`(super #,deserialize-method sup))
                #,@(for/list ([i (in-list (attribute state.name))])
                     #`(set! #,i (hash-ref table '#,(syntax->datum i))))
                #,@(for/list ([i (in-list (attribute public-state.name))])
                     #`(set! #,i (hash-ref public-table '#,(syntax->datum i)))))
              (#,(if base? #'public #'override) #,deserialize-method)
              body ...))))]))

(define-syntax (define-idmt* stx)
  (syntax-parse stx
    [(_ name:id super (interfaces ...) body ...)
     #`(~define-idmt #,stx name super (interfaces ...) body ...)]))

(define-syntax (define-base-idmt* stx)
  (syntax-parse stx
    [(_ name:id super (interfaces ...) body ...)
     #`(~define-idmt #,stx name super (interfaces ...) #:base? #t body ...)]))

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
          (define w (exact-ceiling (max w* 1)))
          (define h (exact-ceiling (max h* 1)))
          (define bit (make-object bitmap% w h))
          (send this draw (new bitmap-dc% [bitmap bit]) 0 0 w h)
          (define s (open-output-bytes))
          (send bit save-file s 'png)
          (get-output-bytes s)]
         [else default]))])
   get-min-extent
   get-max-extent
   draw))

(define-base-idmt* base$ object% (idmt<$>)
  (super-new)
  (define/public (add-data key val)
    (void))
  (define/public (draw dc x y w h)
    (void))
  (define/public (get-min-extent)
    (values 0 0))
  (define/public (get-max-extent)
    (values +inf.0 +inf.0)))

(define-idmt widget$ base$
  (super-new)
  (init-field [font normal-control-font]))

(define-idmt label$ widget$
  (super-new)
  (inherit-field font)
  (init [(internal-text text) #f])
  (define-state text internal-text)
  (define/override (get-min-extent)
    (define pic (pict:text (or text "---") font))
    (values (pict-width pic) (pict-height pic)))
  (define/override (draw dc x y w h)
    (define old-font (send dc get-font))
    (send dc set-font font)
    (send dc draw-text (or text "---") x y)
    (send dc set-font old-font)))

(define-idmt button$ widget$
  (super-new)
  (init [(internal-label label) (new label$)])
  (define-state label internal-label)
  (define/override (get-min-extent)
    (send label get-min-extent))
  (define/override (draw dc x y w h)
    (send label draw dc x y w h)))

(define-idmt field$ widget$
  (super-new))

;(new label$ [text "Hello"])
;(serialize (new label$ [text "Hello"]))
;(deserialize (serialize (new label$ [text "Hello"])))
(new button$)
