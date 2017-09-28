#lang racket/base

(provide (all-defined-out))
(require file/convertible
         pict
         racket/list
         racket/draw
         racket/class
         racket/gui/base
         racket/serialize
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

(define-syntax (define-state stx)
  (syntax-parse stx
    [x:defstate
     (raise-syntax-error 'define-state "Use outside of define-idmt is an error" stx)]))

(define-syntax (define-public-state stx)
  (syntax-parse stx
    [x:defstate
     (raise-syntax-error 'define-public-state "Use outside of define-idmt is an error" stx)]))

(define-syntax (~define-idmt stx)
 (syntax-parse stx
    [(_ orig-stx name:id sup (interfaces ...)
        (~optional (~seq #:base? b?) #:defaults ([b? #'#f]))
        (~or state:defstate public-state:defpubstate body) ...)
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
            (λ (super table public)
              (define this (new name))
              (send this #,deserialize-method super table public))
            (λ ()
              (error "Not implemented yet. :("))))
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
                           #`(#,(syntax->datum i) ,#,i)))
                     (make-immutable-hash
                      `#,(for/list ([i (in-list (attribute public-state.name))])
                           #`(#,(syntax->datum i) ,#,i)))))
            (#,(if base? #'public #'override) #,serialize-method)
            (define (#,deserialize-method data)
              (define sup (first data))
              (define table (second data))
              (define public-table (third data))
              #,(if base?
                    #`(void)
                    #`(super #,deserialize-method))
              #,@(for/list ([i (in-list (attribute state.name))])
                   #`(set! #,i (hash-ref table #,(syntax->datum i))))
              #,@(for/list ([i (in-list (attribute state.name))])
                   #`(set! #,i (hash-ref public-table #,(syntax->datum i)))))
            (#,(if base? #'public #'override) #,deserialize-method)
            (define state.name state.body ...) ...
            (field [public-state.name public-state.body ...] ...)
            body ...)))]))

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

(define-base-idmt* base$ object% (idmt<$>)
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
  (init [(internal-text text)])
  (super-new)
  (inherit-field font)
  (define-state text internal-text))

(define-idmt button$ base$
  (super-new))

(define-idmt field$ base$
  (super-new))

;(new label$ [text "Hello"])
(serialize (new label$ [text "Hello"]))
