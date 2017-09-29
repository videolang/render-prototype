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
(define copy-key (generate-member-key))

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
    [x:defpubstate
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
     (define copy-method (gensym 'copy))
     (define base? (syntax-e (attribute b?)))
     #`(begin
         (provide name-deserialize)
         (define-member-name #,serialize-method serial-key)
         (define-member-name #,deserialize-method deserial-key)
         (define-member-name #,copy-method copy-key)
         (define name-deserialize
           (make-deserialize-info
            (λ (sup table public-table)
              (define this (new name))
              (send this #,deserialize-method (vector sup table public-table))
              this)
            (λ ()
              (define pattern (new name))
              (values pattern
                      (λ (other)
                        (send pattern #,copy-method other))))))
         (splicing-syntax-parameterize ([defstate-parameter
                                          (syntax-parser
                                            [(_ st:defstate who)
                                             #'(field [st.name st.body (... ...)])]
                                            [(_ st:defpubstate who)
                                             #'(field [st.name st.body (... ...)])])])
           (define name
             (let ()
               #,@(for/list ([i (in-list (attribute state.name))])
                    #`(define-local-member-name #,i))
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
                (define (#,copy-method other)
                  #,(if base?
                        #`(void)
                        #`(super #,copy-method other))
                  #,@(for/list ([i (in-list (attribute state.name))])
                       #`(set! #,i (get-field #,i other)))
                  #,@(for/list ([i (in-list (attribute public-state.name))])
                       #`(set! #,i (get-field #,i other)))
                  (void))
                (#,(if base? #'public #'override) #,copy-method)
                body ...)))))]))

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
   draw
   on-mouse-event
   on-keyboard-event))

(define-base-idmt* base$ object% (idmt<$>)
  (super-new)
  (define/public (add-data key val)
    (void))
  (define/public (draw dc x y w h)
    (void))
  (define/public (get-min-extent)
    (values 0 0))
  (define/public (on-mouse-event event)
    (void))
  (define/public (on-keyboard-event event)
    (void))
  (define/public (get-max-extent)
    (values +inf.0 +inf.0)))

(define idmt-canvas%
  (class canvas%
    (init-field idmt)
    (define-values (min-width min-height)
      (send idmt get-min-extent))
    (super-new [min-width (exact-ceiling min-width)]
               [min-height (exact-ceiling min-height)]
               [paint-callback (λ (c dc)
                                 (send idmt draw dc 0 0 (send c get-width) (send c get-height)))])
    (define/override (on-event event)
      (send idmt on-mouse-event event))
    (define/override (on-char event)
      (send idmt on-keyboard-event event))))

(define-idmt widget$ base$
  (super-new)
  (init-field [font normal-control-font])
  (define-public-state vert-margin 2)
  (define-public-state horiz-margin 2)
  (define-public-state style '())
  (define-public-state parent #f)
  (define/override (get-min-extent)
    (values (* 2 vert-margin) (* 2 horiz-margin)))
  (define/public (register-parent other)
    (set! parent other)))

(define-idmt list-widget$ widget$
  (super-new)
  (define-public-state idmt-list '())
  (define/public (add-idmt idmt)
    (set! idmt-list (append idmt-list (list idmt)))
    (send idmt register-parent this))
  (define/public (remove-idmt idmt)
    (when (empty? idmt-list)
      (error 'remove-idmt "List widget already emtpy"))
    (send idmt register-parent #f)
    (set! idmt-list (take idmt-list (sub1 (length idmt-list))))))

(define-idmt vertical-block$ list-widget$
  (super-new)
  (inherit-field idmt-list)
  (define/override (get-min-extent)
    (define-values (base-w base-h)
      (super get-min-extent))
    (define-values (w h)
      (for/fold ([width 0]
                 [height 0])
                ([i (in-list idmt-list)])
        (define-values (w h)
          (send i get-min-extent))
        (values (max w width)
                (+ h height))))
    (values (+ base-w w) (+ base-h h)))
  (define/override (draw dc x y w h)
    (define item-height (if (empty? idmt-list) #f (/ h (length idmt-list))))
    (for/fold ([y y])
              ([i (in-list idmt-list)])
      (send i draw dc x y w item-height)
      (values (+ y item-height)))
    (void))
  (define/override (on-mouse-event event)
    (define x (send event get-x))
    (define y (send event get-y))
    (void)))

(define-idmt horizontal-block$ list-widget$
  (super-new)
  (inherit-field idmt-list)
  (define/override (get-min-extent)
    (define-values (b-w b-h)
      (super get-min-extent))
    (define-values (w h)
      (for/fold ([width 0]
                 [height 0])
                ([i (in-list idmt-list)])
        (define-values (w h)
          (send i get-min-extent))
        (values (+ w width)
                (max h height))))
    (values (+ b-w w) (+ b-h h)))
  (define/override (draw dc x y w h)
    (define item-width (if (empty? idmt-list) #f (/ w (length idmt-list))))
    (for/fold ([x x])
              ([i (in-list idmt-list)])
      (send i draw dc x y item-width h)
      (values (+ x item-width)))
    (void)))

(define-idmt label$ widget$
  (super-new)
  (inherit-field font
                 horiz-margin
                 vert-margin)
  (init [(internal-text text) #f])
  (define-state text* internal-text)
  (define/override (get-min-extent)
    (define-values (b-w b-h)
      (super get-min-extent))
    (define pic (pict:text (or text* "---") font))
    (values (+ b-w (pict-width pic)) (+ b-h (pict-height pic))))
  (define/override (draw dc x y w h)
    (define old-font (send dc get-font))
    (send dc set-font font)
    (send dc draw-text (or text* "---") (+ horiz-margin x) (+ vert-margin y))
    (send dc set-font old-font)))

(define-idmt button$ widget$
  (super-new)
  (inherit-field horiz-margin
                 vert-margin)
  (init [(internal-label label) (new label$)])
  (define mouse-state 'up)
  (define-state label* internal-label)
  (define-state up-color "WhiteSmoke")
  (define-state down-color "Gainsboro")
  (define-state hover-color "LightGray")
  (define/override (on-mouse-event event)
    (displayln "button mouse event"))
  (define/override (get-min-extent)
    (define-values (b-w b-h)
      (super get-min-extent))
    (define-values (w h)
      (send label* get-min-extent))
    (values (+ b-w w) (+ b-h h)))
  (define/override (draw dc x y w h)
    (send dc draw-rectangle
          (+ x horiz-margin) (+ y vert-margin)
          (- w (* 2 horiz-margin)) (- h (* 2 vert-margin)))
    (send label* draw dc
          (+ x horiz-margin) (+ y vert-margin)
          (- w (* 2 horiz-margin)) (- h (* 2 vert-margin)))))

(define-idmt field$ widget$
  (super-new))

(define f (new frame% [label "IDMT"]))
(define idmt (new vertical-block$))
(send idmt add-idmt (new label$ [text "Hello"]))
(send idmt add-idmt (new label$ [text "World"]))
(send idmt add-idmt (new label$ [text "I am an IDMT!!!"]))
(send idmt add-idmt (new button$ [label (new label$ [text "CLICK ME!"])]))
(new idmt-canvas%
     [parent f]
     [idmt idmt])
(send f show #t)
(serialize idmt)
(deserialize (serialize idmt))
