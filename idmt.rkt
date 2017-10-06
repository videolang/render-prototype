#lang racket/base

(provide (all-defined-out))
(require file/convertible
         pict
         (prefix-in pict: pict)
         racket/set
         racket/list
         racket/math
         racket/draw
         racket/class
         racket/gui/base
         racket/serialize
         racket/stxparam
         racket/splicing
         racket/match
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
    [(_ orig-stx name:id supclss (interfaces ...)
        (~or (~optional (~seq #:base? b?) #:defaults ([b? #'#f]))
             (~optional (~seq #:direct-deserialize? dd?) #:defaults ([dd? #'#t])))
        ...
        (~and
         (~seq (~or state:defstate public-state:defpubstate internal-body) ...)
         (~seq body ...)))
     #:with name-deserialize (format-id stx "~a:fdeserialize" #'name)
     (define dd?* (syntax-e #'dd?))
     (unless (or (not dd?*) (eq? 'module (syntax-local-context)))
       (raise-syntax-error #f "Must be defined at the module level" #'orig-stx))
     (define serialize-method (gensym 'serialize))
     (define deserialize-method (gensym 'deserialize))
     (define copy-method (gensym 'copy))
     (define base? (syntax-e (attribute b?)))
     #`(begin
         (define-member-name #,serialize-method serial-key)
         (define-member-name #,deserialize-method deserial-key)
         (define-member-name #,copy-method copy-key)
         #,@(if dd?*
                (list
                 #`(provide name-deserialize)
                 #`(define name-deserialize
                     (make-deserialize-info
                      (λ (sup table public-table)
                        (define this (new name))
                        (send this #,deserialize-method (vector sup table public-table))
                        this)
                      (λ ()
                        (define pattern (new name))
                        (values pattern
                                (λ (other)
                                  (send pattern #,copy-method other)))))))
                '())
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
                 supclss
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

(define-syntax (define-base-idmt* stx)
  (syntax-parse stx
    [(_ name:id super (interfaces ...) body ...)
     #`(~define-idmt #,stx name super (interfaces ...) #:base? #t body ...)]))

(define-syntax (define-idmt stx)
  (syntax-parse stx
    [(_ name:id super
        (~or (~optional (~seq #:interfaces (interfaces ...)) #:defaults ([(interfaces 1) '()])))
        ...
        body ...)
     #`(~define-idmt #,stx name super (interfaces ...) body ...)]))

(define-syntax (define-idmt-mixin stx)
  (syntax-parse stx
    [(_ name:id
        (~or (~optional (~seq #:interfaces (interfaces ...)) #:defaults ([(interfaces 1) '()]))
             (~optional (~seq #:mixins (mixins ...)) #:defaults ([(mixins 1) '()])))
        ...
        body ...)
     #`(begin
         (define (name $)
           (~define-idmt #,stx
                         name
                         ((compose #,@(reverse (attribute mixins))) $)
                         (interfaces ...)
                         #:direct-deserialize? #f
                         body ...)
           name))]))

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
   set-current-extent
   get-current-extent
   draw
   on-mouse-event
   on-keyboard-event))

(define-base-idmt* base$ object% (idmt<$>)
  (super-new)
  (define-state x #f)
  (define-state y #f)
  (define-state width #f)
  (define-state height #f)
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
    (values +inf.0 +inf.0))
  (define/public (set-current-extent nx ny nw nh)
    (set! x nx)
    (set! y ny)
    (set! width nw)
    (set! height nh))
  (define/public (get-current-extent)
    (values x y width height)))

(define-idmt-mixin receiver$
  (super-new)
  (define/public (on-receive event)
    (void))
  (define/public (signal event)
    (on-receive event)))

(define-idmt-mixin signaler$
  (super-new)
  (define-public-state receivers (mutable-set))
  (define/public (signal event)
    (for ([r (in-set receivers)])
      (send r signal event)))
  (define/public (register-receiver x)
    (set-add! receivers x))
  (define/public (unregister-receiver x)
    (set-remove! receivers x)))

(define idmt-canvas%
  (class canvas%
    (init-field idmt)
    (define-values (init-min-width init-min-height)
      (send idmt get-min-extent))
    (super-new [min-width (exact-ceiling init-min-width)]
               [min-height (exact-ceiling init-min-height)]
               [paint-callback (λ (c dc)
                                 (send idmt set-current-extent 0 0 (send c get-width) (send c get-height))
                                 (send idmt draw dc 0 0 (send c get-width) (send c get-height))
                                 (define-values (w h) (send idmt get-min-extent))
                                 (send this min-width (exact-ceiling w))
                                 (send this min-height (exact-ceiling h)))])
    (define/override (on-event event)
      (send idmt on-mouse-event event)
      (send this refresh))
    (define/override (on-char event)
      (send idmt on-keyboard-event event)
      (send this refresh))))

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
    (set! parent other))
  (define/public (get-child-extent child)
    (error 'get-child-extent "IDMT does not have children")))

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
  (define/override (set-current-extent x y w h)
    (super set-current-extent x y w h)
    (define item-height (if (empty? idmt-list) #f (/ h (length idmt-list))))
    (for/fold ([y y])
              ([i (in-list idmt-list)])
      (send i set-current-extent x y w item-height)
      (values (+ y item-height)))
    (void))
  (define/override (on-mouse-event event)
    (super on-mouse-event event)
    (for/list ([i (in-list idmt-list)])
      (send i on-mouse-event event))))

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

(define-idmt button$ (signaler$ widget$)
  (super-new)
  (inherit-field horiz-margin
                 vert-margin)
  (init [(internal-label label) (new label$)])
  (define mouse-state 'up)
  (define-state label* internal-label)
  (define-state up-color "WhiteSmoke")
  (define-state hover-color "Gainsboro")
  (define-state down-color "LightGray")
  (define/override (on-mouse-event event)
    (define-values (x y w h)
      (send this get-current-extent))
    (define x-max (+ x w))
    (define y-max (+ y h))
    (define mouse-x (send event get-x))
    (define mouse-y (send event get-y))
    (define in-button?
      (and (<= x mouse-x x-max)
           (<= y mouse-y y-max)))
    (match (send event get-event-type)
      ['left-down
       (when (and in-button? (eq? mouse-state 'hover))
         (set! mouse-state 'down))]
      ['left-up
       (when (and in-button? (eq? mouse-state 'down))
         (if in-button?
             (set! mouse-state 'hover)
             (set! mouse-state 'up))
         (send this signal this))]
      ['motion
       (match mouse-state
         [(or 'up 'hover)
          (if in-button?
           (set! mouse-state 'hover)
           (set! mouse-state 'up))]
         ['down
          (unless in-button?
            (set! mouse-state 'up))])]
      [_ (void)]))
  (define/override (get-min-extent)
    (define-values (b-w b-h)
      (super get-min-extent))
    (define-values (w h)
      (send label* get-min-extent))
    (values (+ b-w w) (+ b-h h)))
  (define/override (draw dc x y w h)
    (define old-brush (send dc get-brush))
    (send dc set-brush
          (new brush% [color (make-object color%
                               (match mouse-state
                                 ['up up-color]
                                 ['hover hover-color]
                                 ['down down-color]))]))
    (send dc draw-rectangle
          (+ x horiz-margin) (+ y vert-margin)
          (- w (* 2 horiz-margin)) (- h (* 2 vert-margin)))
    (send label* draw dc
          (+ x horiz-margin) (+ y vert-margin)
          (- w (* 2 horiz-margin)) (- h (* 2 vert-margin)))
    (send dc set-brush old-brush)))

(define-idmt toggle$ widget$
  (super-new))

(define-idmt radio$ list-widget$
  (super-new))

(define-idmt field$ widget$
  (super-new))

; ==========

(define f (new frame% [label "IDMT"]))
(define idmt (new vertical-block$))
(define counter 0)
(define-idmt add-item$ (receiver$ base$)
  (super-new)
  (define/override (on-receive event)
    (send idmt add-idmt (new label$ [text (format "Item: ~a" counter)]))
    (set! counter (add1 counter))))
(send idmt add-idmt (new label$ [text "Hello"]))
(send idmt add-idmt (new label$ [text "World"]))
(send idmt add-idmt (new label$ [text "I am an IDMT!!!"]))
(define btn (new button$ [label (new label$ [text "CLICK ME!"])]))
(send btn register-receiver (new add-item$))
(send idmt add-idmt btn)
(new idmt-canvas%
     [parent f]
     [idmt idmt])
(send f show #t)
(serialize idmt)
(deserialize (serialize idmt))
