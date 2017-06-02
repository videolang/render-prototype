#lang at-exp racket/gui

(require ffi/unsafe
         ffi/unsafe/define
         ffi/vector
         ;ffi/unsafe/define/conventions
         data/gvector
         opengl
         opengl/util
         video/private/ffmpeg
         racket/draw/private/local ;; needed for in-cairo-context
         racket/draw/unsafe/cairo
         racket/draw/unsafe/cairo-lib)

(define-ffi-definer define-cairo cairo-lib)
(define-syntax-rule (_cfun . rest)
  (_fun #:lock-name "cairo-pango-lock" . rest))
(define-cairo cairo_image_surface_create_for_data
  (_cfun _pointer _uint _int _int _int -> _cairo_surface_t))

#|
(define tri
  (f32vector -1.0 1.0 0.0
              -1.0 -1.0 0.0
              1.0 1.0 0.0
              1.0 -1.0 0.0))
(define vert
  @~a{
 #version 330 core
 layout(location = 0) in vec3 vertexPosition_modelspace;
 void main(){
  gl_Position.xyz = vertexPosition_modelspace;
  gl_Position.w = 1.0;
 }})
(define frag
  @~a{
 #version 330 core
 out vec3 color;
 void main(){
  color = vec3(1,0,0);
 }})

(define glconf (new gl-config%))
(send glconf set-legacy? #f)

(define f (new frame%
               [label "Movie"]
               [width 500]
               [height 500]))
(define c (new canvas%
               [parent f]
               [gl-config glconf]
               [min-width 500]
               [min-height 500]
               [style '(gl no-autoclear)]))
(define gl-buff #f)
(define prog #f)
(send c with-gl-context
      (λ ()
        (define arr (glGenVertexArrays 1))
        (glBindVertexArray (u32vector-ref arr 0))
        (set! gl-buff (glGenBuffers 1))
        (glBindBuffer GL_ARRAY_BUFFER (u32vector-ref gl-buff 0))
        (glBufferData GL_ARRAY_BUFFER
                      (* (compiler-sizeof 'float) (f32vector-length tri))
                      tri
                      GL_STATIC_DRAW)
        (define v-shad (glCreateShader GL_VERTEX_SHADER))
        (define f-shad (glCreateShader GL_FRAGMENT_SHADER))
        (glShaderSource v-shad 1 (vector vert) (s32vector (string-length vert)))
        (glCompileShader v-shad)
        (define-values (a b) (glGetShaderInfoLog v-shad (glGetShaderiv v-shad GL_INFO_LOG_LENGTH)))
        (glShaderSource f-shad 1 (vector frag) (s32vector (string-length frag)))
        (glCompileShader f-shad)
        (set! prog (glCreateProgram))
        (glAttachShader prog v-shad)
        (glAttachShader prog f-shad)
        (glLinkProgram prog)
        (glDetachShader prog v-shad)
        (glDetachShader prog f-shad)
        (glDeleteShader v-shad)
        (glDeleteShader f-shad)
        (glUseProgram prog)
        (glViewport 0 0 500 500)
        (glClearColor 0.0 0.0 0.0 0.0)))
(send f show #t)
|#

(define testfile "/Users/leif/demo2.mp4")
(av-register-all)
(define avformat (avformat-open-input testfile #f #f))
(avformat-find-stream-info avformat #f)
(av-dump-format avformat 0 testfile 0)
(define strs (avformat-context-streams avformat))
(define-values (codec-ctx codec-index)
  (for/fold ([codec #f]
             [index #f])
            ([i strs]
             [i* (in-naturals)])
    (define c (avstream-codec i))
    (if codec
        (values codec index)
        (and (equal? (avcodec-context-codec-type* c) 'video)
             (values c i*)))))
(define codec-id (avcodec-context-codec-id codec-ctx))
(define codec (avcodec-find-decoder codec-id))
(define new-ctx (avcodec-copy-context codec codec-ctx))
(avcodec-open2 new-ctx codec #f)
(define frame (av-frame-alloc))
(define frame-rgb (av-frame-alloc))
(define num-bytes (av-image-get-buffer-size 'argb
                                            (avcodec-context-width new-ctx)
                                            (avcodec-context-height new-ctx)
                                            32))
(define buff (av-malloc num-bytes _uint8))
(avpicture-fill (cast frame-rgb _av-frame-pointer _avpicture-pointer)
                (array-ptr buff)
                'rgb24
                (avcodec-context-width new-ctx)
                (avcodec-context-height new-ctx))
(define sws
  (sws-getContext (avcodec-context-width new-ctx)
                  (avcodec-context-height new-ctx)
                  (avcodec-context-pix-fmt new-ctx)
                  (avcodec-context-width new-ctx)
                  (avcodec-context-height new-ctx)
                  'rgb24
                  SWS-BILINEAR
                  #f #f #f))
(define packet (av-read-frame avformat))
(define film (make-gvector #:capacity 10000))
(define img (cairo_image_surface_create CAIRO_FORMAT_ARGB32
                                        (avcodec-context-width new-ctx)
                                        (avcodec-context-height new-ctx)))
(let loop ([data packet]
           [count 1])
  (when (and data (<= count 50))
    ;(displayln count)
    (define count-inc 0)
    (when (= (avpacket-stream-index data) codec-index)
      (with-handlers ([exn:ffmpeg:again? void]
                      [exn:ffmpeg:eof? void])
        (avcodec-send-packet new-ctx data)
        (avcodec-receive-frame new-ctx frame)
        (set! count-inc 1)
        (sws-scale sws
                   (array-ptr (av-frame-data frame))
                   (array-ptr (av-frame-linesize frame))
                   0
                   (avcodec-context-height new-ctx)
                   (array-ptr (av-frame-data frame-rgb))
                   (array-ptr (av-frame-linesize frame-rgb)))
        (define linesize (array-ref (av-frame-linesize frame-rgb) 0))
        (define b (make-object bitmap%
                    (avcodec-context-width new-ctx)
                    (avcodec-context-height new-ctx)))
        (send (new bitmap-dc% [bitmap b]) in-cairo-context
              (λ (target-cr)
                (cairo_surface_flush img)
                (define data (cairo_image_surface_get_data img))
                (for ([i (in-range (avcodec-context-height new-ctx))])
                  (memcpy
                   data
                   (* i (avcodec-context-width new-ctx))
                   (array-ref (av-frame-data frame-rgb) 0)
                   (* i linesize)
                   (* 4 (avcodec-context-width new-ctx))))
                #;
                (displayln (cblock->vector (array-ref (av-frame-data frame-rgb) 0)
                                           _uint8
                                           (* 4 (avcodec-context-width new-ctx))))
                (cairo_surface_mark_dirty img)
                (cairo_set_source_surface target-cr img 10.0 10.0)
                #;
                (displayln (cblock->vector (ptr-add data (* 700 (avcodec-context-width new-ctx)))
                                           _uint8
                                           (* 4 (avcodec-context-width new-ctx))))
                (cairo_paint target-cr)))
        (send b save-file (format "frame~a.png" count) 'png)
        #;
        (displayln 
        (cblock->vector (cairo_image_surface_get_data img)
                        _uint8
                        (* 4
                           (avcodec-context-width new-ctx)
                           (avcodec-context-height new-ctx))))
        #;
        (define fbuff
          (for/vector ([i (in-range (avcodec-context-height new-ctx))])
            (cblock->vector (ptr-add (array-ref (av-frame-data frame-rgb) 0)
                                     (* i linesize))
                            _uint8
                            (* 4 (avcodec-context-width new-ctx)))))
        ;fbuff
        ;(gvector-add! film fbuff)
        #|
        (send c with-gl-context
              (λ ()
                (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
                (glUseProgram prog)
                (glEnableVertexAttribArray 0)
                (glBindBuffer GL_ARRAY_BUFFER (u32vector-ref gl-buff 0))
                (glVertexAttribPointer 0 3 GL_FLOAT #f 0 #f)
                (glDrawArrays GL_TRIANGLE_STRIP 0 4)
                (glDisableVertexAttribArray 0)))
        (send c swap-gl-buffers)
|#
        ))
    (loop (av-read-frame avformat data) (+ count count-inc))))
(when packet
  (av-packet-unref packet))
(cairo_surface_destroy img)
