#lang at-exp racket/gui

(require ffi/unsafe
         ffi/unsafe/define
         ffi/vector
         data/gvector
         opengl
         opengl/util
         video/private/ffmpeg)

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
(define num-bytes (av-image-get-buffer-size 'rgb24
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

(define scr
  (f32vector -1.0 1.0 0.0
              -1.0 -1.0 0.0
              1.0 1.0 0.0
              1.0 -1.0 0.0))
(define tex
  (f32vector 0.0 0.0
             0.0 1.0
             1.0 0.0
             1.0 1.0))
(define vert
  @~a{
 #version 330 core
 layout(location = 0) in vec3 vertexPosition_modelspace;
 layout(location = 1) in vec2 vertexUV;
 out vec2 UV;
 void main(){
  gl_Position.xyz = vertexPosition_modelspace;
  gl_Position.w = 1.0;
  UV = vertexUV;
 }})
(define frag
  @~a{
 #version 330 core
 in vec2 UV;
 out vec3 color;
 uniform sampler2D myTextureSampler;
 void main(){
  color = texture(myTextureSampler, UV).rgb;
 }})

(define glconf (new gl-config%))
(send glconf set-legacy? #f)

(define f (new frame%
               [label "Movie"]
               [width (avcodec-context-width new-ctx)]
               [height (avcodec-context-height new-ctx)]))
(define c (new canvas%
               [parent f]
               [gl-config glconf]
               [style '(gl no-autoclear)]))
(define gl-buff #f)
(define gl-uv-buff #f)
(define gl-tex-buff #f)
(define gl-tex #f)
(define prog #f)
(send c with-gl-context
      (λ ()
        (define arr (u32vector-ref (glGenVertexArrays 1) 0))
        (glBindVertexArray arr)
        (set! gl-buff (u32vector-ref (glGenBuffers 1) 0))
        (glBindBuffer GL_ARRAY_BUFFER gl-buff)
        (glBufferData GL_ARRAY_BUFFER
                      (* (compiler-sizeof 'float) (f32vector-length scr))
                      scr
                      GL_STATIC_DRAW)
        (set! gl-uv-buff (u32vector-ref (glGenBuffers 1) 0))
        (glBindBuffer GL_ARRAY_BUFFER gl-uv-buff)
        (glBufferData GL_ARRAY_BUFFER
                      (* (compiler-sizeof 'float) (f32vector-length tex))
                      tex
                      GL_STATIC_DRAW)
        (set! gl-tex-buff (u32vector-ref (glGenTextures 1) 0))
        (glBindTexture GL_TEXTURE_2D gl-tex-buff)
        (glTexImage2D GL_TEXTURE_2D
                      0
                      GL_RGB
                      (avcodec-context-width new-ctx)
                      (avcodec-context-height new-ctx)
                      0
                      GL_RGB
                      GL_UNSIGNED_BYTE
                      #f)
        (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
        (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)

        (define v-shad (glCreateShader GL_VERTEX_SHADER))
        (define f-shad (glCreateShader GL_FRAGMENT_SHADER))
        (glShaderSource v-shad 1 (vector vert) (s32vector (string-length vert)))
        (glCompileShader v-shad)
        ;(define-values (a b) (glGetShaderInfoLog v-shad (glGetShaderiv v-shad GL_INFO_LOG_LENGTH)))
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
        (set! gl-tex (glGetUniformLocation prog "myTextureSampler"))
        (glViewport 0 0 (avcodec-context-width new-ctx) (avcodec-context-height new-ctx))
        (glClearColor 0.0 0.0 0.0 0.0)))
(send f show #t)

(define packet (av-read-frame avformat))
(define film (make-gvector #:capacity 10000))
(let loop ([data packet]
           [count 0])
  (when (and data (<= count 10000))
    (displayln count)
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
        (send c with-gl-context
              (λ ()
                (for ([i (in-range (avcodec-context-height new-ctx))])
                  (glTexSubImage2D GL_TEXTURE_2D
                                   0
                                   0
                                   i
                                   (avcodec-context-width new-ctx)
                                   1
                                   GL_RGB
                                   GL_UNSIGNED_BYTE
                                   (ptr-add (array-ref (av-frame-data frame-rgb) 0)
                                            (* i linesize))))
                (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
                (glUseProgram prog)
                (glActiveTexture GL_TEXTURE0)
                (glBindTexture GL_TEXTURE_2D gl-tex-buff)
                (glUniform1i gl-tex 0)
                (glEnableVertexAttribArray 0)
                (glBindBuffer GL_ARRAY_BUFFER gl-buff)
                (glVertexAttribPointer 0 3 GL_FLOAT #f 0 #f)
                (glEnableVertexAttribArray 1)
                (glBindBuffer GL_ARRAY_BUFFER gl-uv-buff)
                (glVertexAttribPointer 1 2 GL_FLOAT #f 0 #f)
                (glDrawArrays GL_TRIANGLE_STRIP 0 4)
                (glDisableVertexAttribArray 0)
                (glDisableVertexAttribArray 1)))
        (send c swap-gl-buffers)))
    (loop (av-read-frame avformat data) (+ count count-inc))))
(when packet
  (av-packet-unref packet))
