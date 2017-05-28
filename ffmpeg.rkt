#lang racket

(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/define/conventions)

(define avcodec-lib (ffi-lib "libavcodec"))
(define-ffi-definer define-avcodec avcodec-lib
  #:make-c-id convention:hyphen->underscore)
(define avformat-lib (ffi-lib "libavformat"))
(define-ffi-definer define-avformat avformat-lib
  #:make-c-id convention:hyphen->underscore)

(define _avcodec-id _fixint)
(define _av-duration-estimation-method _fixint)

(define-cstruct _byte-io-context
  ([buffer _bytes]
   [buffer-size _int]
   [buf-ptr _bytes]
   [buf-end _bytes]
   [opeque _pointer]
   [read-packet _fpointer]
   [write-packet _fpointer]
   [seek _fpointer]
   [pos _int64]
   [must-flush _int]
   [eof-reached _int]
   [write-flag _int]
   [is-streamd _int]
   [max-packet-size _int]
   [checksum _ulong]
   [checksum-ptr _pointer]
   [update-checksum _fpointer]
   [error _int]
   [read-pause _fpointer]
   [read-seek _fpointer]))

(define-cstruct _av-io-interrupt-cb
  ([callback _fpointer]
   [opaque _pointer]))

(define _streams
  (let ()
    (define-cstruct _streams
      ([count _uint]
       [lst _pointer])
      #:alignment 1)
  (make-ctype
   _streams
   #f
   (λ (v)
     (cblock->list (ptr-ref (streams-lst v) _pointer) _pointer (streams-count v))))))

(define-cstruct _avformat-context
  ([av-class _pointer]
   [iformat _pointer]
   [oformat _pointer]
   [priv_data _pointer]
   [pb _pointer]
   [ctx-flags _int]
   [streams _streams]
   [filename (_array _byte 1024)]
   [start-time _int64]
   [duration _int64]
   [bit-rate _int]
   [packet-size _uint]
   [max-delay _uint]
   [flags _int]
   [probesize _uint]
   [max-anlayze-duration _int]
   [key _pointer]
   [keylen _int]
   [nb-programs _uint]
   [programs _pointer]
   [video-codec-id _avcodec-id]
   [audio-codec-id _avcodec-id]
   [subtitle-codec-id _avcodec-id]
   [max-index-size _uint]
   [max-picture-buffer _uint]
   [nb-chapters _uint]
   [chapters _pointer]
   [metadata _pointer]
   [start-time-realtime _int64]
   [fps-probe-size _int]
   [error-recognition _int]
   [interrupt-callback _av-io-interrupt-cb]
   [debug _int]
   [max-interleave-delay _int64]
   [string-std-compliance _int]
   [event-flags _int]
   [max-ts-probe _int]
   [avoid-negative-ts _int]
   [ts-id _int]
   [audio-preload _int]
   [max-chunk-duration _int]
   [max-chunk-size _int]
   [use-wallclock-as-timestamps _int]
   [avio-flags _int]
   [durration-estimation-method _av-duration-estimation-method]
   [skip-initial-bytes _int64]
   [correct-ts-overflow _uint]
   [seek2any _int]
   [flush-packets _int]
   [probe-score _int]
   [format-probesize _int]
   [codec-whitelist _pointer]
   [format-whitelist _pointer]
   [internal _pointer]
   [io-repositioned _int]
   [video-codec _pointer]
   [audio-codec _pointer]
   [subtitle-codec _pointer]
   [data-codec _pointer]
   [metadata-header-padding _int]
   [opaque _pointer]
   [control-message-cb _pointer]
   [output-ts-offset _int64]
   [max-analyze-duration2 _int64]
   [probesize2 _int64]
   [dump-separator _uint8]
   [data-codec-id _avcodec-id]
   [open-cb _pointer]))

(define-avformat av-register-all (_fun -> _void))
(define-avformat avformat-open-input (_fun (out : (_ptr io _avformat-context-pointer/null) = #f)
                                           _path
                                           _pointer
                                           _pointer
                                           -> [r : _bool]
                                           -> (let ()
                                                (when r (error "NOO"))
                                                out)))
(define-avformat av-dump-format (_fun _avformat-context-pointer _int _path _int
                                      -> _void))

(define testfile "/Users/leif/demo2.mp4")
(av-register-all)
(define avformat (avformat-open-input testfile #f #f))
(av-dump-format avformat 0 testfile 0)
(define strs (avformat-context-streams avformat))
strs
