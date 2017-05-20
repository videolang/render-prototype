#lang racket/base

#|
   Copyright 2016-2017 Leif Andersen

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
|#

;; This is an experimental renderer for h.26x. It is still very incomplete.
;; All I guarentee at the moment is that it doesn't make video crash on installation.
;; Hopefully with time this will get moved out of the experimental section.

(require bitsyntax
         racket/file
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse))

(define-syntax (define-constant stx)
  (syntax-parse stx
    [(_ name:id number:nat width:nat)
     #'(define-syntax name
         (syntax-rules ()
           [(_ #t input ks kf)
            (bit-string-case input
              ([(= number :: bits width) (rest :: binary)]
               (ks number rest))
              [else (kf)])]
           [(_ #f src)
            (number->bit-string number width #t)]))]))
(define-syntax (define-header stx)
  (syntax-parse stx
    [(_ name:id width:nat)
     #'(define-syntax name
         (syntax-rules ()
           [(_ #t input ks kf)
            (bit-string-case input
              ([(body :: integer bits width) (rest :: binary)]
               (ks body rest))
              (else (kf)))]
           [(_ #f)
            (bit-string width (str :: binary))]))]))

(define-syntax (define-vlc-table stx)
  (syntax-parse stx
    [(_ name:id (tag number:nat width:nat) ...)
     #'(define-syntax name
         (syntax-rules ()
           [(_ #t input ks kf)
            (bit-string-case input
              ([(= number :: bits width) (rest :: binary)]
               (ks tag rest))
              ...)]
           [(_ #f str)
            (raise-syntax-error "Not done yet")]))]))

;; ===================================================================================================

(define-constant PSC #b00000000000000010000 20)
(define-header TR 5)
(define-header PTYPE 6)
(define-header PEI 1)
(define-header PSPARE 8)

(define-constant GBSC #b0000000000000001 16)
(define-header GN 4)
(define-header GQUANT 5)
(define-header GEI 1)
(define-header GSPARE 8)

(define-vlc-table MBA
  (1        #b1 1)
  (2        #b011 3)
  (3        #b010 3)
  (4        #b0011 4)
  (5        #b0010 4)
  (6        #b00011 5)
  (7        #b00010 5)
  (8        #b0000111 7)
  (9        #b0000110 7)
  (10       #b00001011 8)
  (11       #b00001010 8)
  (12       #b00001001 8)
  (13       #b00001000 8)
  (14       #b00000111 8)
  (15       #b00000110 8)
  (16       #b0000010111 10)
  (17       #b0000010110 10)
  (18       #b0000010101 10)
  (19       #b0000010100 10)
  (20       #b0000010011 10)
  (21       #b0000010010 10)
  (22       #b00000100011 11)
  (23       #b00000100010 11)
  (24       #b00000100001 11)
  (25       #b00000100000 11)
  (26       #b00000011111 11)
  (27       #b00000011110 11)
  (28       #b00000011101 11)
  (29       #b00000011100 11)
  (30       #b00000011011 11)
  (31       #b00000011010 11)
  (32       #b00000011001 11)
  (stuffing #b00000001111 11)
  (start    #b0000000000000001 16))

;; ===================================================================================================

(define (read-frame str)
  (bit-string-case str
    ([(:: (PSC))
      (tr :: (TR))
      (ptype :: (PTYPE))
      (pei :: (PEI))
      (rest :: binary)]
     (if (= pei 1)
         (read-pspare rest)
         (read-gob rest)))))

(define (read-pspare str)
  (bit-string-case str
    ([(pspare :: (PSPARE))
      (pei :: (PEI))
      (rest :: binary)]
     (if (= pei 1)
         (read-pspare rest)
         (read-gob rest)))))

(define (read-gob str)
  (bit-string-case str
    ([(gbsc :: (GBSC))
      (gn :: (GN))
      (gquant :: (GQUANT))
      (rest :: binary)]
     (displayln "yay"))))

;; ===================================================================================================

(define file
  (file->bytes "/Users/leif/demo.h261"))

(read-frame file)

