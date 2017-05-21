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
               (ks (#%datum . tag) rest))
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
(define-vlc-table MTYPE
  ((intra (TCOEFF)) 1 4)
  ((intra (MQUANT TCOEFF)) 1 7)
  ((intra (CBP TCOEFF)) 1 1)
  ((intra (MQUANT CBP TCOEFF)) 1 5)
  ((intra MC (MVD)) 1 9)
  ((intra MC (MVD CBP TCOEFF)) 1 8)
  ((intra MC (MQUANT MVD CBP TCOEFF)) 1 10)
  ((intra MC FIL (MVD)) 1 3)
  ((intra MC FIL (MVD CBP TCOEFF)) 1 2)
  ((intra MC FIL (MQUANT MVD CBP TCOEFF)) 1 6))
(define-header MQUANT 5)
(define-vlc-table MVD
  (16 #b11001 11)
  (17 #b11011 11)
  (18 #b11101 11)
  (19 #b11111 11)
  (20 #b100001 11)
  (21 #b100011 11)
  (22 #b10011 10)
  (23 #b10101 10)
  (24 #b10111 10)
  (25 #b111 8)
  (26 #b1001 8)
  (27 #b1011 8)
  (28 #b111 7)
  (29 #b11 5)
  (30 #b11 4)
  (31 #b11 3)
  (0 #b1 1)
  (1 #b10 3)
  (2 #b10 4)
  (3 #b10 5)
  (4 #b110 7)
  (5 #b1010 8)
  (6 #b1000 8)
  (7 #b110 8)
  (8 #b10110 10)
  (9 #b10100 10)
  (10 #b10010 10)
  (11 #b100010 11)
  (12 #b100000 11)
  (13 #b11110 11)
  (14 #b11100 11)
  (15 #b11010 11))
(define MVD-mod 32)
(define-vlc-table CBP
  (60 #b111 3)
  (4 #b1101 4)
  (8 #b1100 4)
  (16 #b1011 4)
  (32 #b1010 4)
  (12 #b10011 5)
  (48 #b10010 5)
  (20 #b10001 5)
  (40 #b10000 5)
  (28 #b01111 5)
  (44 #b01110 5)
  (52 #b01101 5)
  (56 #b01100 5)
  (1 #b01011 5)
  (61 #b01010 5)
  (2 #b01001 5)
  (62 #b01000 5)
  (24 #b001111 6)
  (36 #b001110 6)
  (3 #b001101 6)
  (63 #b001100 6)
  (5 #b0010111 7)
  (9 #b0010110 7)
  (17 #b0010101 7)
  (33 #b0010100 7)
  (6 #b0010011 7)
  (10 #b0010010 7)
  (18 #b0010001 7)
  (34 #b0010000 7)
  (7 #b00011111 8)
  (11 #b00011110 8)
  (19 #b00011101 8)
  (35 #b00011100 8)
  (13 #b00011011 8)
  (49 #b00011010 8)
  (21 #b00011001 8)
  (41 #b00011000 8)
  (14 #b00010111 8)
  (50 #b00010110 8)
  (22 #b00010101 8)
  (42 #b00010100 8)
  (15 #b00010011 8)
  (51 #b00010010 8)
  (23 #b00010001 8)
  (43 #b00010000 8)
  (25 #b00001111 8)
  (37 #b00001110 8)
  (26 #b00001101 8)
  (38 #b00001100 8)
  (29 #b1011 8)
  (45 #b1010 8)
  (53 #b1001 8)
  (57 #b1000 8)
  (30 #b111 8)
  (46 #b110 8)
  (54 #b101 8)
  (58 #b100 8)
  (31 #b111 9)
  (47 #b110 9)
  (55 #b101 9)
  (59 #b100 9)
  (27 #b11 9)
  (39 #b10 9)) 

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
      (gei :: (GEI))
      (rest :: binary)]
     (if (= gei 1)
         (read-gspare rest)
         (read-MB rest)))))

(define (read-gspare str)
  (bit-string-case str
    ([(gspace :: (GSPARE))
      (gei :: (GEI))
      (rest :: binary)]
     (if (= gei 1)
         (read-gspare rest)
         (read-MB rest)))))

(define (read-MB str)
  (bit-string-case str
    ([(mba :: (MBA))
      (mtype :: (MTYPE))
      (rest :: binary)]
     (displayln mtype))))

;; ===================================================================================================

(define file
  (file->bytes "/Users/leif/demo.h261"))

(read-frame file)

