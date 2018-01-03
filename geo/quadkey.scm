;;; quadkey.scm - A Quadkey implementation in Gauche

;; See also: Bing Maps Tile System
;; https://msdn.microsoft.com/ja-jp/library/bb259689.aspx

;; Copyright (c) 2018, OOHASHI Daichi

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

(define-module geo.quadkey
  (export make-quadkey quadkey? quadkey-value quadkey-level
          quadkey-level? min-quadkey-level max-quadkey-level
          valid-latitude? max-latitude min-latitude
          valid-longitude? max-longitude min-longitude
          quadkey->string quadkey->latitude&longitude
          string->quadkey
          quadkey-has-parent? quadkey-parent
          quadkey-has-child? quadkey-child
          quadkey-sibling
          quadkey-left-neighbor quadkey-right-neighbor
          quadkey-top-neighbor quadkey-bottom-neighbor)
  (use srfi-11)
  (use srfi-13)
  (use srfi-60)
  (use srfi-141)
  (use srfi-145)
  (use gauche.record)
  (use math.const))

(select-module geo.quadkey)

(define-record-type quadkey
    %make-quadkey
    quadkey?
  (value quadkey-value)
  (level quadkey-level))

(define-method write-object ((qk quadkey) port)
  (format port "#<quadkey(~A) ~A>"
          (quadkey-level qk)
          (quadkey->string qk)))

(define min-quadkey-level 1)

(define max-quadkey-level 23)

(define (quadkey-level? x)
  (and (integer? x)
       (<= min-quadkey-level x max-quadkey-level)))

(define tile-size 256)

(define (map-size level)
  (assume (quadkey-level? level))
  (arithmetic-shift tile-size level))

(define (pixel-point->tile-point p)
  (exact (floor (/ p tile-size))))

(define (tile-point->pixel-point p)
  (* p tile-size))

(define (degrees->radians deg)
  (/ (* deg pi) 180))

(define (radians->degrees rad)
  (/ (* rad 180) pi))

(define max-latitude 85.05112878)

(define min-latitude (- max-latitude))

(define (valid-latitude? x)
  (and (real? x)
       (<= min-latitude x max-latitude)))

(define max-longitude 180)

(define min-longitude (- max-longitude))

(define (valid-longitude? x)
  (and (real? x)
       (<= min-longitude x max-longitude)))

(define (make-quadkey lat lng level)
  (assume (valid-latitude? lat))
  (assume (valid-longitude? lng))
  (let ((m (map-size level)))
    (define (normalize v)
      (clamp (exact (floor (+ (* m v) 0.5)))
             0
             (- m 1)))
    (let* ((px (normalize (/ (+ lng 180) 360)))
           (py (normalize (- 0.5 (/ (atanh (sin (degrees->radians lat)))
                                    2 pi))))
           (tx (pixel-point->tile-point px))
           (ty (pixel-point->tile-point py))
           (v (merge-bits tx ty)))
      (%make-quadkey v level))))

(define (quadkey->latitude&longitude qk)
  (let ((m (map-size (quadkey-level qk))))
    (define (normalize v)
      (/ (clamp v 0 (- m 1)) m))
    (let*-values (((tx ty) (unmerge-bits (quadkey-value qk)))
                  ((px) (tile-point->pixel-point tx))
                  ((py) (tile-point->pixel-point ty))
                  ((x) (- (normalize px) 0.5))
                  ((y) (- 0.5 (normalize py))))
      (values (- 90 (* 2 (radians->degrees (atan (exp (* -2 pi y))))))
              (* 360 x)))))

(define (merge-bits x y)
  (assume (and (integer? x)
               (integer? y)
               (not (negative? x))
               (not (negative? y)))
    "arguments should be non-negative integers:" x y)
  (let ((n (+ -1 (max (integer-length x)
                      (integer-length y)))))
    (do ((r 0 (bitwise-ior (arithmetic-shift r 2)
                           (booleans->integer (logbit? n y) (logbit? n x))))
         (n n (- n 1)))
        ((< n 0) r))))

(define (quadkey->string qk)
  (string-pad (number->string (quadkey-value qk) 4)
              (quadkey-level qk)
              #\0))

(define (string->quadkey s)
  (let ((level (string-length s))
        (v (string->number s 4)))
    (%make-quadkey v level)))

(define (unmerge-bits bits)
  (assume (and (integer? bits)
               (not (negative? bits))))
  (let ((n (+ -1 (* 2 (ceiling/ (integer-length bits) 2)))))
    (do ((x 0 (copy-bit 0
                        (arithmetic-shift x 1)
                        (logbit? (* 2 n) bits)))
         (y 0 (copy-bit 0
                        (arithmetic-shift y 1)
                        (logbit? (+ 1 (* 2 n)) bits)))
         (n n (- n 1)))
        ((< n 0) (values x y)))))

(define (quadkey-position qk)
  (ecase (logand 3 (quadkey-value qk))
    ((#b00) 'top-left)
    ((#b01) 'top-right)
    ((#b10) 'bottom-left)
    ((#b11) 'bottom-right)))

(define (quadkey-has-parent? qk)
  (> (quadkey-level qk) min-quadkey-level))

(define (quadkey-parent qk)
  (assume (quadkey-has-parent? qk))
  (%make-quadkey (%quadkey-parent (quadkey-value qk))
                 (- (quadkey-level qk) 1)))

(define (quadkey-has-child? qk)
  (< (quadkey-level qk) max-quadkey-level))

(define (quadkey-child qk pos)
  (assume (quadkey-has-child? qk))
  (%make-quadkey (%quadkey-child (quadkey-value qk) pos)
                 (+ (quadkey-level qk) 1)))

(define (quadkey-sibling qk pos)
  (%make-quadkey (%quadkey-child (%quadkey-parent (quadkey-value qk))
                                 pos)
                 (quadkey-level qk)))

(define (%quadkey-position->integer pos)
  (ecase pos
    ((top-left) #b00)
    ((top-right) #b01)
    ((bottom-left) #b10)
    ((bottom-right) #b11)))

(define (%quadkey-parent v)
  (arithmetic-shift v -2))

(define (%quadkey-child v pos)
  (logior (arithmetic-shift v 2)
          (%quadkey-position->integer pos)))

(define (%quadkey-neighbor f qk)
  (let-values (((recur pos) (f qk)))
    (if recur
        (quadkey-child (%quadkey-neighbor f (quadkey-parent qk))
                       pos)
        (quadkey-sibling qk pos))))

(define quadkey-left-neighbor
  (let ()
    (define (f qk)
      (let ((has-parent (quadkey-has-parent? qk)))
        (case (quadkey-position qk)
          ((top-left)
           (values has-parent 'top-right))
          ((top-right)
           (values #f 'top-left))
          ((bottom-left)
           (values has-parent 'bottom-right))
          ((bottom-right)
           (values #f 'bottom-left)))))
    (lambda (qk)
      (%quadkey-neighbor f qk))))

(define quadkey-right-neighbor
  (let ()
    (define (f qk)
      (let ((has-parent (quadkey-has-parent? qk)))
        (case (quadkey-position qk)
          ((top-left)
           (values #f 'top-right))
          ((top-right)
           (values has-parent 'top-left))
          ((bottom-left)
           (values #f 'bottom-right))
          ((bottom-right)
           (values has-parent 'bottom-left)))))
    (lambda (qk)
      (%quadkey-neighbor f qk))))

(define (invalid-op msg)
  (error msg))

(define quadkey-top-neighbor
  (let ()
    (define (f qk)
      (let ((has-parent (quadkey-has-parent? qk)))
        (case (quadkey-position qk)
          ((top-left)
           (unless has-parent
             (invalid-op "no neighbor"))
           (values has-parent 'bottom-left))
          ((top-right)
           (unless has-parent
             (invalid-op "no neighbor"))
           (values has-parent 'bottom-right))
          ((bottom-left)
           (values #f 'top-left))
          ((bottom-right)
           (values #f 'top-right)))))
    (lambda (qk)
      (%quadkey-neighbor f qk))))

(define quadkey-bottom-neighbor
  (let ()
    (define (f qk)
      (let ((has-parent (quadkey-has-parent? qk)))
        (case (quadkey-position qk)
          ((top-left)
           (values #f 'bottom-left))
          ((top-right)
           (values #f 'bottom-right))
          ((bottom-left)
           (unless has-parent
             (invalid-op "no neighbor"))
           (values has-parent 'top-left))
          ((bottom-right)
           (unless has-parent
             (invalid-op "no neighbor"))
           (values has-parent 'top-right)))))
    (lambda (qk)
      (%quadkey-neighbor f qk))))
