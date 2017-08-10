(define-module quadkey
  (export make-quadkey quadkey? quadkey-value quadkey-level
          quadkey-level? min-quadkey-level max-quadkey-level
          valid-latitude? max-latitude min-latitude
          valid-longitude? max-longitude min-longitude
          quadkey->string quadkey->latitude&longitude
          merge-bits unmerge-bits
          string->quadkey)
  (use srfi-11)
  (use srfi-13)
  (use srfi-60)
  (use srfi-141)
  (use srfi-145)
  (use gauche.record)
  (use math.const))

(select-module quadkey)

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

(define (tile-size level)
  (assume (quadkey-level? level))
  (expt 2 level))

(define (degrees->radians deg)
  (/ (* deg pi) 180))

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
  (let ((t (tile-size level)))
    (define (normalize v)
      (clamp (exact (floor (+ (* t v) 0.5)))
             0
             (- t 1)))
    (let* ((tx (normalize (/ (+ lng 180) 360)))
           (sin-lat (sin (degrees->radians lat)))
           (ty (normalize (- 0.5 (/ (log (/ (+ 1 sin-lat)
                                            (- 1 sin-lat)))
                                    4 pi))))
           (v (merge-bits tx ty)))
      (%make-quadkey v level))))

(define (quadkey->latitude&longitude qk)
  (let ((t (tile-size (quadkey-level qk))))
    (define (normalize v)
      (/ (clamp v 0 (- t 1)) t))
    (let*-values (((tx ty) (unmerge-bits (quadkey-value qk)))
                  ((x) (- (normalize tx) 0.5))
                  ((y) (- 0.5 (normalize ty))))
      (values (- 90 (* 360
                       (/ (atan (exp (* y -2 pi)))
                          pi)))
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
