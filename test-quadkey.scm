(use srfi-11)
(use gauche.test)
(use gauche.generator)
(use data.random)
(use util.match)

(use quadkey)

(test-start "quadkey")

(test-module 'quadkey)

(define test-count 10000)

(define level-1-map
  '#(
     #("0" "1")
     #("2" "3")
     ))

(define level-2-map
  '#(
     #("00" "01" "10" "11")
     #("02" "03" "12" "13")
     #("20" "21" "30" "31")
     #("22" "23" "32" "33")
     ))

(define level-3-map
  '#(
     #("000" "001" "010" "011" "100" "101" "110" "111")
     #("002" "003" "012" "013" "102" "103" "112" "113")
     #("020" "021" "030" "031" "120" "121" "130" "131")
     #("022" "023" "032" "033" "122" "123" "132" "133")
     #("200" "201" "210" "211" "300" "301" "310" "311")
     #("202" "203" "212" "213" "302" "303" "312" "313")
     #("220" "221" "230" "231" "320" "321" "330" "331")
     #("222" "223" "232" "233" "322" "323" "332" "333")
     ))

(define (test-neighbors qmap)
  (define (test-neighbor msg orig expected f)
    (test* msg
           expected
           (quadkey->string (f (string->quadkey orig)))))
  (let ((size (vector-length qmap)))
    (dotimes (i size)
      (dotimes (j size)
        (let ((orig (~ qmap j i)))
          (test-neighbor (format "quadkey-left-neighbor of ~A" orig)
                         orig
                         (~ qmap j (modulo (- i 1) size))
                         quadkey-left-neighbor)
          (test-neighbor (format "quadkey-right-neighbor of ~A" orig)
                         orig
                         (~ qmap j (modulo (+ i 1) size))
                         quadkey-right-neighbor)
          (test-neighbor (format "quadkey-top-neighbor of ~A" orig)
                         orig
                         (if (= j 0)
                             (test-error)
                             (~ qmap (modulo (- j 1) size) i))
                         quadkey-top-neighbor)
          (test-neighbor (format "quadkey-bottom-neighbor of ~A" orig)
                         orig
                         (if (= j (- size 1))
                             (test-error)
                             (~ qmap (modulo (+ j 1) size) i))
                         quadkey-bottom-neighbor)
          )))))

(test-neighbors level-1-map)
(test-neighbors level-2-map)
(test-neighbors level-3-map)

#;
(let ((n test-count))
  (do-generator (level&lat&lng
                 (gtake (tuples-of
                         (integers-between$ min-quadkey-level
                                            max-quadkey-level)
                         (reals-between$ min-latitude max-latitude)
                         (reals-between$ min-longitude max-longitude))
                        n))
    (match-let* (((level lat lng) level&lat&lng)
                 (qk (make-quadkey lat lng level)))
      (test* #"quadkey->string length (~level)"
             (quadkey-level qk)
             (string-length (quadkey->string qk))
             ))))

(let* ((s "01")
       (qk (string->quadkey s)))
  (test* "quadkey->string" s (quadkey->string qk))
  (test* "quadkey->string length"
         (quadkey-level qk)
         (string-length (quadkey->string qk))
         ))

#;
(let ((n test-count))
  (do-generator (level&lat&lng
                 (gtake (tuples-of
                         (integers-between$ min-quadkey-level
                                            max-quadkey-level)
                         (reals-between$ min-latitude max-latitude)
                         (reals-between$ min-longitude max-longitude))
                        n))
    (match-let* (((level lat lng) level&lat&lng)
                 (qk1 (make-quadkey lat lng level))
                 ((and lat&lng (lat lng))
                  (values->list (quadkey->latitude&longitude qk1)))
                 (qk2 (make-quadkey lat lng level))
                 (lat&lng~ (values->list (quadkey->latitude&longitude qk2))))
      (test* #"quadkey-fixpoint (~level)" lat&lng lat&lng~))))

(test-end)
