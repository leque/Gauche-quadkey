(use srfi-11)
(use gauche.test)
(use gauche.generator)
(use data.random)
(use util.match)

(use quadkey)

(test-start "quadkey")

(test-module 'quadkey)

(define test-count 10000)

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
