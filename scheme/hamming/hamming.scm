(import (rnrs))

(define (hamming-distance strand-a strand-b)
  (unless (= (string-length strand-a)
             (string-length strand-b))
    (assertion-violation "hamming-distance" "strands must be same length"))
  (let loop ([strand-a (string->list strand-a)]
             [strand-b (string->list strand-b)]
             [dist 0])
    (if (null? strand-a)
        dist
        (let ([inc (if (equal? (car strand-a) (car strand-b)) 0 1)])
          (loop (cdr strand-a) (cdr strand-b) (+ dist inc))))))

;; hamming-distance-map was not submitted to exercism
(define (hamming-distance-map strand-a strand-b)
  (unless (= (string-length strand-a)
             (string-length strand-b))
    (assertion-violation "hamming-distance" "strands must be same length"))
  (apply + (map (lambda (a b) (if (equal? a b) 0 1))
                (string->list strand-a)
                (string->list strand-b))))

                            

