(import (rnrs))

(define (convert number)
  (let* ([pl-lst (map (lambda (fac str) (if (= (mod number fac) 0) str ""))
                      '(3 5 7) '("Pling" "Plang" "Plong"))]
         [pl-str (apply string-append pl-lst)])
    (if (string=? pl-str "") (number->string number) pl-str)))

