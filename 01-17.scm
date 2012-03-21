; 仮定
(define (double n)
  (* n 2))

(define (halve n)
  (/ n 2))


(define (fast-* a b)
  (cond ((= b 0) 0)
        ((even? b) (double (fast-* a (halve b))))
        (else (+ a (fast-* a (- b 1))))))
