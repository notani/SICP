; 仮定
(define (double n)
  (* n 2))

(define (halve n)
  (/ n 2))


(define (fast-iter-* a b)
  (define (iter x y counter)
    (cond ((= counter 0) x)
          ((even? counter) (iter x (double y) (halve counter)))
          (else (iter (+ x y) y (- counter 1)))))
  (iter 0 a b))
