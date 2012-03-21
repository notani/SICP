;; T_pqを2回適用すると、
;; a ←　b(2pq + q^2) + a(2pq + q^2) + a(p^2 + q^2)
;; b ←　b(p^2 + q^2) + a(2pq + q^2)
;; となる。

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (define (square x)
    (* x x))
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))
                   (+ (* 2 p q) (square q))
                   (/ count 2)))
        (else
         (fib-iter (+ (* b q) (* a q) (* a p))
                   (+ (* b p) (* a q))
                   p
                   q
                   (- count 1)))))
