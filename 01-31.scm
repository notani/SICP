;; part a
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (if (> a b)
      0
      (iter a 1)))

(define (fact n)
  (product (lambda (x) x)
           1
           (lambda (x) (+ x 1))
           n))


;; rangeは2から2n
(define (calc-pi n)
  (define (square x) (* x x))
  (inexact (* 4 (product (lambda (x) (/ (* x (+ x 2))
                                        (square (+ x 1))))
                         2
                         (lambda (x) (+ x 2))
                         (* 2 n)))))


;; part b
(define (product-rec term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-rec term (next a) next b))))
