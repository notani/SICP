
(define (filtered-accumulate combiner null-value term a next b filter)
  (define (iter i result)
    (if (> i b)
        result
        (iter (next i)
              (if (filter i)                   ;; filterに合致すればresultに加える。そうでないとき加えない。
                  (combiner result (term i))
                  result))))
    (iter a null-value))


;; part a
(define (part-a a b)
  (define (square x) (* x x))
  (define (prime? n)
    (define (smallest-divisor n)
      (define (find-divisor n test-divisor)
        (define (divides? a b)
          (= (remainder b a) 0))
        (cond ((> (square test-divisor) n) n)
              ((divides? test-divisor n) test-divisor)
              (else (find-divisor n (+ test-divisor 1)))))
      (find-divisor n 2))
    (if (< n 2)
        #f
        (= (smallest-divisor n) n)))
  (filtered-accumulate + 0 square a (lambda (x) (+ x 1)) b prime?))

;; part b
(define (part-b n)
  (filtered-accumulate *
                       1
                       (lambda (x) x)
                       1
                       (lambda (x) (+ x 1))
                       n
                       (lambda (x) (= (gcd x n) 1))))
