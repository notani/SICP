;; part a
(define (accumulate combiner null-value term a next b)
  (define (iter i result)
    (if (> i b)
        result
        (iter (next i) (combiner result (term i)))))
  (iter a null-value))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))


;; part b
(define (accumulate-rec combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate-rec combiner
                                null-value
                                term
                                (next a)
                                next
                                b))))
