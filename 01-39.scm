(define (tan-cf x k)
  (cont-frac (lambda (i)
               (if (= i 1)
                   x
                   (* x x -1)))
             (lambda (i) (- (* i 2) 1))
             k))
