(define (construct-f val)
  (lambda (x)
    (if (= val 1)
        (begin (set! val 1)
               val)
        x)))

(define f (construct-f 0))
