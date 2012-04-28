(define (make-accumulator val)
  (lambda (newvalue)
    (begin (set! val (+ val newvalue))
           val)))

