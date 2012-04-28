(use srfi-27)
(define (rand-update x)
  (remainder (+ (* x 214013) 2531011) 32767))

(define rand
  (let ((x 12345))
    (lambda (op) (cond ((eq? op 'generate)
                        (set! x ((lambda (seed) (rand-update seed)) x))
                        x)
                       ((eq? op 'reset)
                        (lambda (seed) (set! x seed)))
                       (else
                        (error "Uncorrect Option -- RAND" op))))))
