(load "./01-42.scm")

(define (repeated f n)
  (define (iter proc counter)
    (if (= counter n)
        proc
        (iter (compose f proc) (+ counter 1))))
  (iter f 1))
