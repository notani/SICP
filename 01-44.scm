(load "./01-44.scm")

(define (smooth f)
  (let ((dx 0.0001))
    (lambda (x) (/ (+ (f (- x dx))
                      (f x)
                      (f (+ x dx)))
                   3))))

(define (n-fold-smooth f n)
  (repeated smooth n) f)
