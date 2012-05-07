;; iterative improvement

(define (iterative-improve good-enough? improve)
  (lambda (x)
    (define (iter guess)
      (if (good-enough? guess)
          guess
          (iter (improve guess))))
    (iter x)))


;; sqrt

(define (sqrt x)
  (define (square x) (* x x))
  (define (average x y) (/ (+ x y) 2))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) 1.0))


;; Fixed points

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (good-enough? guess)
    (let ((new-guess (f guess)))
      (< (abs (- guess new-guess)) tolerance)))
  ((iterative-improve good-enough? f) first-guess))

;; fixed-pointはgood-enough?の判定が1.3.3のそれよりimprove1回分甘い
