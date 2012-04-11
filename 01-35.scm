;; x = 1 + 1/x
;; を解くことにより、
;; 解は黄金比Φに等しいことがわかる

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


(define golden-ratio
  (fixed-point (lambda (x) (+ 1 (/ 1 x)))
               1.5))
;; 1.6180327868852458
