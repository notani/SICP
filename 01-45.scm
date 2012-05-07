;; repeated

(load "./01-43.scm")


;; Fixed points

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

(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))


(define (org-sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (log_2 n)
  (/ (log n) (log 2)))

(define (nth-root n)
  (lambda (x)
    (fixed-point ((repeated average-damp (floor (log_2 n)))
                  (lambda (y) (/ x (expt y (- n 1)))))
                 1.0)))

;; aevrage dampingの回数
;; 1回だとfouth rootでconvergeしなくなる
;; 2回だとeighth rootでconvergeしなくなる
;; 3回だとsixteenth rootでconvergeしなくなる
;; log_2{n}回のaverage dampingが必要っぽい

;; gosh> (expt 3 16)
;; 43046721
;; gosh> ((nth-root 16) 43046721)
;; 3.0
;; gosh> (expt 4 32)
;; 18446744073709551616
;; gosh> ((nth-root 32) 18446744073709551616)
;; 4.0
