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


;; Newton's method
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))
(define dx 0.00001)


(define (cube x) (* x x x))

;: ((deriv cube) 5)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))


(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))


(define (cubic a b c)
  (define (cube x)
    (* x x x))
  (define (square x)
    (* x x))
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

;; EXERCISE 1.40
;: (newtons-method (cubic a b c) 1)
;; gosh> (define r (newtons-method (cubic 2 3 4) 1))
;; r
;; gosh> r
;; -1.6506291914330982
;; (+ (cube r) (* 2 (square r)) (* 3 r) 4)
;; 2.8752999980952154e-11
