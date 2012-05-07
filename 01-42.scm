(define (compose f g)
  (lambda (x) (f (g x))))

;; gosh> (define (square x) (* x x))
;; square
;; gosh> (define (inc x) (+ x 1))
;; inc
;; gosh> ((compose square inc) 6)
;; 49
