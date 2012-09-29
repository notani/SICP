; Implementation1

;; Load ex2.2
(load "./02-02.scm")

;; Constructor1
(define (make-rect seg seg2)
  (cons seg1 seg2))

;; Selector1
(define (rect-width rect)
  (distance (start-segment (car rect))
            (end-segment (car rect))))
(define (rect-height rect)
  (distance (start-segment (cdr rect))
            (end-segment (cdr rect))))


; Implementation2

;; Constructor2
(define (make-rect w h)
  (cons w h))

;; Selector2
(define (rect-width rect)
  (car rect))
(define (rect-height rect)
  (cdr rect))


;; Predicator
(define (distance p1 p2)
  (define (double x)
    (* x x))
  (sqrt (+ (double (- (x-point p1)
                      (x-point p2)))
           (double (- (y-point p1)
                      (y-point p2))))))

(define (perimeter rect)
  (+ (* 2 (rect-width rect))
     (* 2 (rect-height rect))))

(define (area rect)
  (* (rect-width rect)
     (rect-height rect)))
