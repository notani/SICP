(define (expmod base exp m)
  (define (square n)
    (* n n))
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermet-test n a)
  (define (try-it x)
    (= (expmod x n n) x))
  (try-it a))

(define (fast-prime? n)
  (define (iter times)
    (cond ((= times 0) #t)
          ((fermet-test n times) (iter (- times 1)))
          (else #f)))
  (iter (- n 1)))


;; すべて合成数であるが、素数と判定される
;; > (fast-prime? 561)
;; #t
;; > (fast-prime? 1105)
;; #t
;; > (fast-prime? 1729)
;; #t
;; > (fast-prime? 2465)
;; #t
;; > (fast-prime? 2821)
;; #t
;; > (fast-prime? 6601)
;; #t
