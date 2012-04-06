(define (random n)
  (remainder (sys-random) n))

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

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a n (- n 1)) 1))
  (try-it (+ 1 (random n))))

(define (fast-prime? n)
  (define (iter times)
    (cond ((= times 0) #t)
          ((fermet-test n times) (iter (- times 1)))
          (else #f)))
  (iter (- n 1)))
