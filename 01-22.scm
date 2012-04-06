(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-prime)
  (display "***")
  (display elapsed-prime)
  #t)

(define (prime? n)
  (= (smallest-divisor n) n))

;; http://sicp.g.hatena.ne.jp/n-oohira/?word=*%5Bgauche%5D
(define (runtime)
  (use srfi-11)
  (let-values (((a b) (sys-gettimeofday)))
    (+ (* a 1000000) b)))

(define (search-for-primes low)
  (define (iter n counter)
    (cond ((= counter 3) #t)
          ((prime? n)
           (timed-prime-test n)
           (iter (+ n 1) (+ counter 1)))
          (else (iter (+ n 1) counter))))
  (iter low 0))


;; > (search-for-primes 1000)
;; 1009***30
;; 1013***21
;; 1019***21#t

;; > (search-for-primes 10000)
;; 10007***67
;; 10009***138
;; 10037***61#t

;; > (search-for-primes 100000)
;; 100003***230
;; 100019***186
;; 100043***184#t

;; > (search-for-primes 1000000)
;; 1000003***621
;; 1000033***572
;; 1000037***687#t

;; 実行時間もステップ数のオーダーO(sqrt(n))にだいたい従うと言える。
