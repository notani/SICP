(define (random n)
  (remainder (sys-random) n))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))
      #f))

(define (report-prime elapsed-prime)
  (display "***")
  (display elapsed-prime)
  #t)

;; (define (prime? n)
;;   (= (smallest-divisor n) n))

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

(define (fermet-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermet-test n) (fast-prime? n (- times 1)))
        (else #f)))

(define (prime? n)
  (fast-prime? n 1))

;; http://sicp.g.hatena.ne.jp/n-oohira/?word=*%5Bgauche%5D
(define (runtime)
  (use srfi-11)
  (let-values (((a b) (sys-gettimeofday)))
    (+ (* a 1000000) b)))

(define (search-for-primes low num)
  (define (iter n counter)
    (cond ((= counter num) #t)
          ((prime? n)
           (if (timed-prime-test n)
               (iter (+ n 1) (+ counter 1))
               (iter (+ n 1) counter)))
          (else (iter (+ n 1) counter))))
  (iter low 0))


;; 初期状態
;; > (search-for-primes 1000 12)
;; 1009***43
;; 1013***22
;; 1019***21
;; 1021***21
;; 1031***21
;; 1033***22
;; 1039***22
;; 1049***30
;; 1051***22
;; 1061***22
;; 1063***21
;; 1069***22#t
;; ave: 24.083
;; > (search-for-primes 10000 12)
;; 10007***66
;; 10009***60
;; 10037***60
;; 10039***60
;; 10061***61
;; 10067***60
;; 10069***60
;; 10079***64
;; 10091***60
;; 10093***60
;; 10099***61
;; 10103***60#t
;; ave: 61.0
;; > (search-for-primes 100000 12)
;; 100003***189
;; 100019***184
;; 100043***183
;; 100049***346
;; 100057***185
;; 100069***190
;; 100103***187
;; 100109***188
;; 100129***187
;; 100151***186
;; 100153***186
;; 100169***189#t
;; ave: 200.0
;; > (search-for-primes 1000000 12)
;; 1000003***576
;; 1000033***2438
;; 1000037***580
;; 1000039***744
;; 1000081***575
;; 1000099***575
;; 1000117***860
;; 1000121***575
;; 1000133***576
;; 1000151***582
;; 1000159***580
;; 1000171***580#t
;; ave: 770.083


;; 改良版
;; > (search-for-primes 1000 12)
;; 1003
;; 1009***19
;; 1013***20
;; 1019***19
;; 1021***20
;; 1031***18
;; 1033***17
;; 1039***17
;; 1049***17
;; 1051***18
;; 1061***17
;; 1063***18
;; 1069***18#t
;; ave: 18.167
;; > (search-for-primes 10000 12)
;; 10007***29
p;; 10009***22
;; 10037***24
;; 10039***26
;; 10061***39
;; 10067***22
;; 10069***23
;; 10079***28
;; 10086
;; 10091***24
;; 10093***24
;; 10099***76
;; 10103***29#t
;; ave: 30.5
;; > (search-for-primes 100000 12)
;; 100003***70
;; 100019***61
;; 100043***44
;; 100049***65
;; 100057***47
;; 100069***59
;; 100103***76
;; 100109***136
;; 100129***49
;; 100151***56
;; 100153***61
;; 100169***72#t
;; ave: 66.25
;; > (search-for-primes 1000000 12)
;; 1000003***60
;; 1000033***74
;; 1000037***58
;; 1000039***59
;; 1000081***86
;; 1000099***69
;; 1000117***67
;; 1000121***65
;; 1000133***81
;; 1000151***103
;; 1000159***73
;; 1000171***79#t
;; ave: 72.833

;; O(sqrt(n))とO(log(n))の違い。
