(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (define (square x)
    (* x x))
  (define (next d)
    (if (> d 2)
        (+ d 2)
        (+ d 1)))
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

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

(define (search-for-primes low num)
  (define (iter n counter)
    (cond ((= counter num) #t)
          ((prime? n)
           (timed-prime-test n)
           (iter (+ n 1) (+ counter 1)))
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
;; 1009***25
;; 1013***16
;; 1019***14
;; 1021***13
;; 1031***13
;; 1033***14
;; 1039***14
;; 1049***13
;; 1051***14
;; 1061***14
;; 1063***14
;; 1069***13#t
;; ave: 14.75
;; > (search-for-primes 10000 12)
;; 10007***40
;; 10009***40
;; 10037***36
;; 10039***35
;; 10061***35
;; 10067***34
;; 10069***35
;; 10079***35
;; 10091***37
;; 10093***35
;; 10099***35
;; 10103***34#t
;; ave: 35.917
;; > (search-for-primes 100000 12)
;; 100003***106
;; 100019***102
;; 100043***147
;; 100049***103
;; 100057***102
;; 100069***101
;; 100103***127
;; 100109***102
;; 100129***138
;; 100151***101
;; 100153***101
;; 100169***102#t
;; ave: 111.0
;; > (search-for-primes 1000000 12)
;; 1000003***320
;; 1000033***313
;; 1000037***526
;; 1000039***310
;; 1000081***313
;; 1000099***312
;; 1000117***325
;; 1000121***315
;; 1000133***315
;; 1000151***316
;; 1000159***326
;; 1000171***316#t
;; ave: 333.917

;; ステップ数の半減に伴って、実行時間もほぼ半分になることがわかる。
