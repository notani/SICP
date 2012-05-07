(define (double f)
  (lambda (x) (f (f x))))

;; gosh> (define (inc x) (+ x 1))
;; inc
;; gosh> (((double (double double)) inc) 5)
;; 21

;; doubleは引数にとった手続きを2回適用する手続き
;; (double double)は引数にとった手続きを4回適用する手続き
;; (double (double double))は引数にとった手続きを16回適用する手続き
;; よって、((double (double double)) inc)は引数にとった数値に16を足す手続き
;; となる。
