(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (s-integral f a b n)
  (let ((h (/ (- b a) n)))
    (define (iter sum i)
      (cond ((= i 0) (iter (+ sum (f a)) (+ i 1)))
            ((= i n) (+ sum (f b)))
            ((odd? i) (iter (+ sum (* 4 (f (* h i)))) (+ i 1)))
            (else (iter (+ sum (* 2 (f (* h i)))) (+ i 1)))))
    (/ (* h (iter 0 0)) 3)))

(define (cube x) (* x x x))

;; 比較
;; (integral cube 1 0 0.001) => 0.249999875000001
;; (s-integral cube 1 0 100) => 1/4
;; (s-integral cube 1 0 1000) => 1/4
;; Simpson's Ruleを使った場合、goshではその誤差が無視されるほど小さくなった。
