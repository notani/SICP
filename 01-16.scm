(define (fast-exp-iter b n)
  (define (square x)
    (* x x))
  (define (iter x y counter)
    (cond ((= counter 0) x)
          ((even? counter)
           (iter (* x (square y)) (square y) (- (/ counter 2) 1)))
          (else
           (iter (* x y) y (- counter 1)))))
  (iter 1 b n))

; 内部の手続きで名前が衝突してしまう場合はどうするのがスマートなのだろうか…。
