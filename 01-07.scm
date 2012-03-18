; SICPのプログラムで(sqrt 0.0016)を実行すると、0.04669259597054016となる（正しい値は0.04）。このように、値が小さくなると誤差が大きくなる。
; 一方、大きい数では誤差は小さい
; (sqrt 15241383936)を実行すると、123456.00000000044（正しい値は123456）

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (define (average x y)
    (/ (+ x y) 2))
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (define (square a)
    (* a a))
  (< (abs (- (square guess) x)) 0.001))


; 以下の誤差判定を改良した手続きでは
; (sqrt-imp 0.0016)は0.04000284155333112
; (sqrt-imp 15241383936)は123456.0
; いずれも精度が良くなった。

(define (sqrt-imp x)
  (sqrt-iter-imp 1.0 x))

(define (sqrt-iter-imp guess x)
  (let ((new-guess (improve guess x)))
    (if (good-enough?-imp new-guess guess)
        new-guess
        (sqrt-iter-imp new-guess x))))

(define (good-enough?-imp guess old-guess)
  (< (abs (- guess old-guess)) 0.001))
