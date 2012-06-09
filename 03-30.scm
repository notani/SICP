(define (ripple-carry-adder a b s c)
  (define (iter a-list b-list s-list c-in)
    (let ((c-out (make-wire)))
      (if (null? a-list)
          'ok
          (begin (full-adder (car a-list)
                             (car b-list)
                             c-in
                             (car s-list)
                             c-out)
                 (iter (cdr a-list)
                       (cdr b-list)
                       (cdr s-list)
                       c-out)))))
  (iter a b s c))

;; 一番目~n番目のfull-adderのdelayは、
;; h = max{and-delay + inverter-delay, or-delay} + and-delay (half-adderのdelay)
;; h' = max{and-delay} (half-adderにおいてCが出るまでのdelay)
;; とすると、h + h' + or-delay (sumは次の計算に関係がないので)
;; n番目のfull-adderのdelayは、2*h + or-delay
;; 以上から、delayは、
;; (h + h' + or-delay)(n-1) + 2*h + or-delay
;; ※並列処理が実装されていれば、の話
