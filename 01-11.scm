(define (f-r n)
  (if (< n 3)
      n
      (+ (f-r (- n 1))
         (* 2 (f-r (- n 2)))
         (* 3 (f-r (- n 3))))))

(define (f-i n)
  (define (iter a b c counter)
    (cond ((= counter n) a)
          ((< counter 2) (iter (+ 1 a) a b (+ counter 1)))
          (else (iter (+ a (* 2 b) (* 3 c)) a b (+ counter 1)))))
  (iter 0 0 0 0))
