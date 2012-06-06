(define (list-of-values-l2r exps env)
  (if (no-operands? exps)
      '()
      (let ((first-exp (eval (first-operand exps) env)))
        (cons first-exp
              (list-of-values-l2r (rest-operands exps) env)))))

(define (list-of-values-r2l exps env)
  (if (no-operands? exps)
      '()
      (let ((first-exp (list-of-values-r2l (rest-operands exps) env)))
        (cons (eval (first-operand exps) env)
              first-exp))))
