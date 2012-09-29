(define (eval exp env)
  (cond
   ((self-evaluating? exp) exp)
   ((variable? exp) (lookup-variable-value exp env))
   ((quoted? exp) (text-of-quotation exp env))
   ((assignment? exp) (eval-assignment exp env))
   ((definition? exp) (eval-definition exp env))
   ((if? exp) (eval-if exp env))
   ((lambda? exp)
    (make-procedure (lambda-parameters exp)
		    (lambda-body exp)
		    env))
   ((begin? exp) 
    (eval-sequence (begin-actions exp) env))
   ((cond? exp) (eval (cond->if exp) env))
   ((application? exp)
    (my-apply (actual-value (operator exp) env) ; changed
	      (operands exp)
	      env))
   (else
    (error "Unknown expression type -- EVAL" exp))))


(define (text-of-quotation exp env)
  (let ((quotation-body (cadr exp)))
    (if (pair? quotation-body)
        (eval (list->pair quotation-body) env)
        quotation-body)))


(define (list->pair l)
  (if (null? l)
      '()
      (list 'cons
            (list 'quote (car l))
            (list->pair (cdr l)))))

(define cons-exp
  '(define (cons x y)
     (lambda (m) (m x y))))

(define car-exp
  '(define (car z)
     (z (lambda (p q) p))))

(define cdr-exp
  '(define (cdr z)
     (z (lambda (p q) q))))

(define test-env
  (let ((env (setup-environment)))
    (extend-environment
     (list 'cons 'car 'cdr)
     (list (eval (definition-value cons-exp) env)
             (eval (definition-value car-exp) env)
             (eval (definition-value cdr-exp) env))
     env)))
