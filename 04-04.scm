(define (and? exp) (tagged-list? exp 'and))
(define (and-clauses exp) (cdr exp))
(define (or? exp) (tagged-list? exp 'or))
(define (or-clauses exp) (cdr exp))

(define (eval-and exp env)
  (define (rec clauses)
    (if (null? clauses)
        'true
        (if (true? (eval (car clauses) env))
            (if (last-exp? clauses)
                (eval (car clauses) env)
                (rec (cdr clauses)))
          'false)))
  (rec (and-clauses exp)))

(define (eval-or exp env)
  (define (rec clauses)
    (if (null? clauses)
        'false
        (if (true? (eval (car clauses) env))
            (eval (car clauses) env)
            (if (last-exp? clauses)
                'false
                (rec (cdr clauses))))))
  (rec (or-clauses exp)))


;; derived expressions
(define (and->if exps)
  (expand-and-clauses (and-clauses exps)))
(define (expand-and-clauses clauses)
  (if (null? clauses)
      'true
      (let ((first (car clauses)))
        (if (last-exp? clauses)
            (make-if first
                     first
                     'false)
            (make-if first
                     (expand-and-clauses (cdr clauses))
                     'false)))))

(define (or->if exps)
  (expand-or-clauses (or-clauses exps)))
(define (expand-or-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses)))
        (if (last-exp? clauses)
            (make-if first
                     'true
                     first)
            (make-if first
                     'true
                     (expand-or-clauses (cdr clauses)))))))


;; eval
(define (eval exp env)
  (cond
   ((self-evaluating? exp) exp)
   ((variable? exp) (lookup-variable-value exp env))
   ((quoted? exp) (text-of-quotation exp))
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
   ((and? exp) (eval-and exp env))
   ((or? exp) (eval-or exp env))
   ((application? exp)
    (my-apply (eval (operator exp) env)
	   (list-of-values (operands exp) env)))
   (else
    (error "Unknown expression type -- EVAL" exp))))
