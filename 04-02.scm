;; evalにおいて、condの条件式の順番を変えて、代入の判断を処理の判断のあとに置いた場合にどうなるか？

;; a.
;; 代入の式が、applicationだと誤解される場合がある。
;; 例えば、evalに渡されたexpが(define x 3)とすると、これはpairであることから
;; (application?　exp)
;; は真となる。すると、
;; (apply (eval (operator exp) env)
;;        (list-of-values (operands exp) env))
;; が呼び出されるが、ここで(operands exp) = (x 3)のxはまだ定義されていない変数と見なされ、エラーとなる。

;; b.
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
   ((application? exp)
    (my-apply (eval (operator exp) env)
	   (list-of-values (operands exp) env)))
   (else
    (error "Unknown expression type -- EVAL" exp))))

(define (application? exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))
