(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-sequence-ex exps)
  (define (execute-sequence procs env)
    (cond ((null? (cdr procs)) ((car procs) env))
          (else ((car procs) env)
                (execute-sequence (cdr procs) env))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (lambda (env) (execute-sequence procs env))))


;; >((analyze-sequence '((cons 1 2))) the-global-environment)
;; (1 . 2)

;; >((analyze-sequence-ex '((cons 1 2))) the-global-environment)
;; (1 . 2)

;; >((analyze-sequence '((cons 1 2) (cons 3 4))) the-global-environment)
;; (3 . 4)

;; >((analyze-sequence-ex '((cons 1 2) (cons 3 4))) the-global-environment)
;; (3 . 4)

;; 実行結果は同じだが、analyze-sequenceがどの程度まで解析した式を返すかに違いがある。
;; 式を一つだけ持つseqence：(analyze-sequence '((cons 1 2)))を実行した場合を考える。
;; 本文のものは、(lambda (env) (<(cons 1 2)を解析したもの>))という手続きが返る。このあとのlambda式の呼び出しでは、引数envを適用するだけで、再び解析が行われることはない。
;; Alyssaのものでは、(lambda (env) (excute-sequence (<(cons 1 2)を解析したもの>) env))という手続きが返る。このlambda式がが呼ばれると、excute-sequenceが実行されてsequenceの解析が行われる。
;; 複数の式を持つsequeceを私た場合も同様で、本文のものはlambdaのbodyにはすでに解析された式が入る。一方、Alyssaのものではlambdaのbodyの中はそれぞれの式は解析されているがsequence自体はまだ解析されず、envを与えたときにはじめて解析される。
