;; (A)
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (if (eq? (car vals) '*unassigned*)  ;; added
                 (error 'THE VALUE IS UNASSIGNED -- LOOKUP-VARIABLE-VALUE' (car vals))
                 (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))


;; (B)
(define (scan-out-defines body)
  (define (iter exps vars vals)
    (let ((first-exp (car exps)))
      (if (definition? first-exp)
          (iter (cdr exps)
                (append vars (list (list (definition-variable first-exp)
                                         '*unassigned*)))
                (append vals (list (list 'set!
                                         (definition-variable first-exp)
                                         (definition-value first-exp)))))
          (append (list 'let
                        vars)
                  vals
                  exps))))
  (iter body '() '()))

;; (C)
(define (make-procedure parameters body env)
  (list 'procedure
        parameters
        (scan-out-defines body)
        env))
;; make-procedureはコンストラクタで、procedure-bodyはセレクタである。
;; セレクタでscan-out-definesを適用する設計にした場合、セレクタを使うたびに変換処理が行われるので効率が悪い
;; コンストラクタでscan-out-definesを適用する設計にした場合、はじめにprocedureが定義されるときにだけ変換処理が行われる。また、内部定義のletへの変換はsyntax sugarなので、処理の意味は保たれる。
