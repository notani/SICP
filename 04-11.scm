(define (make-frame-alt variables values)
  (map cons variables values))
(define (binding-variable binding) (car binding))
(define (binding-value binding) (cdr binding))
(define (set-binding-value! binding val) (set-cdr! binding val))
(define (add-binding-to-frame-alt! var val frame)
  (set-cdr! frame (cons (car frame) (cdr frame)))
  (set-car! frame (cons var val)))

(define (extend-environment-alt bindings base-env)
  (cons bindings base-env))

(define (lookup-variable-value-alt var env)
  (define (env-loop env)
    (define (scan binding rest)
      (cond ((eq? var (binding-variable binding))
             (binding-value binding))
            ((null? rest)
             (env-loop (enclosing-environment env)))
            (else (scan (car rest)
                        (cdr rest)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (car frame)
                (cdr frame)))))
  (env-loop env))

(define (set-variable-value-alt! var val env)
  (define (env-loop env)
    (define (scan binding rest)
      (cond ((eq? var (binding-variable binding))
             (set-binding-value! binding val))
            ((null? rest)
             (env-loop (enclosing-environment env)))
            (else (scan (car rest)
                        (cdr rest)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (car frame)
                (cdr frame)))))
  (env-loop env))

(define (define-variable-alt! var val env)
  (let ((frame (first-frame env)))
    (define (scan binding rest)
      (cond ((eq? var (binding-variable binding))
             (set-binding-value! binding val))
            ((null? rest)
             (add-binding-to-frame-alt! var val frame))
            (else (scan (car rest)
                        (cdr rest)))))
    (scan (car frame)
          (cdr frame))))