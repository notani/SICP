(define (make-account balance password)
  (let ((incorrect-counter 0))

    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))

    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)

    (define (call-the-cops)
      "通報した")

    (define (validate-password input-password)
      (eq? password input-password))

    (define (dispatch input-password op)
      (if (validate-password input-password)
          (begin (set! incorrect-counter 0)
                 (cond ((eq? op 'withdraw) withdraw)
                       ((eq? op 'deposit) deposit)
                       (else (error "Unknown request -- MAKE-ACCOUNT" op))))
          (lambda (val)
            (begin (set! incorrect-counter (+ incorrect-counter 1))
                   (if (< incorrect-counter 7)
                       "Incorrect password"
                       (call-the-cops))))))
    dispatch))
