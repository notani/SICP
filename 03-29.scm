(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (loginal-not (logical-or (logical-not (get-signal a1))
                                    (logical-not (get-signal a2))))))
      (after-delay (+ (* 2 inverter-delay) and-delay)
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

;; delay-timeは(+ (* inverter-delay 2) and-delay)
