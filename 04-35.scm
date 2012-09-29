(define (an-integer-between low hi)
  (require (<= low hi))
  (amb low (an-integer-between (+ low 1) hi)))
