(define (make-rat n d)
  (if (< d 0)
      (cons (* n -1)
            (abs d))
      (cons n d)))

