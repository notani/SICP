(define (partial-sums stream)
  (if (stream-null? stream)
      the-empty-stream
      (cons-stream (stream-car stream)
                   (add-streams (partial-sums stream)
                                (stream-cdr stream)))))

