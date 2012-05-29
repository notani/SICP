(define (stream-limit stream tolerance)
  (cond ((or (stream-null? stream)
             (stream-null? (stream-cdr stream)))
         (error "an empty stream" stream))
        ((< (abs (- (stream-car stream)
                    (stream-car (stream-cdr stream))))
            tolerance)
         (stream-car (stream-cdr stream)))
        (else
         (stream-limit (stream-cdr stream) tolerance))))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))
