;; (define (stream-limit stream tolerance)
;;   (cond ((or (stream-null? stream)
;;              (stream-null? (stream-cdr stream)))
;;          (error "an empty stream" stream))
;;         ((< (abs (- (stream-car stream)
;;                     (stream-car (stream-cdr stream))))
;;             tolerance)
;;          (stream-car (stream-cdr stream)))
;;         (else
;;          (stream-limit (stream-cdr stream) tolerance))))

;; iter ver
(define (stream-limit stream tolerance)
  (define (iter s current)
    (if (or (stream-null? s)
            (stream-null? (stream-cdr s)))
        (error "an empty stream" s)
        (let ((next (stream-car s)))
          (if (< (abs (- current next)) tolerance)
              next
              (iter (stream-cdr s) next)))))
  (iter (stream-cdr stream)
        (stream-car stream)))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))
