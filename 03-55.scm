;; (define (partial-sums stream)
;;   (if (stream-null? stream)
;;       the-empty-stream
;;       (cons-stream (stream-car stream)
;;                    (add-streams (partial-sums stream)
;;                                 (stream-cdr stream)))))

;; iterで時間計算量の削減
(define (partial-sums stream)
  (define (iter s pre-sums)
    (cons-stream (+ (stream-car s) pre-sums)
                 (iter (stream-cdr s)
                       (+ (stream-car s) pre-sums))))
  (iter stream the-empty-stream))
