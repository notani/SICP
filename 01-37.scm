(define (cont-frac n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1)
              (/ (n i) (+ (d i) result)))))
  (iter (- k 1) (/ (n k) (d k))))


(define (cont-frac-rec n d k)
  (define (rec i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i)
           (+ (d i) (rec (+ i 1))))))
  (rec 1))

(define (find-k k val)
  (define (close-enough? x y)
    (< (abs (- x y)) 0.0001))
  (let ((result (cont-frac (lambda (i) 1.0)
                           (lambda (i) 1.0)
                           k)))
    (if (close-enough? result val)
        k
        (find-k (+ k 1) val))))

;; kの下限は10
