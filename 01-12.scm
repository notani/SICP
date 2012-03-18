(define (pascal n)
  (define (edge row)
    (/ (* row (+ row 1)) 2))
  (define (row n)
    (define (iter row)
      (if (and (> n (edge (- row 1)))
               (or (= n (edge row))
                   (< n (edge row))))
          row
          (iter (+ row 1))))
    (iter 1))
  (let ((r (row n)))
    (if (or (= n (edge r))
            (= n (+ (edge (- r 1)) 1)))
        1
        (+ (pascal (- n r))
           (pascal (+ (- n r) 1))))))

