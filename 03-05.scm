;; http://sicp.g.hatena.ne.jp/hyuki/20060505/random
(use srfi-27)
(define (random n) (* (random-real) n))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral P x1 x2 y1 y2)
  (let* ((low-x (min x1 x2))
         (low-y (min y1 y2))
         (high-x (max x1 x2))
         (high-y (max y1 y2)))
    (define (experiment)
      (P (random-in-range low-x high-x)
         (random-in-range low-y high-y)))
    (* (monte-carlo 100000 experiment)
       (* (- high-x low-x) (- high-y low-y)))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1)
                 trials-passed))))
  (iter trials 0))

(define (estimate-pi)
  (estimate-integral (lambda (x y)
                       (not (> (+ (* x x) (* y y)) 1.0)))
                     -1.0
                     1.0
                     -1.0
                     1.0))
