;; make-queueでつくられるデータオブジェクトは、二つのポインターのペアである。carパートは先頭の要素から末尾の要素までのリストを指し、cdrパートはそのリストの最後尾の要素を指している。つまり、二つのポインタは同じリストを指している。\\
;; 手続きinsert-queue!が実行されたとき、要素がリストの末尾に追加される。表示上は2回挿入されているように見えるが、全体としてはリストに1度しか挿入されていない。\\
;; また、delete-queue!を繰り返した結果、最後にqueueにあるはずのない要素bが残ってしまったのは、delete-queue!の実行時にrear-ptrが更新されないためである。
(define (make-queue) (cons '() '()))

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           (print-queue queue))
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           (print-queue queue)))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))

(define (print-queue queue)
  (front-ptr queue))
