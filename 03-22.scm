(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))

    (define (empty-queue?) (null? front-ptr))

    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (begin (set-front-ptr! new-pair)
                      (set-rear-ptr! new-pair)
                      front-ptr))
              (else
               (begin (set-cdr! rear-ptr new-pair)
                      (set-rear-ptr! new-pair)
                      front-ptr)))))

    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called with an empty queue" #f))
            (else
             (begin (set-front-ptr! (cdr front-ptr))
                    front-ptr))))

    (define (dispatch op)
      (cond ((eq? op 'front-ptr) front-ptr)
            ((eq? op 'rear-ptr) rear-ptr)
            ((eq? op 'set-front-ptr!) set-front-ptr!)
            ((eq? op 'set-rear-ptr!) set-rear-ptr!)
            ((eq? op 'empty-queue?) (empty-queue?))
            ((eq? op 'insert-queue!) insert-queue!)
            ((eq? op 'delete-queue!) (delete-queue!))
            (else
             (error "Unknown request -- MAKE-QUEUE" op))))
    dispatch))
