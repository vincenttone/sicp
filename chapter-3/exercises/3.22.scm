(load "../lib/helper.scm")

(define (make-queue)
  (let ((front-ptr '())
		(rear-ptr '())
		)

	(define (empty-queue?) (null? front-ptr))

	(define (set-front-ptr! item) (set! front-ptr item))
	(define (set-rear-ptr! item) (set! rear-ptr item))

	(define (front-queue)
	  (if (empty-queue?)
		  (error "FRONT called with an empty queue" front-ptr)
		  (car front-ptr)))

	(define (insert-queue! item)
	  (let ((new-pair (cons item '())))
		(cond ((empty-queue?)
			   (set-front-ptr! new-pair)
			   (set-rear-ptr! new-pair)
			   front-ptr)
			  (else
			   (set-cdr! rear-ptr new-pair)
			   (set-rear-ptr! new-pair)
			   front-ptr))))

	(define (delete-queue!)
	  (cond ((empty-queue?)
			 (error "DELETE! called with an empty queue" front-ptr))
			(else
			 (set-front-ptr! (cdr front-ptr))
			 front-ptr)))

	(define (dispatch m)
	  (cond ((eq? m 'insert-queue!) insert-queue!)
			((eq? m 'delete-queue!) (delete-queue!))
			((eq? m 'front-queue) (front-queue))
			((eq? m 'rear-ptr) rear-ptr)
			((eq? m 'queue) front-ptr)
			(else front-ptr)
			) ; cond
	  ) ; def
	dispatch) ; let
  ) ; def

(define q (make-queue))
((q 'insert-queue!) 'a)
(p (q 'queue))
(p (q 'rear-ptr))
((q 'insert-queue!) 'b)
(p (q 'queue))
(p (q 'rear-ptr))
((q 'insert-queue!) 'c)
(p (q 'queue))
(p (q 'rear-ptr))
(p (q 'front-queue))
(p "delete queue")
(q 'delete-queue!)
(p (q 'queue))
(p (q 'rear-ptr))
(p (q 'front-queue))
(q 'delete-queue!)
(p (q 'queue))
(p (q 'rear-ptr))
(p (q 'front-queue))
