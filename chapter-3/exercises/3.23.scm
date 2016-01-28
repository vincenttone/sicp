(load "../lib/helper.scm")

(define (make-deque)
  (let ((front-ptr '())
		(rear-ptr '())
		)

	(define (empty-deque?) (null? front-ptr))

	(define (set-front-ptr! item) (set! front-ptr item))
	(define (set-rear-ptr! item) (set! rear-ptr item))

	(define (front-deque)
	  (if (empty-deque?)
		  (error "FRONT called with an empty deque" front-ptr)
		  (car front-ptr)))

	(define (rear-deque)
	  (if (empty-deque?)
		  (error "REAR called with an empty deque" front-ptr)
		  rear-ptr))

	(define (front-insert-deque! item)
	  (let ((new-pair (cons item '())))
		(cond ((empty-deque?)
			   (set-front-ptr! new-pair)
			   (set-rear-ptr! new-pair)
			   front-ptr)
			  (else
			   (set! front-ptr (cons item front-ptr))
			   front-ptr))))

	(define (rear-insert-deque! item)
	  (let ((new-pair (cons item '())))
		(cond ((empty-deque?)
			   (set-front-ptr! new-pair)
			   (set-rear-ptr! new-pair)
			   front-ptr)
			  (else
			   (set-cdr! rear-ptr new-pair)
			   (set-rear-ptr! new-pair)
			   front-ptr))))

	(define (front-delete-deque!)
	  (cond ((empty-deque?)
			 (error "DELETE! called with an empty deque" front-ptr))
			(else
			 (set-front-ptr! (cdr front-ptr))
			 front-ptr)))

	(define (rear-delete-deque!)
	  (cond ((empty-deque?)
			 (error "DELETE! called with an empty deque" front-ptr))
			(else
			 (set-car! rear-ptr '())
			 ;(set! (car rear-ptr) '())
			 front-ptr)))

	(define (dispatch m)
	  (cond ((eq? m 'front-insert-deque!) front-insert-deque!)
			((eq? m 'rear-insert-deque!) rear-insert-deque!)
			((eq? m 'front-delete-deque!) (front-delete-deque!))
			((eq? m 'rear-delete-deque!) (rear-delete-deque!))
			((eq? m 'front-deque) (front-deque))
			((eq? m 'rear-deque) (rear-deque))
			((eq? m 'rear-ptr) rear-ptr)
			((eq? m 'deque) front-ptr)
			(else front-ptr)
			) ; cond
	  ) ; def
	dispatch) ; let
  ) ; def

(define (px queue)
  (p (queue 'deque))
  (p (queue 'rear-ptr))
  )

(define q (make-deque))
((q 'front-insert-deque!) 'a)
(px q)
((q 'front-insert-deque!) 'b)
(px q)
((q 'front-insert-deque!) 'c)
(px q)
((q 'rear-insert-deque!) 'd)
(px q)
((q 'rear-insert-deque!) 'e)
(px q)

(q 'rear-delete-deque!)
(px q)

(q 'front-delete-deque!)
(px q)
