(load "queue.scm")
;;;;;;;;; half adder ;;;;;;;;;
(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
	(c1 (make-wire))
	(c2 (make-wire))
	)
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok)
  )

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
		   (lambda () (set-signal! output new-value))
		   )
      ) ; end let
    ) ; end define
  (add-action! input invert-input)
  'ok) ; end define

(define (logical-not s)
  (cond ((= s 0) 1)
	((= s 1) 0)
	(else (error "Invalid signal" s))
	)
  )

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
		   (lambda ()
		     (set-signal! output new-value)
		     ) ; end lambda
		   ) ; end after-delay
      ) ; end let
    ) ; end define
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  ) ; end define

(define (logical-and a1 a2)
  (cond ((= a1 a2) 1)
	(else 0)
	)
  )

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
		   (lambda ()
		     (set-signal! output new-value)
		     ) ; end lambda
		   ) ; end after delay
      ) ; end let
    ) ; end define
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  ) ; end define

(define (logical-or a1 a2)
  (cond ((= a1 a2) 0)
	(else 1)
	)
  )

(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
	  (begin (set! signal-value new-value)
		 (call-each action-procedures))
       ) ; end if
      'done) ; end define

    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc)
     ) ; end define

    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
	    ((eq? m 'set-signal!) set-my-signal!)
	    ((eq? m 'add-action!) accept-action-procedure!)
	    (else (error "Unkown operation -- WIRE" m))
	    ) ; end cond
      ) ; end define
    dispatch
    ) ; end let
  ) ; end define

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
	((car procedures))
	(call-each (cdr procedures))
	) ; end begin
   ) ; end if
  ) ; end define

(define (get-signal wire)
  (wire 'get-signal)
  ) ; end define

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value)
  ) ; end define

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure)
  ) ; end define

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
		  action
		  the-agenda) ; end add-to-agenda
  ) ; end define

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
	(first-item)
	(remove-first-agenda-item! the-agenda)
	(propagate)
       ) ; end let
   ) ; end if
 ) ; end define

(define (probe name wire)
  (add-action! wire
	       (lambda ()
		 ; (newline)
		 (display name)
		 (display " ")
		 (display (current-time the-agenda))
		 (display "  New-value = ")
		 (display (get-signal wire))
		 (newline)
		 ) ; end lambda
	       ) ; end add-action!
 ) ; end define

(define (make-time-segment time queue)
  (cons time queue)
  ) ; end define

(define (segment-time s) (car s))

(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))

(define (current-time agenda) (car agenda))

(define (set-current-time! agenda time)
  (set-car! agenda time)
  ) ; end define

(define (segments agenda)
  (cdr agenda)
  ) ; end define

(define (set-segments! agenda segments)
  (set-cdr! agenda segments)
  ) ; end define

(define (first-segment agenda)
  (car (segments agenda))
  ) ; end define

(define (rest-segments agenda)
  (cdr (segments agenda))
  ) ; end define

(define (empty-agenda? agenda)
  (null? (segments agenda))
  ) ; end define

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
	(< time (segment-time (car segments)))
	) ; end or
    ) ; end define
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)
      ) ; end let
    ) ; end define
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
	(insert-queue! (segment-queue (car segments)) action)
	(let ((rest (cdr segments)))
	  (if (belongs-before? rest)
	      (set-cdr!
	       segments
	       (cons (make-new-time-segment time action)
		     (cdr segments)
		     ) ; end cons
	       ) ; end set-cdr!
	      (add-to-segments! rest)
	      ) ; end if
	  ) ; end let
     ) ; end if
    ) ; end define
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
	(set-segments!
	 agenda
	 (cons (make-new-time-segment time action)
	       segments)
	 ) ; end cons
	(add-to-segments! segments)
	) ; end if
    ) ; end let
  ) ; end define

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
	(set-segments! agenda (rest-segments agenda))
	) ; end if
   ) ; end let
  ) ; end define

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
	(set-current-time! agenda (segment-time first-seg))
	(front-queue (segment-queue first-seg))
	) ; end let
   ) ; end if
  ) ; end define

(define the-agenda (make-agenda))

(define inverter-delay 2)

(define and-gate-delay 3)

(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))
(probe 'input-1 input-1)
(probe 'input-2 input-2)
(probe 'sum sum)
(probe 'carry carry)
(half-adder input-1 input-2 sum carry)
(set-signal! input-1 1)
(propagate)
(set-signal! input-2 1)
(propagate)
