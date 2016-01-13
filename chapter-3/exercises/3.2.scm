(load "lib.scm")

;; 监控器
(define (make-monitored f)

  (define (make-counter base)
    (define (incr)
      (set! base (+ base 1))
      base) ; end define
    (define (reset)
      (set! base 0)
      base) ; end define
    (define (current)
      base) ; end define
    (define (dispatch f)
      (cond ((eq? f 'incr) (incr))
	    ((eq? f 'reset) (reset))
	    ((eq? f 'current) (current))
	    (else (error "Unknow cmd")))
      ) ; end define
    dispatch) ; end define

  (define counter (make-counter 0))
  (define (dispatch arg)
    (cond
     ((eq? arg 'how-many-calls?) (counter 'current))
     ((eq? arg 'reset-count) (counter 'reset))
     (else
      (counter 'incr)
      (f arg)) ; end else
     ) ; end cond
    ) ; end define
  dispatch) ; end define

(define s (make-monitored sqrt))

(p (s 100))
(p (s 'how-many-calls?))

(p (s 100))
(p (s 100))
(p (s 100))
(p (s 'how-many-calls?))

(p (s 'reset-count))
; (p (s 'how-many-calls?))

(p (s 100))
(p (s 'how-many-calls?))
