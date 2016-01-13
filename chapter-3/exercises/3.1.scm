(load "lib.scm")
(define (make-accumulator base)
  (define (incr count)
    (set! base (+ base count))
    base) ; end define
  incr) ; end define

(define A (make-accumulator 5))

(p (A 10))
(p (A 10))
