(load "lib.scm")
(define (factory base)
  (lambda (value)
    (set! base (logand value base))
    base))
(define f1 (factory 0))
(p (+ (f1 0) (f1 1)))
(define f2 (factory 1))
(p (+ (f2 1) (f2 0)))
