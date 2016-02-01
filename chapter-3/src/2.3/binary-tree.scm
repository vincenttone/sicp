(load "../../lib/helper.scm")

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right)) ; define

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))
		) ; cond
  ) ; def

(define tree '())
(define tree (adjoin-set  5 tree))
(define tree (adjoin-set 1 tree))
(define tree (adjoin-set  10 tree))
(define tree (adjoin-set  2 tree))
(define tree (adjoin-set  7 tree))
(define tree (adjoin-set  3 tree))

(p tree)
