(load "../lib/helper.scm")

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right)) ; define

(define (element-of-set x set)
  (cond ((null? set) #f)
        ((= x (key (entry set))) (entry set))
        ((< x (key (entry set)))
         (element-of-set x (left-branch set)))
        ((> x (key (entry set)))
         (element-of-set x (right-branch set)))))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (key (entry set))) #t)
        ((< x (key (entry set)))
         (element-of-set? x (left-branch set)))
        ((> x (key (entry set)))
         (element-of-set? x (right-branch set)))))

(define (key list)
  (if (null? list)
	  list
	  (car list)
	  )
  )

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= (key x) (key (entry set))) set)
        ((< (key x) (key (entry set)))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> (key x) (key (entry set)))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))
		) ; cond
  ) ; def



(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))

(p (lookup 7 '((5 'a) (1 'b) (10 'c) (2 'd) (7 'e) (3 'f))))

(define (lookup-2 given-key tree)
  (element-of-set given-key tree)
  )
(define tree '())
(define tree (adjoin-set '(5 'a) tree))
(define tree (adjoin-set '(1 'b) tree))

(define tree (adjoin-set '(10 'c) tree))
(define tree (adjoin-set '(2 'd) tree))
(define tree (adjoin-set '(7 'e) tree))
(define tree (adjoin-set '(3 'f) tree))

;(p tree)
(p (lookup-2 7 tree))
