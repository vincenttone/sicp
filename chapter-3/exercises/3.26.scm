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

(define (make-table) 
  (let ((table '()))
	(define (insert! key value)
	  (set! table (adjoin-set (list key value) table))
	  table)

	(define (lookup given-key)
	  (let ((r (element-of-set given-key table)))
		(if r
			(cadr r)
			r
			)
		)
	  )
	(define (dispatch m)
	  (cond ((equal? m 'insert!) insert!)
			((equal? m 'lookup) lookup)
			(else table)
			)
	  )
	dispatch)
  )
(define t (make-table))
((t 'insert!) 5 'a)
((t 'insert!) 8 'b)
((t 'insert!) 3 'c)
((t 'insert!) 6 'd)
((t 'insert!) 7 'e)

(p (t 'table))
(p ((t 'lookup) 7))
(p ((t 'lookup) 5))
(p ((t 'lookup) 8))
(p ((t 'lookup) 9))
