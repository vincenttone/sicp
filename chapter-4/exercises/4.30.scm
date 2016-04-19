;; a)
; display是一个primitive-procedures，会出发force-it
; 所以结果是正确的
; ps. 虽然我没觉得有什么副作用。。。这不就是一个递归吗...
(define (for-each proc items)
	(if (null? items)
		'done
	(begin (proc (car items))
		(for-each proc (cdr items)))))

(for-each (lambda (x) (newline) (display x)) (list 57 321 88))

;; b)
; 第一个返回(1 2)
; 第二个返回1，因为返回的是x，set!会被放到一个thunk里，会被delay

;; c)
; 序列调用时会调用actual-value，会调用force-it

;; d)
;; 从正确性来讲Cy的会好些，无副作用的时候延迟执行会提交效率
