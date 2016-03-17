((lambda (n)
   ((lambda (fact)
      (fact fact n))
    (lambda (ft k)
      (if (= k 1)
          1
          (* k (ft ft (- k 1)))))))
 10)

;; a) 菲波那切数列表达式
;; 0 1 1 2 3 5 8 13 21 34 55
(display ((lambda (n)
			((lambda (fib)
			   (fib fib n))
			 (lambda (fb k)
			   (cond ((= k 0) 0)
					 ((= k 1) 1)
					 (else (+ (fb fb (- k 2)) (fb  fb (- k 1))))))))
		  10))
(newline)

;; b)
(define (f x)
  (define (even? n)
    (if (= n 0)
        #t
        (odd? (- n 1))))
  (define (odd? n)
    (if (= n 0)
        #f
        (even? (- n 1))))
  (even? x))

(display (f 100))
(newline)

(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) #t (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) #f (ev? ev? od? (- n 1))))))

(display (f 100))
(newline)
(display (f 123))
(newline)

;; ps. 此方法就是通过构造一个lambda来存放变量名
;; 给此变量赋值的方式就是通过方法调用
