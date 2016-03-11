;; 无法执行
;; 第一组的let里面设置了y的值为'*unasssigned*
;; 第二组let的调用时会对a和b做apply，但此时调用了y，y却是'*unassigned*
(lambda <vars>
  (let ((u '*unassigned*)  ;; 定义y为'*unasssigned*
        (v '*unassigned*))
    (let ((a <e1>)  ;; 这里会转化为一个lambda <e1>和<e2>成为调用lambda的参数
          (b <e2>))  ;; 产生类似这样的运算：(lambda (u v) ((lambda (a b) (set! u a) (set! v b)) <e1> <e2>) <e3>)
      (set! u a)      ;; (lambda (a b) (set! u a) (set! v b)) 调用lambda里面的set!之前就需要先apply<e1>和<e2>
      (set! v b))
    <e3>))

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(define (solve f y0 dt)
  (let ((y '*unassigned*)
		(dy '*unassigned*))
	(let ((a (integral (delay dy) y0 dt))
		  (b (stream-map f y)))
	  (set! y a)
	  (set! dy b)) ; end let
	y) ; end let
  )
;; 做个转换
(define (solve f y0 dt)
  ((lambda (y dy)
	((lambda (a b) (set! y a) (set! dy b))  ;; lambda定义，有两个参数
	 (integral (delay dy) y0 dt)  ;; 此时的dy还是'*unassigned*
	 (stream-map f y) ;; 此时的y是'*unassigned*
	 )  ;; lambda调用结束
	'*unassigned*
	'*unassigned*)
   y)
  )

;; 如果使用文中方案转换出来的就是
(define (solve f y0 dt)
  ((lambda (y dy)
	 (set! y (integral (delay dy) y0 dt))
	 (set! dy (stream-map f y)))  ;; lambda调用结束
   '*unassigned*
   '*unassigned*)
  y)
;; 因为set的body是执行的时候eval的，此时dy和y已经定义了，没有问题
