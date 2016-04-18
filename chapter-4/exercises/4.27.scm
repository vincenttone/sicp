;;; L-Eval input:
(define count 0)

;;; L-Eval value:
ok

;;; L-Eval input:
(define (id x)
  (set! count (+ count 1))
  x)

;;; L-Eval value:
ok

;;; L-Eval input:
(define w (id (id 10)))

;;; L-Eval value:
ok

;;; L-Eval input:
count

;;; L-Eval value:
;;; 这里会delay掉(id 10)，所以计数只加1
1

;;; L-Eval input:
w

;;; L-Eval value:
10

;;; L-Eval input:
count

;;; L-Eval value:
;;; 这里thunk执行了，所以计数为2
2
