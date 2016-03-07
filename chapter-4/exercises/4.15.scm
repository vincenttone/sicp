(define (run-forever) (run-forever))

(define (try p)
  (if (halts? p p)
      (run-forever)
      'halted))

(halts? try try)

;; 1. 假设try能执行终止，引发run-forever，所以try无法执行终止
;; 2. 假设try无法执行终止，引发'hatled，所以try执行终止
;; 以上，try和自己发生了冲突，所以halts?不靠谱
