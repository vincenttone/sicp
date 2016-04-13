;; 因为是递归调用，应用序会导致程序一直尝试解开程序体中的factorial是什么
;; 因为每个部分是严格的，unless的每个参数都需要求值
;; 即使在n等于1的时候也需要算出来(* n (factorial (- n 1)))这部分，也就无限循环下去了
(define (factorial n)
  (unless (= n 1)
          (* n (factorial (- n 1)))
          1))
