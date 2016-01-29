(load "../lib/helper.scm")

(define (make-table same-key?)
  (define (assoc key records)
	(cond ((null? records) #f)
		  ((same-key? key (caar records)) (car records))
		  (else (assoc key (cdr records)))))

  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  #f))
            #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define (same-key? a b)
	(if (and (number? a)
			 (number? b))
		(= a b)
		(equal? a b))
	)

(define t1 (make-table same-key?))
(define t1-get (t1 'lookup-proc))
(define t1-put (t1 'insert-proc!))

(t1-put 'm 123 100)
(p (t1-get 'm 123))
(p (t1-get 'm 123.0))

(define t2 (make-table equal?))
(define t2-get (t2 'lookup-proc))
(define t2-put (t2 'insert-proc!))

(t2-put 'm 123 100)
(p (t2-get 'm 123))
(p (t2-get 'm 123.0))
