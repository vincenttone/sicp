(load "../lib/helper.scm")

(define (make-table same-key?)
  (define (assoc key records)
    (cond ((null? records) #f)
	  ((same-key? key (caar records)) (car records))
	  (else (assoc key (cdr records)))))

  (define (rec-assoc keys table finish-call)
    (let ((subtable (assoc (car keys) (cdr table))))
      (cond (subtable
	     (cond ((and (not (null? (cdr keys))))
		    (rec-assoc (cdr keys) subtable finish-call))
		   (else (finish-call keys subtable table)))
	     ) ; cond
	    (else (finish-call keys subtable table))
	    ) ; cond
      ) ; let
    )

  (define (set-value table keys value)
    (define (get-list rec-keys)
      (if (null? (cdr rec-keys))
	  (cons (car rec-keys) value)
	  (list (car rec-keys) (get-list (cdr rec-keys)))
	  )
      )
    (set-cdr! table
	      (cons
	       (get-list keys)
	       (cdr table)) ;cons
	      ) ; set-cdr!
    )

  (let ((local-table (list '*table*)))
    (define (lookup keys)
      (rec-assoc keys
		 local-table
		 (lambda (cb-keys cb-subtable pre-table)
		   (if (pair? cb-subtable)
		       (cdr cb-subtable)
		       #f))
		 )
      ) ; def
    (define (insert! keys value)
      (rec-assoc keys
		 local-table
		 (lambda (cb-keys subtable pre-table)
		   (set-value pre-table cb-keys value)
		   ) ; lambda
		 ) ; rec-assoc
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
	    ((eq? m 'table) local-table)
            (else (error "Unknown operation -- TABLE" m)))
      )
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
(define t1-tbl (t1 'table))

(t1-put '('m 11) 100)
(t1-put '('m 22) 1231)
(t1-put '('n 33 'aaa 2222 xxxxx) 'xxx)
(t1-put '('n 44 'bbb 2222 yyyyy) 'yyyy)
(t1-put '('o 55 'ccc) 'zzzzz)

(p (t1-get '('m 11)))
(p (t1-get '('m 22)))
(p (t1-get '('n 33 'aaa 2222 xxxxx)))
(p (t1-get '('n 44 'bbb 2222 yyyyy)))
(p (t1-get '('o 55 'ccc)))
(p (t1-get '('n 33)))
(p (t1-get '('n 44 'kkk)))

