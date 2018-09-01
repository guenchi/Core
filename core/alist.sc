(library (core alist)
    (export
        ref
        val
        alter
        drop
        alter!
        drop!
        push!
        pop!
        insert!
        eject!
        alist->vector
        vector->alist)
    (import
        (scheme))


    (define ref
        (lambda (str x)
            (if (null? str)
                '()
                (if (equal? (caar str) x)
                    (cdar str)
                    (ref (cdr str) x)))))


    (define val
        (lambda (str x)
            (if (null? str)
                '()
                (if (equal? (cdar str) x)
                    (caar str)
                    (val (cdr str) x)))))
 
    (define alter
        (lambda (lst x y)
            (if (null? (car lst))
                lst
                (if (equal? (caar lst) x)
                    (cons (cons x y) (cdr lst))
                    (if (null? (cdr lst))
                        lst
                        (cons (car lst) (alter (cdr lst) x y)))))))             


    (define drop
        (lambda (lst x)
            (if (null? (car lst))
                lst
                (if (equal? (caar lst) x)
                    (cdr lst)
                    (if (null? (cdr lst))
                        lst
                        (cons (car lst) (drop (cdr lst) x))))))) 
 
 
    (define pop
        (lambda (lst)
            (if (null? (car lst))
                (lambda (f)
                    (f '() lst))
                (let l ((lst lst)(k '())(b #t))
                    (if (null? (cdr lst))
                        (lambda (f)
                            (f (car lst) (if b '(()) (reverse k))))
                        (l (cdr lst) (cons (car lst) k) #f))))))
 
    (define insert
        (lambda (lst x y)
            (if (null? (car lst))
                (list (cons x y))
                (cons (cons x y) lst))))
 
 
    (define eject
        (lambda (lst)
                (lambda (f)
                    (if (null? (cdr lst))
                        (f (car lst) '(()))
                        (f (car lst)(cdr lst)))))) 
 

    
    (define alter!
        (lambda (lst x y)
            (if (null? (car lst))
                #f
                (if (equal? (caar lst) x)
                    (begin
                        (set-car! lst (cons x y))
                        #t)
                    (if (null? (cdr lst))
                        #f
                        (alter! (cdr lst) x y))))))
              


    (define drop!
        (lambda (lst x)
            (if (null? (car lst))
                #f
                (if (equal? (caar lst) x)
                    (if (null? (cdr lst))
                        (begin
                            (set-car! lst '())
                            #t)
                        (begin
                            (set-car! lst (cadr lst))
                            (set-cdr! lst (cddr lst))
                            #t))
                    (if (null? (cdr lst))
                        #f
                        (if (null? (cddr lst))
                            (if (equal? (caadr lst) x)
                                (begin 
                                    (set-cdr! lst '())
                                    #t)
                                #f)
                            (drop! (cdr lst) x)))))))



    (define push!
	    (lambda (lst x y)
		    (if (null? (cdr lst))
                (if (null? (car lst))
                    (begin
                        (set-car! lst (cons x y))
                        #t)
                    (begin
                        (set-cdr! lst (cons (cons x y) '()))
                        #t))
                (push! (cdr lst) x y))))


    (define pop!
	    (lambda (str)
		    (if (null? (cdr str))
			    (let ((str str)(x (car str))) 
					(set-car! str '())
					x)
			    (let loop ((str str)(x (cdr str)))
					    (if (null? (cdr x))
						    (begin    
							    (set-cdr! str '())
							    (car x))
                            (loop (cdr str) (cdr x)))))))



    (define insert!
        (lambda (lst x y)
            (if (not (equal? (cdr lst) '()))
                (set-cdr! lst (cons (car lst) (cdr lst))))
            (set-car! lst (cons x y))
            #t))               

                            

    (define eject!
	    (lambda (str)
		    (let ((str str)(x (car str)))
			    (if (null? (cdr str))
				    (set-car! str '())
				    (begin 
					    (set-car! str (cadr str))
					    (set-cdr! str (cddr str))))
			    x)))


    (define vector->alist
        (lambda (x)
            (let l ((x (vector->list x))(n 0))
                (cons (cons n (car x)) 
                    (if (null? (cdr x))
                        '()
                        (l (cdr x) (+ n 1)))))))


    (define alist->vector
        (lambda (x)
            (list->vector         
                (let l ((x x)(n 0))
                    (cons (cdar x)
                        (if (null? (cdr x))
                            '()
                            (l (cdr x) (+ n 1))))))))

)
