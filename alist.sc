;  MIT License

;  Copyright guenchi (c) 2018 
         
;  Permission is hereby granted, free of charge, to any person obtaining a copy
;  of this software and associated documentation files (the "Software"), to deal
;  in the Software without restriction, including without limitation the rights
;  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;  copies of the Software, and to permit persons to whom the Software is
;  furnished to do so, subject to the following conditions:
         
;  The above copyright notice and this permission notice shall be included in all
;  copies or substantial portions of the Software.
         
;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;  SOFTWARE.






(library (core alist)
    (export
        ref
        val
        alter
        drop
        push
        pop
        insert
        eject
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
        (lambda (l x)
            (if (null? l)
                #f
                (if (equal? (caar l) x)
                    (cdar l)
                    (ref (cdr l) x)))))



    (define val
        (lambda (l x)
            (if (null? l)
                #f
                (if (equal? (cdar l) x)
                    (caar l)
                    (val (cdr l) x)))))
 
    (define alter
        (lambda (l x y)
            (if (null? (car l))
                l
                (if (equal? (caar l) x)
                    (cons (cons x y) (cdr l))
                    (if (null? (cdr l))
                        l
                        (cons (car l) (alter (cdr l) x y)))))))             


    (define drop
        (lambda (l x)
            (if (null? (car l))
                l
                (if (equal? (caar l) x)
                    (cdr l)
                    (if (null? (cdr l))
                        l
                        (cons (car l) (drop (cdr l) x))))))) 
 
 
    (define push
        (lambda (l x y)
            (if (null? (car l))
                (list (cons x y))
                (if (null? (cdr l))
                    (list (car l) (cons x y))
                    (cons (car l) (push (cdr l) x y))))))
 
 
    (define pop
        (lambda (l)
            (if (null? (car l))
                (lambda (f)
                    (f '() l))
                (let loop ((l l)(k '())(b #t))
                    (if (null? (cdr l))
                        (lambda (f)
                            (f (car l) (if b '(()) (reverse k))))
                        (loop (cdr l) (cons (car l) k) #f))))))
 
    (define insert
        (lambda (l x y)
            (if (null? (car l))
                (list (cons x y))
                (cons (cons x y) l))))
 
 
    (define eject
        (lambda (l)
                (lambda (f)
                    (if (null? (cdr l))
                        (f (car l) '(()))
                        (f (car l)(cdr l)))))) 
 

    
    (define alter!
        (lambda (l x y)
            (if (null? (car l))
                #f
                (if (equal? (caar l) x)
                    (begin
                        (set-car! l (cons x y))
                        #t)
                    (if (null? (cdr l))
                        #f
                        (alter! (cdr l) x y))))))
              


    (define drop!
        (lambda (l x)
            (if (null? (car l))
                #f
                (if (equal? (caar l) x)
                    (if (null? (cdr l))
                        (begin
                            (set-car! l '())
                            #t)
                        (begin
                            (set-car! l (cadr l))
                            (set-cdr! l (cddr l))
                            #t))
                    (if (null? (cdr l))
                        #f
                        (if (null? (cddr l))
                            (if (equal? (caadr l) x)
                                (begin 
                                    (set-cdr! l '())
                                    #t)
                                #f)
                            (drop! (cdr l) x)))))))



    (define push!
	    (lambda (l x y)
		    (if (null? (cdr l))
                (if (null? (car l))
                    (begin
                        (set-car! l (cons x y))
                        #t)
                    (begin
                        (set-cdr! l (cons (cons x y) '()))
                        #t))
                (push! (cdr l) x y))))


    (define pop!
	    (lambda (l)
		    (if (null? (cdr l))
			    (let ((l l)(x (car l))) 
					(set-car! l '())
					x)
			    (let loop ((l l)(x (cdr l)))
					    (if (null? (cdr x))
						    (begin    
							    (set-cdr! l '())
							    (car x))
                            (loop (cdr l) (cdr x)))))))



    (define insert!
        (lambda (l x y)
            (if (not (equal? (cdr l) '()))
                (set-cdr! l (cons (car l) (cdr l))))
            (set-car! l (cons x y))
            #t))               

                            

    (define eject!
	    (lambda (l)
		    (let ((l l)(x (car l)))
			    (if (null? (cdr l))
				    (set-car! l '())
				    (begin 
					    (set-car! l (cadr l))
					    (set-cdr! l (cddr l))))
			    x)))

 
    (define alist->vector
        (lambda (x)
            (list->vector         
                (let l ((x x)(n 0))
                    (cons (cdar x)
                        (if (null? (cdr x))
                            '()
                            (l (cdr x) (+ n 1)))))))) 

    (define vector->alist
        (lambda (x)
            (let l ((x (vector->list x))(n 0))
                (cons (cons n (car x)) 
                    (if (null? (cdr x))
                        '()
                        (l (cdr x) (+ n 1)))))))



)
