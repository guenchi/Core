;;;;library for Simple Parser Combinator

(library (rachez parser)
         (export satisfy/p equal/p parser digit/p alpha/p parse-string
                 eof/p many/p or/p some/p integer/p
                 )
         (import (chezscheme))

         (define (satisfy/p procedure)
           (lambda (l)
             (cond
               [(null? l) 'parse-fail]
               [(procedure (car l)) (list 'parse-success (car l)
                                          (cdr l))]
               [else 'parse-fail])))

         (define (equal/p v) (satisfy/p (lambda (x) (equal? x v))))
         (define digit/p (satisfy/p (lambda (x) (char<=? #\0 x #\9))))
         (define alpha/p (satisfy/p (lambda (x) (char-alphabetic? x))))
         
         (define (eof/p l) (if (null? l) (list 'parse-success #f '())
                               'parse-fail))
         (define (or/p p . parsers)
           (lambda (l)
             (let ([res (p l)])
               (cond [(eq? res 'parse-fail) (if (null? parsers) 'parse-fail
                                                ((apply or/p parsers) l))]
                     [else res]))))

         (define (many/p p)
           (or/p 
            (parser [a p]
                    [r (many/p p)]
                    (cons a r))
            (parser '())))

         (define (some/p p)
           (parser [a p]
                   [r (many/p p)]
                   (cons a r)))

         (define (chars->integer lst)
           (car (fold-right (lambda (x y)
                         (let ([factor (cdr y)]
                               [acc (car y)])
                           (cons (+ acc (* factor (- (char->integer x) (char->integer #\0))))
                                 (* factor 10))))
                       (cons 0 1) lst)))

         (define integer/p
           (let ([integer-without-sign/p
                  (parser [a (many/p digit/p)]
                          (chars->integer a))])
             (or/p (parser [sign (or/p (equal/p #\+) (equal/p #\-))]
                           [int integer-without-sign/p]
                           (if (equal? sign #\-) (- int)
                               int))
                   integer-without-sign/p)))
           

         (define-syntax parser
           (syntax-rules ()
             [(_  return-value)  (lambda (l)
                                   (list 'parse-success return-value l))]
             [(_ [var val] rest ...)
              (lambda (l)
                (let ([res (val l)])
                  (cond [(eq? 'parse-fail res) 'parse-fail]
                        [(eq? 'parse-success (car res))
                         (let ([var (cadr res)])
                           ((parser rest ...) (caddr res)))]))
                )]))

         (define (parse-string p str)
           (define res (p (string->list str)))
           (cond [(eq? res 'parse-fail) (error 'parser "parse failed")]
                 [(eq? (car res) 'parse-success)
                  (cadr res)]))

         

         
           
         )