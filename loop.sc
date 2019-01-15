;  MIT License

;  Copyright guenchi, syntacticlosure (c) 2018 
         
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


(library (core loop)
         (export
          in
          in-list
          in-vector
          in-alist
          in-string
          in-naturals
          in-fxvector
          in-stream
          in-indexed
          in-lined
          in-delimited
          in-directory
          range
          indexed
          for
          for/sum
          for/list
          for/vector
          for/product
          for/max
          for/min
          for/and
          for/or
          for/string
          for/break
          for*
          for*/list
          for*/vector
          for*/sum
          for*/product
          for*/max
          for*/min
          for*/and
          for*/or
          listc)
         (import 
          (chezscheme) (core data) (core syntax) (core string))

         (define-syntax in-list (syntax-rules ()))
         (define-syntax in-vector (syntax-rules ()))
         (define-syntax in-alist (syntax-rules ()))
         (define-syntax in-string (syntax-rules ()))
         (define-syntax in-fxvector (syntax-rules ()))
         (define-syntax in-naturals (syntax-rules ()))
         (define-syntax in-stream (syntax-rules ()))
         (define-syntax in-indexed (syntax-rules ()))
         (define-syntax in (syntax-rules ()))
         (define-syntax in-lined (syntax-rules ()))
         (define-syntax in-delimited (syntax-rules ()))
         (define-syntax in-directory (syntax-rules ()))
     

         (define-syntax listc
           (syntax-rules (if)
             [(_ ret-expr) (list ret-expr)]
             [(_ ret-expr if condition rest ...)
              (if condition
                  (listc ret-expr rest ...)
                  '())]
             [(_ ret-expr [var val] rest ...)
              (apply append (map (lambda (var) (listc ret-expr rest ...))
                                 val))]))
         
         (meta define must-return-list
               (list #'range #'append #'map #'filter #'range
                     #'string->list #'fold-right #'list))
         (meta define must-return-string
               (list #'string-append #'list->string #'stream-take))
         (meta define must-return-stream
               (list #'stream-map #'stream* #'stream))

         (meta define in-symbols
               (syntax->list
                #'(in-vector in-alist in-string in range
                             map string-append append filter in-fxvector
                             in-naturals in-stream
                             in-indexed in-lined in-delimited in-directory)))
         (define-syntax transform-if
           (syntax-rules (if)
             [(_ if condition statements ...)
              (if condition
                  (let ()
                    statements ...)
                  (void))]
             [(_ statements ...)
              (begin statements ...)]))
         
         (define-syntax for
           (lambda (stx)
             (syntax-case stx (in-list in-vector in-alist in-string in range
                                       map string-append append filter in-fxvector
                                       in-naturals in-stream
                                       in-indexed in-lined in-delimited in-directory
                                       )
               ;;;general optimizations
               ((_ var in (head args ...) block ...)
                (member/free-identifier=? #'head must-return-list)
                #'(for var in-list (head args ...) block ...))
               ((_ var in (head args ...) block ...)
                (member/free-identifier=? #'head must-return-stream)
                #'(for var in-stream (head args ...) block ...))
               ((_ var in (head args ...) block ...)
                (member/free-identifier=? #'head must-return-string)
                #'(for var in-string (head args ...) block ...))
               
               ;;simple optimizations for in-list
               ((_ var in-list (append) block ...)
                #'(void))
               ((_ var in-list (append l) block ...)
                #'(for var in-list l block ...))
               ((_ var in-list (append val r ...) block ...)
                #'(begin
                    (for var in-list val block ...)
                    (for var in-list (append r ...) block ...)))
             
               ((_ var in-list (map f val) block ...)
                #'(let loop ((lst val))
                    (if (null? lst)
                        (void)
                        (let ((var (f (car lst))))
                          (transform-if block ...)
                          (loop (cdr lst))))))
               

               ((_ var in-list (range a) block ...)
                #'(for var in-list (range 0 a) block ...))
               ((_ var in-list (range a b) block ...)
                #'(let loop ((num a))
                    (if (>= num b)
                        (void)
                        (let ([var num])
                          (transform-if block ...)
                          (loop (+ num 1))))))
             
               ;;;
               ((_ var in-directory path block ...)
                #'(for var in-list (directory-list path)
                    block ...))
               ((_ var (in-delimited sep) val block ...)
                #'(for var in-list (string-split val sep)
                    block ...))
               ((_ var in-lined val block ...)
                #'(for var in-list (map remove-r (split val #\newline))
                    block ...))
               
               ((_ (a b) in-indexed val block ...)
                #'(let ([a -1])
                    (for b in val
                      (set! a (+ a 1))
                      (transform-if block ...))))
                
               ((_ var in-stream val block ...)
                #'(let loop ((stm val))
                    (if (stream-null? stm)
                        (void)
                        (let ((var (stream-car stm)))
                          (transform-if block ...)
                          (loop (stream-cdr stm))))))
               ((_ var in-fxvector val block ...)
                #'(let loop ((pos 0))
                    (if (>= pos (fxvector-length val))
                        (void)
                        (let ((var (fxvector-ref val pos)))
                          (transform-if block ...)
                          (loop (+ pos 1))))))
               ((_ var in-list val block ...)
                #'(let loop ((lst val))
                    (if (null? lst)
                        (void)
                        (let ((var (car lst)))
                          (transform-if block ...)
                          (loop (cdr lst))))))
               ((_ var in-vector val block ...)
                #'(let loop ((pos 0))
                    (if (>= pos (vector-length val))
                        (void)
                        (let ((var (vector-ref val pos)))
                          (transform-if block ...)
                          (loop (+ pos 1))))))
               ((_ (key val) in-alist alist block ...)
                #'(let loop ((pos alist))
                    (if (null? pos)
                        (void)
                        (let ((key (car (car pos)))
                              (val (cdr (car pos)))
                              )
                          (transform-if block ...)
                          (loop (cdr pos))))))
               ((_ var in-string val block ...)
                #'(let loop ((pos 0))
                    (if (>= pos (string-length val))
                        (void)
                        (let ((var (string-ref val pos)))
                          (transform-if block ...)
                          (loop (+ pos 1))))))
               ((_ var in-naturals block ...)
                #'(let loop ((pos 0))
                    (let ((var pos))
                      (transform-if block ...)
                      (loop (+ pos 1)))))

               ((_ var in val block ...)
                #'(cond [(list? val) (for var in-list val block ...)]
                        [(vector? val) (for var in-vector val block ...)]
                        [(string? val) (for var in-string val block ...)]
                        [(stream? val) (for var in-stream val block ...)]
                        [(fxvector? val) (for var in-fxvector val block ...)]
                        [(set? val) (for var in-list (set->list val) block ...)]
                        ))
               ((_ . args) (error 'for "for : invalid syntax."))
               )))

         (define-syntax for/break
           (lambda (stx)
             (syntax-case stx ()
               [(k matcher <- val do ...)
                #`(call/cc (lambda (#,(datum->syntax #'k 'break))
                             (for matcher <- val do ...)))])))
             

         (define-syntax for/sum
           (syntax-rules ()
             ((_ matcher <- val do ...)
              (let ((acc 0))
                (for matcher <- val
                  (set! acc (+ acc (let ()
                                     do ...))))
                acc))   
             ))

         (define-syntax for/product
           (syntax-rules ()
             ((_ matcher <- val do ...)
              (let ((acc 1))
                (for matcher <- val
                  (set! acc (* acc (let ()
                                     do ...))))
                acc))
             ))


         (define-syntax for/list
           (syntax-rules ()
             ((_ matcher <- val do ...)
              (let ((acc '()))
                (for matcher <- val
                  (set! acc (cons (let () do ...) acc)))
                (reverse acc)))))

         (define-syntax for/vector
           (syntax-rules ()
             ((_ matcher <- val do ...)
              (list->vector (for/list matcher <- val do ...)))))

         (define-syntax for/string
           (syntax-rules ()
             ((_ matcher <- val do ...)
              (list->string (for/list matcher <- val do ...)))))

         (define-syntax for/max
           (syntax-rules ()
             ((_ matcher <- val do ...)
              (apply max (for/list matcher <- val do ...)))))

         (define-syntax for/min
           (syntax-rules ()
             ((_ matcher <- val do ...)
              (apply min (for/list matcher <- val do ...)))))

         (define-syntax for/and
           (syntax-rules ()
             ((_ matcher <- val do ...)
              (call/cc (lambda (exit)
                         (for matcher <- val
                           (if (let () do ...)
                               (void)
                               (exit #f)))
                         #t)))))

         (define-syntax for/or
           (syntax-rules ()
             ((_ matcher <- val do ...)
              (call/cc (lambda (exit)
                         (for matcher <- val
                           (if (let () do ...)
                               (exit #t)
                               (void)
                               ))
                         #f)))))
         (define range
           (case-lambda
             ((s e) (if (>= s e) '()
                        (cons s (range (+ s 1) e))))
             ((e) (range 0 e))))

         (define (indexed seq)
           (let ([c -1])
             (for/list v in seq
               (set! c (+ c 1))
               (cons c v))))

         (meta define extract-for*
               (lambda (stx)
                 (syntax-case stx ()
                   [([var <- rest ...] rest1 ...)
                    (and (identifier? #'<-) (member/free-identifier=? #'<- in-symbols))
                    (let-values ([(decls body)
                                  (extract-for* #'(rest1 ...))])
                      (values (cons #'(var <- rest ...) decls)
                              body))
                    ]
                   [(rest1 ...)
                    (values '() #'(rest1 ...))])))
                                              

         (define-syntax for*
           (lambda (stx)
             (syntax-case stx ()
               [(_ [var0 <-0 rest0 ...] [var1 <-1 rest1 ...] rest2 ...)
                (and (identifier? #'<-0) (member/free-identifier=? #'<-0 in-symbols)
                     (identifier? #'<-1) (member/free-identifier=? #'<-1 in-symbols))
                #'(for var0 <-0 rest0 ... (for* [var1 <-1 rest1 ...] rest2 ...))]
               [(_ [var0 <-0 rest0 ...] rest1 ...)
                (and (identifier? #'<-0) (member/free-identifier=? #'<-0 in-symbols))
                #'(for var0 <-0 rest0 ... rest1 ...)]))
           )

         (define-syntax for*/list
           (lambda (stx)
             (syntax-case stx ()
               ((_ rest ...)
                (let-values ([(decls body) (extract-for* #'(rest ...))])
                  #`(let ((acc '()))
                      (for* #,@decls
                        (set! acc (cons (let () #,@body) acc)))
                      (reverse acc)))))))

         (define-syntax for*/vector
           (syntax-rules ()
             [(_ rest ...) (list->vector (for*/list rest ...))]))

         (define-syntax for*/sum
           (lambda (stx)
             (syntax-case stx ()
               ((_ rest ...)
                (let-values ([(decls body) (extract-for* #'(rest ...))])
                  #`(let ((acc 0))
                      (for* #,@decls
                        (set! acc (+ (let () #,@body) acc)))
                      acc))))))

         (define-syntax for*/product
           (lambda (stx)
             (syntax-case stx ()
               ((_ rest ...)
                (let-values ([(decls body) (extract-for* #'(rest ...))])
                  #`(let ((acc 1))
                      (for* #,@decls
                        (set! acc (* (let () #,@body) acc)))
                      acc))))))
         
         (define-syntax for*/max
           (syntax-rules ()
             [(_ rest ...) (apply max (for*/list rest ...))]))
         (define-syntax for*/min
           (syntax-rules ()
             [(_ rest ...) (apply min (for*/list rest ...))]))

         (define-syntax for*/and
           (lambda (stx)
             (syntax-case stx ()
               ((_ rest ...)
                (let-values ([(decls body) (extract-for* #'(rest ...))])
                  #`(call/cc (lambda (break)
                               (for* #,@decls
                                 (unless (let () #,@body)
                                   (break #f)))
                               #t)))))))

         (define-syntax for*/or
           (lambda (stx)
             (syntax-case stx ()
               ((_ rest ...)
                (let-values ([(decls body) (extract-for* #'(rest ...))])
                  #`(call/cc (lambda (break)
                               (for* #,@decls
                                 (when (let () #,@body)
                                   (break #t)))
                               #f)))))))
         (define (remove-r x)
           (define strlen (string-length x))
           (define c (string-ref x strlen))
           (if (char=? c #\return)
               (substring x 0 (- strlen 1))
               x))
         )

(import (core loop) (core data))