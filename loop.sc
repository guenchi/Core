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
          range
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
          listc)
         (import 
          (chezscheme))

         (define-syntax in-list (syntax-rules ()))
         (define-syntax in-vector (syntax-rules ()))
         (define-syntax in-alist (syntax-rules ()))
         (define-syntax in-naturals (syntax-rules ()))
         (define-syntax in-string (syntax-rules ()))
         (define-syntax in (syntax-rules ()))


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

         
         (define-syntax for
           (syntax-rules (in-list in-vector in-alist in-string in range
                                  map string-append append filter)
             ;;simple optimizations for loops
             ((_ var in (append) block ...)
              (void))
             ((_ var in (append l) block ...)
              (for var in-list l block ...))
             ((_ var in (append val r ...) block ...)
              (begin
                (for var in-list val block ...)
                (for var in (append r ...) block ...)))
             
             ((_ var in (map f val) block ...)
              (let loop ((lst val))
                (if (null? lst)
                    (void)
                    (let ((var (f (car lst))))
                      block ...
                      (loop (cdr lst))))))
             ((_ var in (map args ...) block ...)
              (for var in-list (map args ...) block ...))
             ((_ var in (string-append args ...) block ...)
              (for var in-string (string-append args ...) block ...))
             ((_ var in (filter args ...) block ...)
              (for var in-list (filter args ...) block ...))
             
             ;;;simple optimizations for loops
             ((_ var in-list (range args ...) block ...)
              (for var in (range args ...) block ...))
             ((_ var in (range a) block ...)
              (for var in (range 0 a) block ...))
             ((_ var in (range a b) block ...)
              (let loop ((num a))
                (if (>= num b)
                    (void)
                    (let ([var num])
                      block ...
                      (loop (+ num 1))))))
             
             ((_ var in-list val block ...)
              (let loop ((lst val))
                (if (null? lst)
                    (void)
                    (let ((var (car lst)))
                      block ...
                      (loop (cdr lst))))))
             ((_ var in-vector val block ...)
              (let loop ((pos 0))
                (if (>= pos (vector-length val))
                    (void)
                    (let ((var (vector-ref val pos)))
                      block ...
                      (loop (+ pos 1))))))
             ((_ (key val) in-alist alist block ...)
              (let loop ((pos alist))
                (if (null? pos)
                    (void)
                    (let ((key (car (car pos)))
                          (val (cdr (car pos)))
                          )
                      block ...
                      (loop (cdr pos))))))
             ((_ var in-string val block ...)
              (let loop ((pos 0))
                (if (>= pos (string-length val))
                    (void)
                    (let ((var (string-ref val pos)))
                      block ...
                      (loop (+ pos 1))))))
             ((_ var in-naturals block ...)
              (let loop ((pos 0))
                (let ((var pos))
                  block ...
                  (loop (+ pos 1)))))

             ((_ var in val block ...)
              (cond [(list? val) (for var in-list val block ...)]
                    [(vector? val) (for var in-vector val block ...)]
                    [(string? val) (for var in-string val block ...)]))
             ((_ . args) (error "for : invalid syntax."))
             ))

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
                 
         )

(import (core loop))
