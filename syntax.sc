(library (core syntax)
         (export define-syntax-rule ~> ~>> and-let*
                 assoc/free-identifier=? member/free-identifier=?)
         (import (chezscheme))

         (define-syntax define-syntax-rule
           (syntax-rules ()
             [(_ (macro-name . patterns) form)
              (define-syntax macro-name
                (syntax-rules ()
                  [(_ . patterns) form]))]))
         ;;threading macros
         (define-syntax ~>
           (syntax-rules ()
             [(_ init) init]
             [(_ init (f args ...) r ...) (~> (f init args ...) r ...)]))

         (define-syntax ~>>
           (syntax-rules ()
             [(_ init) init]
             [(_ init (f args ...) r ...) (~>> (f args ... init) r ...)]))

         (define-syntax and-let*
           (syntax-rules ()
             [(_ () body ...)
              (let () body ...)]
             [(_ ([name value] rest ...) body ...)
              (let ([name value])
                (if name (and-let* (rest ...) body ...)
                    #f))]))

         (define (assoc/free-identifier=? x lst)
           (if (null? lst)
               #f
               (if (free-identifier=? x (caar lst))
                   (car lst)
                   (assoc/free-identifier=? x (cdr lst)))))

         (define (member/free-identifier=? x lst)
           (if (null? lst)
               #f
               (or (free-identifier=? x (car lst))
                   (member/free-identifier=? x (cdr lst)))
               ))

         )
