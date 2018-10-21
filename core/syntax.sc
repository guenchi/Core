(library (core syntax)
         (export define-syntax-rule ~> ~>> -> and-let*)
         (import (chezscheme))

         (define-syntax define-syntax-rule
           (syntax-rules ()
             [(_ (macro-name . patterns) form)
              (define-syntax macro-name
                (syntax-rules ()
                  [(_ . patterns) form]))]))
         ;;threading macro
         (define-syntax ~>
           (syntax-rules ()
             [(_ init) init]
             [(_ init (f args ...) r ...) (~> (f init args ...) r ...)]))

         (define-syntax ~>>
           (syntax-rules ()
             [(_ init) init]
             [(_ init (f args ...) r ...) (~>> (f args ... init) r ...)]))

         (define-syntax ->
           (syntax-rules ()
             [(_ init) init]
             [(_ init f r ...) (-> (f init) r ...)]))

         (define-syntax and-let*
           (syntax-rules ()
             [(_ () body ...)
              (let () body ...)]
             [(_ ([name value] rest ...) body ...)
              (let ([name value])
                (if name (and-let* (rest ...) body ...)
                    #f))]))
                       

         )
