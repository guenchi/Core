(library (core exception)
    (export
        try catch)
    (import
        (chezscheme))

(define-syntax catch (syntax-rules ()))

(define-syntax try 
    (syntax-rules (catch) 
        ((_ body (catch catcher)) 
            (call/cc 
                (lambda (exit) 
                    (with-exception-handler 
                        (lambda (condition) 
                            (catcher condition) 
                            (exit condition)) 
                        (lambda () body)))))))
)
