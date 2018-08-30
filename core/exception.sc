(library (core exception)
    (export
        try)
    (import
        (scheme))


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
