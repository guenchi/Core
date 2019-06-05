(library (core async)
  (export 
    generator
    yield
    coroutine
    coroutine-run
    coroutine-dead?
    coroutine-status
    coroutine-running?
    )
  (import 
    (chezscheme))


  (define *meta-cont* (box (lambda (v) (error 'core-async "No Top Level generator"))))
         
  
         
  (define-syntax generator
    (lambda (stx)
      (syntax-case stx ()
        ((generator expr ...)  #`(letrec ((#,(datum->syntax #'generator `*cont*)
                                              (lambda (v)
                                                (reset-cont expr ...))))
                                          (lambda ()
                                            (#,(datum->syntax #'generator `*cont*) (void))))))))



  (define-syntax yield
    (lambda (stx)
      (syntax-case stx ()
        ((yield v) #`(call/cc (lambda (k)
                                (set! #,(datum->syntax #'yield `*cont*) (lambda (va) (reset-cont (k va))))
                                ((unbox *meta-cont*) v)))))))


                                
  (define-syntax reset-cont
    (syntax-rules ()
      ((_ expr ...) (let ((preserved (unbox *meta-cont*)))
                      (call/cc (lambda (k)
                                 (dynamic-wind
                                   (lambda () (set-box! *meta-cont* k))
                                   (lambda () (let ((result (let () expr ...)))
                                      ((unbox *meta-cont*) result)))
                                   (lambda () (set-box! *meta-cont* preserved)))))))))



  (define-syntax coroutine
    (lambda (stx)
      (syntax-case stx ()
        ((coroutine (arguments ...) expr ...)
         (with-syntax ((yield (datum->syntax #'coroutine 'yield)))
           #`(letrec ((arguments #f) ...
                      (*cont*
                        (lambda args
                          (define args-tmp args)
                          (define-syntax yield
                            (lambda (stx)
                              (syntax-case stx ()
                                ((_) #`(yield (void)))
                                ((_ v) #`(call/cc (lambda (k)
                                                    (set! *cont*
                                                      (lambda args
                                                        (define args-tmp args)
                                                        (begin (set! arguments (car args-tmp))
                                                               (set! args-tmp (cdr args-tmp))) ...
                                                        (reset-cont (k))))
                                                    ((unbox *meta-cont*) v)))))))
                          (begin (set! arguments (car args-tmp))
                                 (set! args-tmp (cdr args-tmp))) ...
                          (reset-cont expr ... (set! *status* 'dead))))
                        (*status*
                         'running))
                      (lambda cmd
                        (if (eq? (car cmd) 'status) *status*
                            (apply *cont* (cdr cmd))))))))))



  (define coroutine-run 
    (lambda (c . args)
      (apply c (cons 'run args))))



  (define coroutine-dead?
    (lambda (c)
      (eq? (c 'status) 'dead)))



  (define coroutine-running? 
    (lambda (c)
      (eq? (c 'status) 'running)))



  (define coroutine-status 
    (lambda (c)
      (c 'status)))


    
)

