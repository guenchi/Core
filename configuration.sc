
(library (core configuration)
         (export define-config define-config-table)
         (import (chezscheme))

         
         (define-syntax define-config
           (lambda (stx)
             (syntax-case stx ()
               [(k var fname default-value)
                (with-syntax
                    ([var-set! (datum->syntax
                                #'k
                                (string->symbol
                                 (format "set-~a!" (syntax->datum #'var))))]
                     )
                  #'(begin
                      (define tmp (let
                                      ([exists (file-exists? fname)])
                                    (if exists
                                        (call-with-input-file
                                            fname
                                          (lambda (p)
                                            (box (read p))))
                                        (box default-value))))
                      (define-syntax var
                        (lambda (stx)
                          (syntax-case stx (set!)
                            [id (identifier? #'id) #'(unbox tmp)])))
                      (define-syntax var-set!
                        (syntax-rules ()
                          [(_ v) (begin (set-box! tmp v)
                                        (call-with-output-file
                                            fname
                                          (lambda (p)
                                            (write v p))))]))
                      ))])))

         (define-syntax define-config-table
           (lambda (stx)
             (syntax-case stx ()
               [(k table-name fname [var default-value] ...)
                (with-syntax
                    ([(tmps ...) (generate-temporaries #'(var ...))]
                     [(table-var ...) (map (lambda (id)
                                             (datum->syntax #'k
                                                            (string->symbol
                                                             (format "~a-~a"
                                                                     (syntax->datum #'table-name)
                                                                     (syntax->datum id)))))
                                           (syntax->list #'(var ...)))]
                     [(var-set! ...) (map (lambda (id)
                                            (datum->syntax #'k
                                                           (string->symbol
                                                            (format "set-~a-~a!"
                                                                    (syntax->datum #'table-name)
                                                                    (syntax->datum id)))))
                                          (syntax->list #'(var ...)))]
                     )
                  #'(begin
                      (define readed-table
                        (if (file-exists? fname)
                            (call-with-input-file fname
                              (lambda (p)
                                (read p)))
                            '()))
                      (define tmps
                        (let ([quer (assoc 'var readed-table)])
                          (if quer
                              (box (cdr quer))
                              (box default-value)))) ...
                                                     (define-syntax table-var (identifier-syntax (unbox tmps)))
                                                     ...
                                                     (define-syntax var-set!
                                                       (syntax-rules ()
                                                         [(_ v) (begin (set-box! tmps v)
                                                                       (call-with-output-file fname
                                                                         (lambda (p)
                                                                           (write (cons
                                                                                   (cons 'var (unbox tmps))
                                                                                   (filter
                                                                                    (lambda (x)
                                                                                      (not (equal? (car x) 'var)))
                                                                                    (list (cons 'var (unbox tmps))
                                                                                          ...))) p))))]))
                                                     ...))])))
         )

(import (core configuration))
