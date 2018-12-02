
(library (core configuration)
         (export define-config define-config-table)
         (import (chezscheme))

         ;;(define-config package.sc '())
         (define-syntax define-config
           (lambda (stx)
             (syntax-case stx ()
               [(k var default-value)
                (with-syntax
                    ([fname (format "~a" (syntax->datum #'var))]
                     [var-set! (datum->syntax
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
               [(k table-name [var default-value] ...)
                (with-syntax
                    ([fname (format "~a" (syntax->datum #'table-name))]
                     [(tmps ...) (generate-temporaries #'(var ...))]
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
                        (let ([quer (assq 'var readed-table)])
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
                                                                                      (not (eq? (car x) 'var)))
                                                                                    (list (cons 'var (unbox tmps))
                                                                                          ...))) p))))]))
                                                     ...))])))
         )

(import (core configuration))


;;;this library make it easier to make configurations
;;;
;;;(define-config-table main-actor.cfg [hp 100][mp 200])
;;;main-actor.cfg-hp => 100
;;;(set-main-actor.cfg-hp 99)
;;;main-actor.cfg-hp => 99