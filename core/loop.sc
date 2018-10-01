(library (core loop)
         (export 
            in-list
            in-vector
            in-alist
            in-string
            in-naturals
            in-range
            for
            <-
            break-when
            for/sum
            for/list
            for/vector
            for/product
            for/max
            for/min
            for/and
            for/or
            for/string)
         (import 
            (chezscheme))

         (define-syntax in-list (syntax-rules ()))
         (define-syntax in-vector (syntax-rules ()))
         (define-syntax in-alist (syntax-rules ()))
         (define-syntax in-naturals (syntax-rules ()))
         (define-syntax <- (syntax-rules ()))
         (define-syntax in-string (syntax-rules ()))
         (define-syntax in-range (syntax-rules ()))
         (define-syntax break-when (syntax-rules ()))
         
         
         (define-syntax for
           (syntax-rules (in-list in-vector in-alist <- in-string
                                  break-when
                                  )
             [(_ var <- (in-list val) block ...)
              (let loop ((lst val))
                (if (null? lst)
                    (void)
                    (let ([var (car lst)])
                      block ...
                      (loop (cdr lst)))))]
             [(_ var <- (in-vector val) block ...)
              (let loop ((pos 0))
                (if (>= pos (vector-length val))
                    (void)
                    (let ([var (vector-ref val pos)])
                      block ...
                      (loop (+ pos 1)))))]
             [(_ (key val) <- (in-alist alist) block ...)
              (let loop ((pos alist))
                (if (null? pos)
                    (void)
                    (let ([key (car (car pos))]
                          [val (cdr (car pos))]
                          )
                      block ...
                      (loop (cdr pos)))))]
             [(_ var <- (in-string val) block ...)
              (let loop ((pos 0))
                (if (>= pos (string-length val))
                    (void)
                    (let ([var (string-ref val pos)])
                      block ...
                      (loop (+ pos 1)))))]
             [(_ var <- (in-naturals) block ...)
              (let loop ((pos 0))
                (let ([var pos])
                  block ...
                  (loop (+ pos 1))))]










             
             [(_ var <- (in-list val break-when condition) block ...)
              (let loop ((lst val))
                (if (null? lst)
                    (void)
                    (let ([var (car lst)])
                      (when (not condition) block ...
                        (loop (cdr lst))))))]
             [(_ var <- (in-vector val break-when condition) block ...)
              (let loop ((pos 0))
                (if (>= pos (vector-length val))
                    (void)
                    (let ([var (vector-ref val pos)])
                      (when (not condition)  block ...
                        (loop (+ pos 1))))))]
             [(_ (key val) <- (in-alist alist break-when condition) block ...)
              (let loop ((pos alist))
                (if (null? pos)
                    (void)
                    (let ([key (car (car pos))]
                          [val (cdr (car pos))]
                          )
                      (when (not condition)  block ...
                        (loop (cdr pos))))))]
             [(_ var <- (in-string val break-when condition) block ...)
              (let loop ((pos 0))
                (if (>= pos (string-length val))
                    (void)
                    (let ([var (string-ref val pos)])
                      (when (not condition)  block ...
                        (loop (+ pos 1))))))]
             [(_ var <- (in-naturals break-when condition) block ...)
              (let loop ((pos 0))
                (let ([var pos])
                  (when (not condition) block ...
                    (loop (+ pos 1)))))]


             
             [(_ var <- (in-range range-args ... break-when condition) block ...)
              (for var <- (in-list (range range-args ...) break-when condition) block ...)]
             [(_ var <- (in-range range-args ...) block ...)
              (for var <- (in-list (range range-args ...)) block ...)]
             [(_ . args) (error "for : invalid syntax.")]
             ))

         (define-syntax for/sum
           (syntax-rules (<-)
             [(_ matcher <- val do ...)
              (let ([acc 0])
                (for matcher <- val
                  (set! acc (+ acc (let ()
                                     do ...))))
                acc)]
             ))

         (define-syntax for/product
           (syntax-rules (<-)
             [(_ matcher <- val do ...)
              (let ([acc 1])
                (for matcher <- val
                  (set! acc (* acc (let ()
                                     do ...))))
                acc)]
             ))


         (define-syntax for/list
           (syntax-rules (<-)
             [(_ matcher <- val do ...)
              (let ([acc '()])
                (for matcher <- val
                  (set! acc (cons (let () do ...) acc)))
                (reverse acc))]))

         (define-syntax for/vector
           (syntax-rules (<-)
             [(_ matcher <- val do ...)
              (list->vector (for/list matcher <- val do ...))]))

         (define-syntax for/string
           (syntax-rules (<-)
             [(_ matcher <- val do ...)
              (list->string (for/list matcher <- val do ...))]))

         (define-syntax for/max
           (syntax-rules (<-)
             [(_ matcher <- val do ...)
              (apply max (for/list matcher <- val do ...))]))

         (define-syntax for/min
           (syntax-rules (<-)
             [(_ matcher <- val do ...)
              (apply min (for/list matcher <- val do ...))]))

         (define-syntax for/and
           (syntax-rules (<-)
             [(_ matcher <- val do ...)
              (call/cc (lambda (exit)
                         (for matcher <- val
                           (if (let () do ...)
                               (void)
                               (exit #f)))
                         #t))]))

         (define-syntax for/or
           (syntax-rules (<-)
             [(_ matcher <- val do ...)
              (call/cc (lambda (exit)
                         (for matcher <- val
                           (if (let () do ...)
                               (exit #t)
                               (void)
                               ))
                         #f))]))
         (define range
           (case-lambda
             [(s e) (if (>= s e) '()
                        (cons s (range (+ s 1) e)))]
             [(e) (range 0 e)]))
                 
         )
