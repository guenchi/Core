(library (core list-comprehension)
         (export listc)
         (import (chezscheme))
         ;;Usage : (listc (cons x y) [x '(1 2 3)] [y '(4 5 6)] if (< (+ x 2) y))
         ;;(listc (+ x y) [x '(1 2 3)] [y '(4 5 6)])

         (define-syntax listc
           (syntax-rules (if)
             [(_ ret-expr) (list ret-expr)]
             [(_ ret-expr if condition) (if condition
                                            (list ret-expr)
                                            '())]
             [(_ ret-expr [var val] rest ...)
              (apply append (map (lambda (var) (listc ret-expr rest ...))
                                 val))])))
