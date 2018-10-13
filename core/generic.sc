(library (core generic)
         (export set-generic! map-generic first second third fourth)
         (import (chezscheme))

         (define-syntax set-generic!
           (syntax-rules (car cdr vector-ref list-ref first second third fourth)
             [(_ (car var) value)
              (set-car! var value)]
             [(_ (cdr var) value)
              (set-cdr! var value)]
             [(_ (vector-ref var index) value)
              (vector-set! var index value)]
             [(_ (list-ref var index) value)
              (list-set! var index value)]
             [(_ (first var) value)
              (cond
                [(list? var) (set-car! var value)]
                [(vector? var) (vector-set! var 0 value)]
                [else (error 'set-generic! "unknown datatype")])]
             [(_ (second var) value)
              (cond
                [(list? var) (list-set! var 1 value)]
                [(vector? var) (vector-set! var 1 value)]
                [else (error 'set-generic! "unknown datatype")])]
             [(_ (third var) value)
              (cond
                [(list? var) (list-set! var 2 value)]
                [(vector? var) (vector-set! var 2 value)]
                [else (error 'set-generic! "unknown datatype")])]
             [(_ (fourth var) value)
              (cond
                [(list? var) (list-set! var 3 value)]
                [(vector? var) (vector-set! var 3 value)]
                [else (error 'set-generic! "unknown datatype")])
              ]
             [(_ . any-other-forms) (set! . any-other-forms)]
               ))

         (define (map-generic f list-or-vector)
           (cond
             [(list? list-or-vector) (map f list-or-vector)]
             [(vector? list-or-vector) (vector-map f list-or-vector)]
             [else (error 'map-generic "unknown datatype")]))

         (define (list-set! var index value)
           (cond [(< index 0) (error 'set-generic "invalid index!")]
                 [(= index 0) (set-car! var value)]
                 [else (list-set! (cdr var) (- index 1) value)]))
         (define (first x)
          (cond
            [(list? x) (car x)]
            [(vector? x) (vector-ref x 0)]
            [else (error 'first "unknown datatype")]))
         (define (second x)
           (cond
             [(list? x) (cadr x)]
             [(vector? x) (vector-ref x 1)]
             [else (error 'second "unknown datatype")]))
         (define (third x)
           (cond
             [(list? x) (caddr x)]
             [(vector? x) (vector-ref x 2)]
             [else (error 'third "unknown datatype")]))
         (define (fourth x)
           (cond
             [(list? x) (cadddr x)]
             [(vector? x) (vector-ref x 3)]
             [else (error 'fourth "unknown datatype")]))

         
           )
