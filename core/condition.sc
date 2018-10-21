(library (core condition)
         (export
          or/=?
          and/=?
          not/=?
          or/eq?
          and/eq?
          not/eq?
          or/eqv?
          and/eqv?
          not/eqv?
          or/equal?
          and/equal?
          not/equal?
          )
         (import
          (scheme)
          )

         (define-syntax define-syntax-rule
           (syntax-rules ()
             [(_ (macro-name . patterns) form)
              (define-syntax macro-name
                (syntax-rules ()
                  [(_ . patterns) form]))]))
         
         (define-syntax-rule (or/=? x e ...) (or (= x e) ...))
         
         (define-syntax-rule (and/=? x e ...)(and (= x e) ...))

         (define-syntax-rule (not/=? x e ...)(and (not (= x e)) ...))

         (define-syntax-rule (or/eq? x e ...)(or (eq? x e) ...))

         (define-syntax-rule (and/eq? x e ...)(and (eq? x e) ...))

         (define-syntax-rule (not/eq? x e ...)(and (not (eq? x e)) ...))

         (define-syntax-rule (or/eqv? x e ...)(or (eqv? x e) ...))

         (define-syntax-rule (and/eqv? x e ...)(and (eqv? x e) ...))

         (define-syntax-rule (not/eqv? x e ...)(and (not (eqv? x e)) ...))

         (define-syntax-rule (or/equal? x e ...)(or (equal? x e) ...))

         (define-syntax-rule (and/equal? x e ...)(and (equal? x e) ...))

         (define-syntax-rule (not/equal? x e ...)(and (not (equal? x e)) ...))
         
         )
