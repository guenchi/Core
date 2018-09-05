(library (core condition)
    (export
        or=?
        or-eq?
        or-eqv?
        or-equal?
        )
    (import
        (scheme))


    (define-syntax or=?
        (syntax-rules ()
            ((_ x e1)(or (= x e1)))
            ((_ x e1 e2 ...)(or (= x e1)(= x e2) ...))))


    (define-syntax or-eq?
        (syntax-rules ()
            ((_ x e1)(or (eq? x e1)))
            ((_ x e1 e2 ...)(or (eq? x e1)(eq? x e2) ...))))

    (define-syntax or-eqv?
        (syntax-rules ()
            ((_ x e1)(or (eqv? x e1)))
            ((_ x e1 e2 ...)(or (eqv? x e1)(eqv? x e2) ...))))

    (define-syntax or-equal?
        (syntax-rules ()
            ((_ x e1)(or (equal? x e1)))
            ((_ x e1 e2 ...)(or (equal? x e1)(equal? x e2) ...))))


)
