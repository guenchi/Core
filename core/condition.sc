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
        (scheme))


    (define-syntax or/=?
        (syntax-rules ()
            ((_ x e ...)(or (= x e) ...))))


    (define-syntax and/=?
        (syntax-rules ()
            ((_ x e ...)(and (= x e) ...))))


    (define-syntax not/=?
        (syntax-rules ()
            ((_ x e ...)(and (not (= x e)) ...))))


    (define-syntax or/eq?
        (syntax-rules ()
            ((_ x e ...)(or (eq? x e) ...))))


    (define-syntax and/eq?
        (syntax-rules ()
            ((_ x e ...)(and (eq? x e) ...))))


    (define-syntax not/eq?
        (syntax-rules ()
            ((_ x e ...)(and (not (eq? x e)) ...))))


    (define-syntax or/eqv?
        (syntax-rules ()
            ((_ x e ...)(or (eqv? x e) ...))))


    (define-syntax and/eqv?
        (syntax-rules ()
            ((_ x e ...)(and (eqv? x e) ...))))


    (define-syntax not/eqv?
        (syntax-rules ()
            ((_ x e ...)(and (not (eqv? x e)) ...))))


    (define-syntax or/equal?
        (syntax-rules ()
            ((_ x e ...)(or (equal? x e) ...))))


    (define-syntax and/equal?
        (syntax-rules ()
            ((_ x e ...)(and (equal? x e) ...))))


    (define-syntax not/equal?
        (syntax-rules ()
            ((_ x e ...)(and (not (equal? x e)) ...))))


)
