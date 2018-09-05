;  MIT License

;  Copyright guenchi (c) 2018 
         
;  Permission is hereby granted, free of charge, to any person obtaining a copy
;  of this software and associated documentation files (the "Software"), to deal
;  in the Software without restriction, including without limitation the rights
;  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;  copies of the Software, and to permit persons to whom the Software is
;  furnished to do so, subject to the following conditions:
         
;  The above copyright notice and this permission notice shall be included in all
;  copies or substantial portions of the Software.
         
;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;  SOFTWARE.






(library (core condition)
    (export
        or=?
        and=?
        not=?
        or-eq?
        and-eq?
        not-eq?
        or-eqv?
        and-eqv?
        not-eqv?
        or-equal?
        and-equal?
        not-equal?
        )
    (import
        (scheme))


    (define-syntax or=?
        (syntax-rules ()
            ((_ x e ...)(or (= x e) ...))))


    (define-syntax and=?
        (syntax-rules ()
            ((_ x e ...)(and (= x e) ...))))


    (define-syntax not=?
        (syntax-rules ()
            ((_ x e ...)(and (not (= x e)) ...))))


    (define-syntax or-eq?
        (syntax-rules ()
            ((_ x e ...)(or (eq? x e) ...))))


    (define-syntax and-eq?
        (syntax-rules ()
            ((_ x e ...)(and (eq? x e) ...))))


    (define-syntax not-eq?
        (syntax-rules ()
            ((_ x e ...)(and (not (eq? x e)) ...))))


    (define-syntax or-eqv?
        (syntax-rules ()
            ((_ x e ...)(or (eqv? x e) ...))))


    (define-syntax and-eqv?
        (syntax-rules ()
            ((_ x e ...)(and (eqv? x e) ...))))


    (define-syntax not-eqv?
        (syntax-rules ()
            ((_ x e ...)(and (not (eqv? x e)) ...))))


    (define-syntax or-equal?
        (syntax-rules ()
            ((_ x e ...)(or (equal? x e) ...))))


    (define-syntax and-equal?
        (syntax-rules ()
            ((_ x e ...)(and (equal? x e) ...))))


    (define-syntax not-equal?
        (syntax-rules ()
            ((_ x e ...)(and (not (equal? x e)) ...))))


)
