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
