 
;  MIT License

;  Copyright syntacticlosure (c) 2018 
         
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





(library (core generic)
         (export generic-set! generic-map first second third fourth
                 fifth sixth seventh eigth ninth tenth eleventh twelfth
                 define-accessor generic-set-property
                 )
         (import (chezscheme) (core syntax) (for (core syntax) expand)
                 (core data))
         (meta define index-identifiers '())
         (define-syntax generic-set-property (syntax-rules ()))
         (trace-define-syntax define-accessor
           (lambda (stx)
             (syntax-case stx ()
               [(_ (name args ...) body)
                #'(begin (define (name args ...) body)
                         (define-property name generic-set-property
                           (lambda (stx)
                             (syntax-case stx ()
                               [(name args ... value) #'(generic-set! body value)]))))])))
         (define-syntax generic-set!
           (lambda (stx)
             (lambda (lookup)
               (syntax-case stx (car cdr vector-ref list-ref if
                                     fxvector-ref eq-hashtable-ref string-ref)
                 [(_ (string-ref s idx) value)
                  #'(string-set! s idx value)]
                 [(_ (eq-hashtable-ref table key rest ...) value)
                  #'(eq-hashtable-set! table key value)]
                 [(_ (if condition var1 var2) value)
                  #'(if condition (set-generic! var1 value)
                        (set-generic! var2 value))]
                 [(_ (fxvector-ref var index) value)
                  #'(fxvector-set! var index value)]
                 [(_ (car var) value)
                  #'(set-car! var value)]
                 [(_ (cdr var) value)
                  #'(set-cdr! var value)]
                 [(_ (vector-ref var index) value)
                  #'(vector-set! var index value)]
                 [(_ (list-ref var index) value)
                  #'(list-set! var index value)]
                 [(_ (maybe-accessor var) value)
                  (assoc/free-identifier=? #'maybe-accessor index-identifiers)
                  (let ([idx
                         (cdr (assoc/free-identifier=? #'maybe-accessor index-identifiers))])
                    #`(let ([real-var var])
                        (cond
                          [(list? real-var) (list-set! var #,idx value)]
                          [(vector? var) (vector-set! var #,idx value)]
                          [(fxvector? var) (fxvector-set! var #,idx value)]
                          [else (error 'set-generic! "unknown datatype")])))]
                 [(_ (maybe-accessor args ...) value)
                  (lookup #'maybe-accessor #'generic-set-property)
                  ((lookup #'maybe-accessor #'generic-set-property)
                   #'(maybe-accessor args ... value))]
                 [(_ . any-other-forms) #'(set! . any-other-forms)]
                 ))))



         (define (generic-map f list-or-vector)
           (cond
             [(list? list-or-vector) (map f list-or-vector)]
             [(vector? list-or-vector) (vector-map f list-or-vector)]
             [(stream? list-or-vector) (stream-map f list-or-vector)]
             [(queue? list-or-vector) (queue-map f list-or-vector)]
             [else (error 'map-generic "unknown datatype")]))

         (define (list-set! var index value)
           (cond [(< index 0) (error 'set-generic "invalid index!")]
                 [(= index 0) (set-car! var value)]
                 [else (list-set! (cdr var) (- index 1) value)]))

         (define-syntax make-index-identifier
           (lambda (stx)
             (syntax-case stx ()
               [(_ index name)
                (set! index-identifiers (cons (cons #'name #'index) index-identifiers))
                #`(begin (define (name x)
                           (cond
                             [(list? x) (list-ref x index)]
                             [(vector? x) (vector-ref x index)]
                             [(fxvector? x) (fxvector-ref x index)]
                             [(stream? x) (stream-ref x index)]
                             [else (error 'fourth "unknown datatype")])))])))
         (make-index-identifier 0 first)
         (make-index-identifier 1 second)
         (make-index-identifier 2 third)
         (make-index-identifier 3 fourth)
         (make-index-identifier 4 fifth)
         (make-index-identifier 5 sixth)
         (make-index-identifier 6 seventh)
         (make-index-identifier 7 eigth)
         (make-index-identifier 8 ninth)
         (make-index-identifier 9 tenth)
         (make-index-identifier 10 eleventh)
         (make-index-identifier 11 twelfth)

         ;;;;;;;;;;;;;;;;
         )
(import (core generic))


