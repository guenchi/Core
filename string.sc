;  MIT License

;  Copyright guenchi, syntacticlosure (c) 2018 
         
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






(library (core string)
         (export
          split
          string-prefix?
          string-split
          build-string
          string-join  
          string-find
          string-replace
          string-replace*)
         (import
          (scheme))

         (define split
           (lambda (s c)
             (define x (string-length s))
             (let l ((x x)(y (- x 1))(r '()))
               (if (= y -1)
                   (cons (substring s 0 x) r)
                   (if (char=? (string-ref s y) c)
                       (l y (- y 1)(cons (substring s (+ y 1) x) r))
                       (l x (- y 1) r))))))
 

         (define (string-prefix? str pre)
           (define len1 (string-length str))
           (define len2 (string-length pre))
           (cond
             ((> len2 len1) #f)
             ((string=? (substring str 0 len2) pre) #t)
             (else #f)))

         (define (string-split str sep)
           (define len (string-length sep))
           (define (split str acc)
             (cond
               ((string=? str "") (list (list->string (reverse acc))))
               ((string-prefix? str sep) (cons (list->string (reverse acc))
                                               (split (substring str len
                                                                 (string-length str)) '())))
               (else (split (substring str 1 (string-length str))
                            (cons (string-ref str 0) acc)))
               ))
           (split str '()))

         (define (build-string n proc)
           (list->string
            (let loop ((s 0))
              (if (>= s n)
                  '()
                  (cons (proc s) (loop (+ s 1)))))))

         (define (string-join lst sep)
           (cond ((null? lst) "")
                 ((null? (cdr lst)) (car lst))
                 (else (string-append (car lst) sep (string-join (cdr lst) sep)))))

         (define (string-find str to-find)
           (define len (string-length to-find))
           (define strlen (string-length str))
           (let loop ((pos 0))
             (cond
               ((> (+ len pos) strlen) '())
               ((string-prefix? (substring str pos (+ pos len)) to-find) (cons pos (loop (+ pos len))))
               (else (loop (+ pos 1))))))

         (define (string-replace str from to)
           (define len (string-length from))
           (define strlen (string-length str))
           (define *replace* (reverse (string->list to)))
           (let loop ((pos 0) (acc '()))
             (cond
               ((>= pos strlen) (list->string (reverse acc)))
               ((and (<= (+ len pos) strlen)
                     (string=? (substring str pos (+ pos len)) from))
                (loop (+ pos len) (append *replace* acc)))
               (else (loop (+ pos 1) (cons (string-ref str pos) acc))))))

         (define (string-replace* str list-of-from list-of-to)
           (define strlen (string-length str))
           (define from-and-to (map (lambda (x y)
                                      (cons x (reverse (string->list y))))
                                    list-of-from list-of-to))
           (let loop ((pos 0) (acc '()))
             (cond
               ((>= pos strlen) (list->string (reverse acc)))
               (else
                (call/cc (lambda (exit)
                           (for-each
                            (lambda (d)
                              (when (string-prefix?
                                     (substring str pos strlen)
                                     (car d))
                                (exit (loop (+ pos (string-length (car d)))
                                            (append (cdr d) acc)))))
                            from-and-to)
                           (loop (+ pos 1) (cons (string-ref str pos) acc))))))))
                                   

         )

