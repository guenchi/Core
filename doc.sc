;;;This Application provides details for functions and syntax in Core library.


(import (chezscheme))
(define (string-prefix? str pre)
  (define len1 (string-length str))
  (define len2 (string-length pre))
  (cond
    ((> len2 len1) #f)
    ((string=? (substring str 0 len2) pre) #t)
    (else #f)))
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

(define desc '())
(define (make-desc keywords . args)
  (set! desc (cons (cons keywords (string-join args "\n")) desc)))

(define (read-string)
  (let loop ((tmp '()))
    (define c (read-char))
    (if (or (char(eof-object? c))
        (list->string (reverse tmp))
        (loop (cons c tmp)))))
          

(display "This Application provides details for functions and syntax in Core library.\n")

;;core string
(make-desc "core string string-join join connect"
           "(import (core string))"
           "(string-join <strs:list of strings> <sep:string>) => string"
           "Appends the strings in strs, inserting sep between each pair of strings in strs.")

(make-desc "core string string-find find search"
           "(import (core string))"
           "(string-find <str:string> <to-find:string>) => list of pairs"
           "returns list of positions of to-find in str")







