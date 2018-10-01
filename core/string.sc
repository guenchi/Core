(library (core string)
    (export
        split
        string-prefix?
        string-split
        build-string
        string-join
        string-find
        string-replace)
    (import
        (scheme))

    (define split
        (lambda (s c)
            (letrec* ((len (string-length s))
                (walk (lambda (str begin end rst)
                        (cond 
                            ((>= begin len) rst)
                            ((or (= end len) (char=? (string-ref str end) c))
                                (walk 
                                    str 
                                    (+ end 1)
                                    (+ end 1)
                                    (if (= begin end) 
                                        rst
                                        (cons (substring str begin end) rst))))
                            (else (walk str begin (+ end 1) rst))))))
            (reverse (walk s 0 0 '())))))
 

         (define (string-prefix? str pre)
           (define len1 (string-length str))
           (define len2 (string-length pre))
           (cond
             [(> len2 len1) #f]
             [(string=? (substring str 0 len2) pre) #t]
             [else #f]))

         (define (string-split str sep)
           (define len (string-length sep))
           (define (split str acc)
             (cond
               [(string=? str "") (list (list->string (reverse acc)))]
               [(string-prefix? str sep) (cons (list->string (reverse acc))
                                               (split (substring str len
                                                                 (string-length str)) '()))]
               [else (split (substring str 1 (string-length str))
                            (cons (string-ref str 0) acc))]
               ))
           (split str '()))

         (define (build-string n proc)
           (list->string
            (let loop ((s 0))
              (if (>= s n)
                  '()
                  (cons (proc s) (loop (+ s 1)))))))

         (define (string-join lst sep)
           (cond [(null? lst) ""]
                 [(null? (cdr lst)) (car lst)]
                 [else (string-append (car lst) sep (string-join (cdr lst) sep))]))

         (define (string-find str to-find)
           (define len (string-length to-find))
           (define strlen (string-length str))
           (let loop ((pos 0))
                (cond
               [(> (+ len pos) strlen) '()]
               [(string-prefix? (substring str pos (+ pos len)) to-find) (cons pos (loop (+ pos len)))]
               [else (loop (+ pos 1))])))

         (define (string-replace str from to)
           (define len (string-length from))
           (define strlen (string-length str))
           (define *replace* (reverse (string->list to)))
           (let loop ((pos 0) (acc '()))
             (cond
               [(>= pos strlen) (list->string (reverse acc))]
               [(and (<= (+ len pos) strlen)
                     (string-prefix? (substring str pos (+ pos len)) from))
                (loop (+ pos len) (append *replace* acc))]
               [else (loop (+ pos 1) (cons (string-ref str pos) acc))])))


)
