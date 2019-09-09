(library (core io)
  (export 
    read-file
    write-file
    file->string
    file-append
    port->string
    read-line
  )
  (import 
    (chezscheme)
    (core string))



  (define read-file
    (lambda (file)
      (call-with-input-file
        file 
        (lambda (p) 
          (let f ((k (read-char p)))
            (if (eof-object? k)
                '()
                (cons k (f (read-char p)))))))))


  (define write-file
    (lambda (file lst)
        (call-with-output-file file
            (lambda (p)
                (let loop ((l lst))
                    (display (car l) p)
                    (unless (null? (cdr l))
                        (loop (cdr l))))))))


  (define file->string 
    (lambda (fname)
      (port->string 
        (open-input-file fname))))

  
  (define file-append  
    (lambda (f str)
      (unless (string? str)
        (error 'file-append "string required"))
      (let ((p (open-output-file f 'append)))
        (dynamic-wind 
          void
          (lambda () (display str p))
          (lambda () (close-output-port p))))))
       

  (define port->string  
    (lambda (p)
      (dynamic-wind 
        void
        (lambda () 
          (let loop ((s '()))
            (let ((c (read-char p)))
              (if (eof-object? c)
                  (list->string (reverse s))
                  (loop (cons c s))))))
        (lambda () (close-input-port p)))))


  (define read-line  
    (lambda (port)
      (define remove-r
        (lambda (x)
          (if (and (not (null? x)) 
                   (char=? (car x) #\return))
              (cdr x)
              x)))
      (let loop ((acc '()))
        (let ((c (read-char port)))
          (if (or (eof-object? c)
                  (char=? c #\newline))
              (reverse (remove-r acc))
              (loop (cons c acc)))))))


  (define remove-r  
    (lambda (x)
      (define strlen 
        (string-length x))
      (define c 
        (string-ref x strlen))
      (if (char=? c #\return)
          (substring x 0 (- strlen 1))
          x)))


  (define file->lines  
    (lambda (f)
      (define str (file->string f))
      (map remove-r (split str #\newline))))
         
)


