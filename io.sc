(library (core io)
         (export file->string file-append port->string
                 read-line)
         (import (chezscheme))
         (define (file->string fname)
           (port->string (open-input-file fname))
           )

         (define (file-append f str)
           (unless (string? str)
             (error 'file-append "string required"))
           (let ((p (open-output-file f 'append)))
             (dynamic-wind void
                           (lambda () (display str p))
                           (lambda () (close-output-port p)))
             )
           )

         (define (port->string p)
           (dynamic-wind void
                         (lambda () (let loop ((s '()))
                                      (let ([c (read-char p)])
                                        (if (eof-object? c)
                                            (list->string (reverse s))
                                            (loop (cons c s)))
                                        )))
                         (lambda () (close-input-port p))))

         (define (read-line port)
           (define (remove-r x)
             (if (and (not (null? x)) (char=? (car x) #\return))
                 (cdr x)
                 x))
           (let loop ((acc '()))
             (let ([c (read-char port)])
               (if (or (eof-object? c) (char=? c #\newline))
                   (reverse (remove-r acc))
                   (loop (cons c acc))))))

         )


(import (core io))
