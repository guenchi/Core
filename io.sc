(library (core io)
         (export file->string file-append port->string)
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

         )


(import (core io))
