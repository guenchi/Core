;  MIT License

;  Copyright guenchi (c) 2018 - 2019 
         
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






(library (core file)
  (export
    read-file
    write-file
  )
  (import 
    (chezscheme))

  (define read-file
    (lambda (file)
      (call-with-input-file file (lambda (p) 
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



                                   

         )
