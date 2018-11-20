(import (chezscheme) (core async) (core io) (core loop))
;;Traditional Way
(time (for i in (range 200)
        (define v (file->string "async.sc"))
        (file-append "test1.txt" v)
        ))

(time (letrec ([cs (for/vector i in (range 200)
                  (coroutine ()
                             (define p (open-input-file "async.sc"))
                             (define v (port->string p))
                             (yield (coroutine-run (vector-ref cs (remainder (+ i 1) 100))))
                             (close-input-port p)
                             (file-append "test2.txt" v)
                             (when (< i 99)
                               (yield (coroutine-run (vector-ref cs (+ i 1)))))))])
        (coroutine-run (vector-ref cs 0))))

