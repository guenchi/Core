(import (core async))

(define g (generator (yield 1)
                     (yield 2)
                     (yield 3)))
(display (g))
(display (g))
(display (g))

;;coroutine-examples = =
(define c (coroutine (a)
                     (yield a)
                     (yield a)
                     (yield a)
                     ))



;;;producer and consumer

(define producer (coroutine ()
                            (yield (coroutine-run consumer 'apple))
                            (yield (coroutine-run consumer 'banana))
                            (yield (coroutine-run consumer 'water))))

(define consumer (coroutine (thing)
                            (define fmt "I got a(an) ~a\n")
                            (printf fmt thing)
                            (yield (coroutine-run producer))
                            (printf fmt thing)
                            (yield (coroutine-run producer))
                            (printf fmt thing)))

(display (coroutine-run c 1))
(display (coroutine-run c 2))
(display (coroutine-dead? c))
(display (coroutine-run c 3))
(display (coroutine-dead? c))

(coroutine-run producer)