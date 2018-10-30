;;library for data structures

(library (core data)
         (export (rename [internal-queue queue]) queue? queue-push queue-pop! queue-push!
                 (rename [internal-stream stream]
                         [stream-null!? stream-null?]
                         [internal-stream? stream?])
                 stream-null
                 stream-car
                 stream-cdr
                 stream-take
                 integers-from
                 )
         (import (chezscheme))

         ;;;Queues

         (define-record queue (head tail))

         (define (internal-queue . args)
           (make-queue '() args))

         (define (queue-push q val)
           (define h (queue-head q))
           (define t (queue-tail q))
           (make-queue (cons val h) t))

         (define (queue-push! q val)
           (set-queue-head! q (cons val (queue-head q))))

         (define (queue-pop! q)
           (define h (queue-head q))
           (define t (queue-tail q))
           (cond [(and (null? t) (null? h)) (error 'queue-pop! "Nothing to pop.")]
                 [(null? t) (set-queue-head! q '())
                            (let ([nt (reverse h)])
                              (set-queue-tail! q (cdr nt))
                              (car nt))]
                 [else (set-queue-tail! q (cdr t))
                       (car t)]))

         (define (queue-empty? q)
           (and (null? (queue-head q)) (null? (queue-tail q))))


         ;;;

         ;;;Streams
         (define-record stream (value rest))
         (define-record stream-null! ())
         (define stream-null (make-stream-null!))

         (define-syntax internal-stream
           (syntax-rules ()
             ((_) stream-null)
             ((_ arg . rest) (make-stream arg
                                          (delay (internal-stream . rest))))))

         (define-syntax stream-cons
           (syntax-rules ()
             ((_ a b) (make-stream a
                                   (delay b)))))
         (define stream-car stream-value)

         (define (stream-cdr s)
           (force (stream-rest s))
           )

         (define (internal-stream? s)
           (or (stream? s) (stream-null!? s)))

         (define (stream-take s n)
           (if (or (= n 0) (stream-null!? s))
               '()
               (cons (stream-car s) (stream-take (stream-cdr s) (- n 1)))))

         (define (integers-from n)
           (stream-cons n
                        (integers-from (+ n 1))))
           
         )

(import (core data))
