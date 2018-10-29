;;library for data structures

(library (core data)
         (export (rename [internal-queue queue]) queue? queue-push queue-pop! queue-push!
                 
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
         
         )
