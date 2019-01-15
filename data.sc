;;library for data structures

(library (core data)
         (export (rename [internal-queue queue]) queue? queue-push queue-pop! queue-push!
                 queue-map
                 (rename [internal-stream stream]
                         [stream-null!? stream-null?]
                         [internal-stream? stream?])
                 stream-null
                 stream-car
                 stream-cdr
                 stream-take
                 stream-ref
                 stream-map
                 integers-from
                 naturals
                 stream-cons
                 stream*
                 stream-drop
                 
                 set
                 set?
                 set-contains?
                 set-map
                 set-for-each
                 set-add
                 set-remove
                 set->list
                 set-union
                 set-intersect
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

         (define (queue-map f q)
           (make-queue (map f (queue-head q))
                       (map f (queue-tail q))))


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
         (define naturals (integers-from 0))

         (define (stream-ref s n)
           (if (= n 0)
               (stream-car s)
               (stream-ref (stream-cdr s) (- n 1))))

         (define (stream-map f . streams)
           (if (andmap stream-null!? streams)
               stream-null
               (stream-cons (apply f (map stream-car streams))
                            (apply stream-map
                                   (cons f (map stream-cdr streams))))))



         (define-syntax stream*
           (syntax-rules ()
             ((_ a) a)
             ((_ a b c ...) (stream-cons a
                                         (stream* b c ...)))))

         (define (stream-drop s n)
           (if (= n 0)
               s
               (stream-drop (stream-cdr s) (- n 1))))



        ;;;SETS

         (define-record set-record (numbers strings symbols others))
         (define (set-add s e)
           (unless (set-record? s) (error 'set-add "not a set"))
           (cond [(number? e) (if (memv e (set-record-numbers s))
                                  s
                                  (make-set-record (cons e (set-record-numbers s))
                                                   (set-record-strings s)
                                                   (set-record-symbols s)
                                                   (set-record-others s)))]
                 [(string? e) (if (member e (set-record-strings s))
                                  s
                                  (make-set-record (set-record-numbers s)
                                                   (cons e (set-record-strings s))
                                                   (set-record-symbols s)
                                                   (set-record-others s)))]
                 [(symbol? e) (if (memq e (set-record-symbols s))
                                  s
                                  (make-set-record (set-record-numbers s)
                                                   (set-record-strings s)
                                                   (cons e (set-record-symbols s))
                                                   (set-record-others s)))]
                 [else (if (member e (set-record-others s))
                                  s
                                  (make-set-record (set-record-numbers s)
                                                   (set-record-strings s)
                                                   (set-record-symbols s)
                                                   (cons e (set-record-others s))))]))
         (define (empty-set) (make-set-record '() '() '() '()))
         (define (set . lst)
           (fold-right (lambda (x y)
                         (set-add y x))
                       (empty-set)
                       lst))

         (define (set-contains? s e)
           (or (memv e (set-record-numbers s))
               (member e (set-record-strings s))
               (memq e (set-record-symbols s))
               (member e (set-record-others s))))
         (define (set? s)
           (set-record? s))

         (define (set-map f s)
           (make-set-record (map f (set-record-numbers s))
                            (map f (set-record-strings s))
                            (map f (set-record-symbols s))
                            (map f (set-record-others s))))

         (define (set-for-each f s)
           (for-each f (set-record-numbers s))
           (for-each f (set-record-strings s))
           (for-each f (set-record-symbols s))
           (for-each f (set-record-others s)))

         (define (set-remove s e)
           (cond [(number? e) (make-set-record (remv e (set-record-numbers s))
                                               (set-record-strings s)
                                               (set-record-symbols s)
                                               (set-record-others s))]
                 [(string? e)
                  (make-set-record (set-record-numbers s)
                                   (remove e (set-record-strings s))
                                   (set-record-symbols s)
                                   (set-record-others s))]
                 [(symbol? e)
                  (make-set-record (set-record-numbers s)
                                   (set-record-strings s)
                                   (remq e (set-record-symbols s))
                                   (set-record-others s))]
                 [else (make-set-record (set-record-numbers s)
                                        (set-record-strings s)
                                        (set-record-symbols s)
                                        (remove e (set-record-others s)))]))

         (define (set->list s)
           (append (set-record-numbers s)
                   (set-record-strings s)
                   (set-record-symbols s)
                   (set-record-others s)))
         (define (set-union a b . lst)
           (if (null? lst)
               (fold-right (lambda (x y)
                             (set-add y x))
                           b
                           (set->list a))
               (set-union a (apply set-union (cons b lst)))))

         (define (list-intersect pred a b)
           (filter (lambda (x)
                     (pred x b)) a))

         (define (list-substract f a b)
           (fold-right (lambda (x y)
                         (f x y))
                       a
                       b))
         (define (set-intersect a b . lst)
           (if (null? lst)
               (make-set-record (list-intersect memv (set-record-numbers a)
                                                (set-record-numbers b))
                                (list-intersect member (set-record-strings a)
                                                (set-record-strings b))
                                (list-intersect memq (set-record-symbols a)
                                                (set-record-symbols b))
                                (list-intersect member (set-record-others a)
                                                (set-record-others b)))
               (set-intersect a (apply set-intersect (cons b lst)))))

         (define (set-subtract a b . lst)
           (if (null? lst)
               (make-set-record (list-subtract remv (set-record-numbers a)
                                               (set-record-numbers b))
                                (list-subtract remove (set-record-strings a)
                                               (set-record-strings b))
                                (list-subtract remq (set-record-symbols a)
                                               (set-record-symbols b))
                                (list-subtract remove (set-record-others a)
                                               (set-record-others b)))
               (set-subtract a (apply set-union (cons b lst)))))
         )

(import (core data))
