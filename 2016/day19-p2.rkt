#lang typed/racket

;(require pfds/deque/real-time)
(require pfds/deque/implicit)

;(define NUM-ELVES 5)
;(define NUM-ELVES 12)
(define input (read-line))
;(define NUM-ELVES 3012210)

(: input->integer (-> (U EOF String) Integer))
(define (input->integer s)
  (if (equal? eof s)
      (error "Must input an integer")
      (let ([x (string->number s)])
        (if (or (not x) (not (exact-integer? x)))
            (error "Input not an integer.")
            x))))

(define NUM-ELVES (input->integer input))

(struct middle-tracking-deque ([ld : (Deque Integer)] [rd : (Deque Integer)] [addto : (U 'r 'l)])
  #:transparent)

(: list->deque (-> (Listof Integer) (Deque Integer)))
(define (list->deque l)
  (apply deque l))

(: list->mtd (-> (Listof Integer) middle-tracking-deque))
(define (list->mtd l)
  (define l-len (length l))
  (define-values
    (first-half second-half)
    (split-at l (exact-floor (/ l-len 2))))
  (middle-tracking-deque (list->deque first-half)
                         (list->deque second-half)
                         (if (even? l-len) 'r 'l)))

(: mtd-move (-> middle-tracking-deque middle-tracking-deque))
(define (mtd-move mtd)
  (define rd (middle-tracking-deque-rd mtd))
  (define ld (middle-tracking-deque-ld mtd))
  (define addto (middle-tracking-deque-addto mtd))
  (when (or (empty? rd) (empty? ld))
    (error "empty deque"))
  (middle-tracking-deque (enqueue (head rd) (tail ld))
                         (enqueue (head ld) (tail rd))
                         addto))

(: mtd-delete-middle (-> middle-tracking-deque middle-tracking-deque))
(define (mtd-delete-middle mtd)
  (define rd (middle-tracking-deque-rd mtd))
  (define ld (middle-tracking-deque-ld mtd))
  (define addto (middle-tracking-deque-addto mtd))
  (when (or (empty? rd) (empty? ld))
    (error "empty deque"))
  (if (symbol=? 'r addto)
      (middle-tracking-deque (init ld) (enqueue-front (last ld) (tail rd)) 'l)
      (middle-tracking-deque ld (tail rd) 'r)))

(: ans middle-tracking-deque)
(define ans
  (for/fold ([current-mtd (list->mtd (range 1 (+ NUM-ELVES 1)))])
            ([i (in-range (- NUM-ELVES 2))])
    (mtd-move (mtd-delete-middle current-mtd))))

(displayln "Answer: ")
(deque->list (middle-tracking-deque-ld ans))
