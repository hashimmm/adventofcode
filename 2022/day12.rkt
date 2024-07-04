#lang racket

(define (read-inp)
  (define as-lists (map string->list (file->lines "day12.txt")))
  (list->vector (map list->vector as-lists)))

(define (->elevation ch)
  (cond [(equal? ch #\S) 1]
        [(equal? ch #\E) 26]
        [else (- (char->integer ch) 96)]))

(define (grid-ref grid x y)
  (vector-ref (vector-ref grid y) x))

(define (within-limits grid x y)
  (and (>= x 0) (>= y 0) (< y (vector-length grid)) (< x (vector-length (vector-ref grid 0)))))

(struct pt (x y) #:transparent)

(define (candidates x y) (list (pt x (add1 y)) (pt x (sub1 y)) (pt (sub1 x) y) (pt (add1 x) y)))

(define (search grid (start-label #\S) (end-label #\E) (step-fn (λ(n c) (<= n (add1 c)))))
  (define (possible-moves x y)
    (define possibilities (candidates x y))
    (define cur-height (->elevation (grid-ref grid x y)))
    (for/list ([move possibilities]
               #:when (and (within-limits grid (pt-x move) (pt-y move))
                           (step-fn (->elevation (grid-ref grid (pt-x move) (pt-y move))) cur-height)
                           (not (set-member? visited move))))
      move))
  (define visited (mutable-set))
  (match-define (pt start-x start-y)
    (for*/last ([(row y) (in-indexed grid)] [(height-label x) (in-indexed row)])
      #:final (equal? height-label start-label)
      (pt x y)))
  (set-add! visited (pt start-x start-y))
  (let loop ([cur-posns (list (pt start-x start-y))]
             [step-num 1])
    (define next-moves
      (remove-duplicates
       (append-map (λ(p) (possible-moves (pt-x p) (pt-y p)))
                   cur-posns)))
    (for ([p next-moves]) (set-add! visited p))
    (if (member end-label (map (λ(p) (grid-ref grid (pt-x p) (pt-y p))) next-moves))
        step-num
        (loop next-moves
              (add1 step-num)))))

(module+ main
  (search (read-inp)) #;497
  (search (read-inp) #\E #\a (λ(n c) (>= n (sub1 c)))) #;492 )
