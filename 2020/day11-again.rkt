#lang racket

(define inp
  (vector-map (compose list->vector string->list)
              (list->vector (file->lines "day11-input-1"))))

(define empty #\L)
(define floor #\.)
(define occupied #\#)
(define empty? (λ(x) (equal? x empty)))
(define floor? (λ(x) (equal? x floor)))
(define occupied? (λ(x) (equal? x occupied)))

(struct $posn (x y) #:transparent)

(define (neighbours layout posn)
  (define xs (range (max (sub1 ($posn-x posn)) 0)
                    (min (+ ($posn-x posn) 2) (vector-length (vector-ref layout 0)))))
  (define ys (range (max (sub1 ($posn-y posn)) 0)
                    (min (+ ($posn-y posn) 2) (vector-length layout))))
  (filter (λ(x) (not (equal? posn x)))
          (map (λ(x) (apply $posn x))
               (cartesian-product xs ys))))

(define (next-state state adjs)
  (cond [(and (empty? state)
              (andmap (λ(s) (or (empty? s) (floor? s))) adjs))
         occupied]
        [(and (occupied? state)
              (>= (length (filter occupied? adjs))
                  4))
         empty]
        [else
         state]))

(define (get-state layout posn)
  (vector-ref (vector-ref layout ($posn-y posn)) ($posn-x posn)))

(define (run-sim layout)
  (for/fold ([layout layout])
            ([_ (in-range 0 1 0)])
    (define next-layout
      (for/vector ([(v y) (in-indexed (in-vector layout))])
        (for/vector ([(place x) (in-indexed (in-vector v))])
          (next-state place (map (λ(p) (get-state layout p))
                                 (neighbours layout ($posn x y)))))))
    #:break (equal? next-layout layout)
    next-layout))

(define (count-occupied layout)
  (count occupied? (for*/list ([v (in-vector layout)] [x (in-vector v)]) x)))

(module+ test
  (require rackunit)
  (define test-inp
    (vector-map
     (compose list->vector string->list)
     (list->vector
      '("L.LL.LL.LL"
        "LLLLLLL.LL"
        "L.L.L..L.."
        "LLLL.LL.LL"
        "L.LL.LL.LL"
        "L.LLLLL.LL"
        "..L.L....."
        "LLLLLLLLLL"
        "L.LLLLLL.L"
        "L.LLLLL.LL"))))
  (check-equal? (count-occupied (run-sim test-inp)) 37)
  ;;Answers
  (check-equal? 2329 (count-occupied (time (run-sim inp)))))
