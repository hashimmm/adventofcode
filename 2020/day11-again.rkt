#lang racket

;; So the threads version was mind-bending enough that I did this separately
;; too.
;; This was simple enough... fun fact: part2 is faster than part1.
;;
;; See day11.rkt for the threads version, implementing only p1.

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

;; Helpers for p1

(define (memoize fun)
  (define memory (make-hash))
  (λ x
    (if (hash-has-key? memory x)
        (hash-ref memory x)
        (begin
          (let ([result (apply fun x)])
            (hash-set! memory x result)
            result)))))

(define (insert-at l idx v)
  (define-values (h t) (split-at l idx))
  (append h (cons v t)))

(define (remove-at l idx)
  (define-values (h t) (split-at l idx))
  (append h (rest t)))

(define (neighbours x-size y-size posn)
  (define xs (range (max (sub1 ($posn-x posn)) 0)
                    (min (+ ($posn-x posn) 2) x-size)))
  (define ys (range (max (sub1 ($posn-y posn)) 0)
                    (min (+ ($posn-y posn) 2) y-size)))
  (filter (λ(x) (not (equal? posn x)))
          (map (λ(x) (apply $posn x))
               (cartesian-product xs ys))))

(define neighbours! (memoize neighbours))

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

;; Helpers for p2

(define (parameterized-memoize fun arg param-no)
  (define memory (make-hash))
  (λ x
    (if (hash-has-key? memory x)
        (hash-ref memory x)
        (begin
          (let ([result (apply fun (insert-at x param-no arg))])
            (hash-set! memory x result)
            result)))))

(define (seats-in-dirs layout posn)
  (define x-size (vector-length (vector-ref layout 0)))
  (define y-size (vector-length layout))
  (define (N p)
    (and p (not (= 0 ($posn-y p)))
         ($posn ($posn-x p) (sub1 ($posn-y p)))))
  (define (S p)
    (and p (not (= (sub1 y-size) ($posn-y p)))
         ($posn ($posn-x p) (add1 ($posn-y p)))))
  (define (W p)
    (and p (not (= 0 ($posn-x p)))
         ($posn (sub1 ($posn-x p)) ($posn-y p))))
  (define (E p)
    (and p (not (= ($posn-x p) (sub1 x-size)))
         ($posn (add1 ($posn-x p)) ($posn-y p))))
  (define (NW p) (N (W p)))
  (define (NE p) (N (E p)))
  (define (SE p) (S (E p)))
  (define (SW p) (S (W p)))
  (define (find-seat p dirf)
    (let loop ([nextp (dirf p)])
      (and nextp (if (not (floor? (get-state layout nextp)))
                     nextp
                     (loop (dirf nextp))))))
  (filter identity
          (for/list ([dirf (in-list (list N S W E NW NE SE SW))])
            (find-seat posn dirf))))

(define (make-seat-checker! layout)
  (parameterized-memoize seats-in-dirs layout 0))

(define (next-state-2 state visible-seats)
  (cond [(and (empty? state)
              (andmap (λ(s) (or (empty? s) (floor? s))) visible-seats))
         occupied]
        [(and (occupied? state)
              (>= (length (filter occupied? visible-seats))
                  5))
         empty]
        [else
         state]))

(define (get-state layout posn)
  (vector-ref (vector-ref layout ($posn-y posn)) ($posn-x posn)))

(define (run-sim layout)
  (define x-size (vector-length (vector-ref layout 0)))
  (define y-size (vector-length layout))
  (for/fold ([layout layout])
            ([_ (in-range 0 1 0)])
    (define next-layout
      (for/vector ([(v y) (in-indexed (in-vector layout))])
        (for/vector ([(state x) (in-indexed (in-vector v))])
          (next-state state (map (λ(p) (get-state layout p))
                                 (neighbours! x-size y-size ($posn x y)))))))
    #:break (equal? next-layout layout)
    next-layout))

(define (run-sim-2 layout)
  (define x-size (vector-length (vector-ref layout 0)))
  (define y-size (vector-length layout))
  (define seats-in-dirs! (make-seat-checker! layout))
  (for/fold ([layout layout])
            ([_ (in-range 0 1 0)])
    (define next-layout
      (for/vector ([(v y) (in-indexed (in-vector layout))])
        (for/vector ([(state x) (in-indexed (in-vector v))])
          (next-state-2 state (map (λ(p) (get-state layout p))
                                   (seats-in-dirs! ($posn x y)))))))
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
  (check-equal? (count-occupied (run-sim-2 test-inp)) 26)
  ;;Answers
  (check-equal? 2329 (count-occupied (time (run-sim inp))))
  (check-equal? 2138 (time (count-occupied (run-sim-2 inp)))))
