#lang racket

(struct O (n flashed) #:transparent #:mutable)

(define (make-oct n) (O n #f))
(define (flash! oct)
  (if (O-flashed oct)
      #f
      (begin (set-O-flashed! #t) #t)))

(define (parse lines)
  (map (λ(s) (map (λ(c) (make-oct (string->number (string c)))) (string->list s))) lines))

(define (adj-coords grid x y)
  (define max-x (length (first grid)))
  (define max-y (length grid))
  (define (within-bounds x+y)
    (match-define (list x y) x+y)
    (and (>= x 0) (>= y 0) (< y max-y) (< x max-x) (list x y)))
  (filter-map within-bounds
              (list (list (add1 x) y)
                    (list x (add1 y))
                    (list (add1 x) (add1 y))
                    (list (sub1 x) y)
                    (list x (sub1 y))
                    (list (sub1 x) (sub1 y))
                    (list (add1 x) (sub1 y))
                    (list (sub1 x) (add1 y)))))

(define (get-point grid x y)
  (list-ref (list-ref grid y) x))

(define (map-grid-items grid f)  ;; apply f to each item of grid.
  (map (λ(row) (map f row)) grid))

(define (flash-step grid flashes)
  (define points-to-flash
    (for*/list ([y (in-range (length grid))]
                [x (in-range (length (first grid)))]
                #:when (and (> (O-n (get-point grid x y)) 9)
                            (not (O-flashed (get-point grid x y)))))
      (set-O-flashed! (get-point grid x y) #t)
      (list x y)))
  (if (null? points-to-flash)
      (values grid flashes)
      (let ()
        (for ([x+y (in-list points-to-flash)])
          (define adj-points (adj-coords grid (first x+y) (second x+y)))
          (for ([ax+ay (in-list adj-points)])
            (define adj-oct (get-point grid (first ax+ay) (second ax+ay)))
            (set-O-n! adj-oct (add1 (O-n adj-oct)))))
        (flash-step grid (+ flashes (length points-to-flash))))))

(define (step grid)
  (define next-grid (map-grid-items grid (λ(oct) (begin (set-O-n! oct (add1 (O-n oct))) oct))))
  (define-values (flashed-grid flashes) (flash-step next-grid 0))
  (values (map-grid-items flashed-grid (λ(oct) (if (O-flashed oct) (make-oct 0) oct)))
          flashes))

(define (how-many-flashes/1 grid)
  (for/fold ([grid grid]
             [flashes 0])
            ([_ (in-range 100)])
    (define-values (next-grid current-flashes) (step grid))
    (values next-grid
            (+ flashes current-flashes))))

(define (how-many-steps-to-sync/2 grid)
  (define grid-size (* (length grid) (length (first grid))))
  (for/fold ([grid grid]
             [flashes 0]
             [steps-so-far 1]
             #:result steps-so-far)
            ([_ (in-naturals)])
    (define-values (next-grid current-flashes) (step grid))
    #:break (= current-flashes grid-size)
    (values next-grid
            (+ flashes current-flashes)
            (add1 steps-so-far))))

(module+ test
  (require rackunit)
  (define inp "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526")
  (define parsed (parse (string-split inp "\n")))
  (define-values (final-grid flashes) (how-many-flashes/1 parsed))
  (check-equal? flashes 1656)
  (check-equal? (how-many-steps-to-sync/2 (parse (string-split inp "\n"))) 195))

(module+ main
  (define octopuses (parse (with-input-from-file "day11.txt" port->lines)))
  (define-values (final-grid flashes) (how-many-flashes/1 octopuses))
  flashes ; 1644
  ;; re-parsing because mutation
  (define octopuses/2 (parse (with-input-from-file "day11.txt" port->lines)))
  (how-many-steps-to-sync/2 octopuses/2) #;229 )
