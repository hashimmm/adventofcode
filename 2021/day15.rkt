#lang racket

(require graph)
(require srfi/1)

(define (parse inp)
  (vector-map (λ(s) (list->vector (map (λ(c) (string->number (string c))) (string->list s))))
              (list->vector inp)))

(define (moves grid x y)
  (define max-x (sub1 (vector-length (vector-ref grid 0))))
  (define max-y (sub1 (vector-length grid)))
  (filter identity (list (if (> x 0) (cons (sub1 x) y) #f)
                         (if (> y 0) (cons x (sub1 y)) #f)
                         (if (< x max-x) (cons (add1 x) y) #f)
                         (if (< y max-y) (cons x (add1 y)) #f))))

(define/match (get-pt grid xy)
  [(grid (Pt x y))
   (vector-ref (vector-ref grid y) x)])

;; So I didn't really solve anything, I just delegated to teh `graph` package
;; which has a handy `dijkstra` method.
;; All I did was write a function to massage the input to a form
;; the graph library expects.
;; And for part 2, write an `enlarge` function.

(struct Pt (x y) #:transparent)

(define (grid->graph grid)
  (define max-y (vector-length grid))
  (define max-x (vector-length (vector-ref grid 0)))
  (define (get-wt xy) (get-pt grid xy))
  (define horiz-vertices
    (for/fold ([all-vs '()])
              ([y (in-range max-y)])
      (define vs
        (zip (map (λ(n) (Pt n y)) (range max-x))
             (map (λ(n) (Pt n y)) (rest (range max-x)))))
      (define weighted-vs
        (map (match-lambda [(and v (list src dst))
                            (cons (get-wt dst) v)])
             vs))
      (define reverse-weighted-vs
        (map (match-lambda [(cons _ (list src dst))
                            (cons (get-wt src) (list dst src))])
             weighted-vs))
      (append all-vs weighted-vs reverse-weighted-vs)))
  (define vert-vertices
    (for/fold ([all-vs '()])
              ([x (in-range max-x)])
      (define vs
        (zip (map (λ(n) (Pt x n)) (range max-y))
             (map (λ(n) (Pt x n)) (rest (range max-y)))))
      (define weighted-vs
        (map (match-lambda [(and v (list src dst))
                            (cons (get-wt dst) v)])
             vs))
      (define reverse-weighted-vs
        (map (match-lambda [(cons _ (list src dst))
                            (cons (get-wt src) (list dst src))])
             weighted-vs))
      (append all-vs weighted-vs reverse-weighted-vs)))
  (weighted-graph/directed (append horiz-vertices vert-vertices)))

(define (shortest-path-to-btm-right/1 inp)
  (define g (grid->graph inp))
  (define-values (costs _) (dijkstra g (Pt 0 0)))
  (hash-ref costs (Pt (sub1 (vector-length (vector-ref inp 0)))
                      (sub1 (vector-length inp)))))

(define (debug . x) (map displayln x) (first x))

(define (enlarge-grid grid)
  (define size-x (vector-length (vector-ref grid 0)))
  (define size-y (vector-length grid))
  (define large-size-y (* size-y 5))
  (define large-size-x (* size-x 5))
  (define gg
    (for*/hash ([ky (in-range 5)]
                [kx (in-range 5)])
      (define offset (+ kx ky))
      (values (cons kx ky)
              (vector-map (λ(row)
                            (vector-map (λ(n) (let ([m (+ n offset)])
                                                (- m (* (quotient (sub1 m) 9) 9))))
                                        row))
                          grid))))
  (for/vector ([y (in-range large-size-y)])
    (for/vector ([x (in-range large-size-x)])
      (define piece-key (cons (floor (/ x (/ large-size-x 5)))
                              (floor (/ y (/ large-size-y 5)))))
      (define piece (hash-ref gg piece-key))
      (define row (vector-ref piece (remainder y size-y)))
      (vector-ref row (remainder x size-x)))))

(module+ test
  (require rackunit)
  (define test-inp "1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581")
  (define test-map (parse (string-split test-inp "\n")))
  (check-equal? (shortest-path-to-btm-right/1 test-map) 40)
  (check-equal? (shortest-path-to-btm-right/1 (enlarge-grid test-map)) 315))

(module+ main
  (define grid (parse (with-input-from-file "day15.txt" port->lines)))
  (time (shortest-path-to-btm-right/1 grid)) ;; 604
  (time (shortest-path-to-btm-right/1 (enlarge-grid grid))) #;2907 )
