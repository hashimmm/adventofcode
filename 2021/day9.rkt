#lang racket

(define (parse inp)
  (list->vector
   (map (λ(line) (list->vector (map (λ(c) (string->number (string c)))
                                    (string->list line))))
        inp)))

(define (get-adj-coords caves x y)
  (define all-adj-coords (list (list (add1 x) y)
                               (list (sub1 x) y)
                               (list x (add1 y))
                               (list x (sub1 y))))
  (define max-x (sub1 (vector-length (vector-ref caves 0))))
  (define max-y (sub1 (vector-length caves)))
  (filter (λ(x+y)
            (match-define (list x y) x+y)
            (cond [(< x 0) #f]
                  [(> x max-x) #f]
                  [(< y 0) #f]
                  [(> y max-y) #f]
                  [else #t]))
          all-adj-coords))

(define (get-point caves x y)
  (vector-ref (vector-ref caves y) x))

(define (get-adjacent-points caves x y)
  (define adj-coords (get-adj-coords caves x y))
  (map (λ(x+y)
         (match-define (list x y) x+y)
         (get-point caves x y))
       adj-coords))

(define (get-lows caves)
  (filter
   identity
   (for*/list ([y (in-range (vector-length caves))]
               [x (in-range (vector-length (vector-ref caves 0)))])
     (define adj-points (get-adjacent-points caves x y))
     (define current-point (get-point caves x y))
     (define current-is-lower? (andmap (λ(h) (> h current-point)) adj-points))
     (and current-is-lower? (list x y)))))

(define (risk-level-sum/1 caves)
  (for/sum ([x+y (in-list (get-lows caves))])
    (add1 (get-point caves (first x+y) (second x+y)))))

(define (get-basin-map caves init-x init-y)
  (let loop ([basin-coords (set)]
             [to-check (list (list init-x init-y))])
    (if (empty? to-check)
        basin-coords
        (let ()
          (match-define (list bx by) (first to-check))
          (define bh (get-point caves bx by))
          (define adj-coords (get-adj-coords caves bx by))
          (define higher
            (list->set (filter (λ(x+y)
                                 (match-define (list x y) x+y)
                                 (define h (get-point caves x y))
                                 (and (> h bh)
                                      (not (= h 9))))
                               adj-coords)))
          ;; First I was checking eq+higher above, then had to additionally
          ;; filter so that the point isn't in basin-coords already.
          ;; Saw in another solution that we can just check for higher-than.
          (define new-basin-coords (set-union (set-add higher (list bx by))
                                              basin-coords))
          (loop new-basin-coords
                (append (set->list higher) (rest to-check)))))))

(define (largest-basins-mul/2 caves)
  (define low-coords (get-lows caves))
  (define largest-basins
    (for/fold ([largest-basins '(0 0 0)])
              ([x+y (in-list low-coords)])
      (match-define (list x y) x+y)
      (define h (get-point caves x y))
      (define basin-map (get-basin-map caves x y))
      (define basin-size (set-count basin-map))
      (if (> basin-size (first largest-basins))
          (sort (cons basin-size (rest largest-basins)) <)
          largest-basins)))
  (apply * largest-basins))

;; Unused function, used it for debugging, left it in for no real reason.
(define (printmap mapset caves size-x size-y)
  (define maplist (set->list mapset))
  (define as-lists
    (for/list ([y (in-range size-y)])
      (for/list ([x (in-range size-x)])
        (if (set-member? mapset (list x y))
            (number->string (get-point caves x y))
            "."))))
  (string-join (map (λ(row) (string-join row " ")) as-lists)
               "\n"))

(module+ test
  (require rackunit)
  (define test-inp "2199943210
3987894921
9856789892
8767896789
9899965678")
  (define caves (parse (string-split test-inp)))
  (check-equal? (risk-level-sum/1 caves) 15)
  (check-equal? (largest-basins-mul/2 caves) 1134))

(module+ main
  (define inp (parse (with-input-from-file "day9.txt" port->lines)))
  (risk-level-sum/1 inp)
  (time (largest-basins-mul/2 inp)))
