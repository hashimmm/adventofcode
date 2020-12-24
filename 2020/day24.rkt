#lang racket

(define (parse-inp str-lst)
  (map (λ(x) (regexp-match* #rx"(e|w|ne|nw|se|sw)" x)) str-lst))

(define inp (parse-inp (file->lines "day24-input-1")))

(struct $xy (x y) #:transparent)

(define (get-position x y dir)
  (match dir
        ["ne" (values (+ x 1/2) (add1 y))]
        ["nw" (values (- x 1/2) (add1 y))]
        ["se" (values (+ x 1/2) (sub1 y))]
        ["sw" (values (- x 1/2) (sub1 y))]
        ["w" (values (sub1 x) y)]
        ["e" (values (add1 x) y)]))

(define (get-positions tile-specs)
  (for/list ([t (in-list tile-specs)])
    (for/fold ([x 0] [y 0] #:result ($xy x y))
              ([dir (in-list t)])
      (get-position x y dir))))

(define (count-occurrences l)
  (let loop ([occurrences (hash)]
             [remaining l])
    (if (null? remaining)
        occurrences
        (loop (hash-update occurrences (first remaining) add1 0)
              (rest remaining)))))

(define (count-black-flips positions)
  (define occurrences (count-occurrences positions))
  (count odd? (hash-values occurrences)))

(define (get-black positions)
  (define occurrences (count-occurrences positions))
  (filter-map (λ(x) (if (odd? (cdr x)) (car x) #f)) (hash->list occurrences)))

(define (get-adjs xy)
  (map (λ(dir) (let-values ([(x y) (get-position ($xy-x xy) ($xy-y xy) dir)])
                 ($xy x y)))
       '("ne" "nw" "se" "sw" "w" "e")))

(define (simulate-rules blacks)
  (define whites
    (set-subtract (list->set (append-map get-adjs (set->list blacks))) blacks))
  (define flip-to-white
    (list->set
     (filter (λ(b) (let ([adj-blacks (count (λ(a) (set-member? blacks a))
                                            (get-adjs b))])
                     (or (= 0 adj-blacks)
                         (> adj-blacks 2))))
             (set->list blacks))))
  (define flip-to-black
    (list->set
     (filter (λ(p) (let ([adj-blacks (count (λ(x) (set-member? blacks x))
                                            (get-adjs p))])
                     (= 2 adj-blacks)))
             (set->list whites))))
  (set-union
   (set-subtract blacks flip-to-white)
   flip-to-black))

(define (run-100 positions)
  (define init-black (list->set (get-black positions)))
  (set-count
   (for/fold ([blacks init-black])
             ([_ (in-range 100)])
     (simulate-rules blacks))))

(module+ test
  (require rackunit)
  (define test-inp (parse-inp
                        '("sesenwnenenewseeswwswswwnenewsewsw"
                          "neeenesenwnwwswnenewnwwsewnenwseswesw"
                          "seswneswswsenwwnwse"
                          "nwnwneseeswswnenewneswwnewseswneseene"
                          "swweswneswnenwsewnwneneseenw"
                          "eesenwseswswnenwswnwnwsewwnwsene"
                          "sewnenenenesenwsewnenwwwse"
                          "wenwwweseeeweswwwnwwe"
                          "wsweesenenewnwwnwsenewsenwwsesesenwne"
                          "neeswseenwwswnwswswnw"
                          "nenwswwsewswnenenewsenwsenwnesesenew"
                          "enewnwewneswsewnwswenweswnenwsenwsw"
                          "sweneswneswneneenwnewenewwneswswnese"
                          "swwesenesewenwneswnwwneseswwne"
                          "enesenwswwswneneswsenwnewswseenwsese"
                          "wnwnesenesenenwwnenwsewesewsesesew"
                          "nenewswnwewswnenesenwnesewesw"
                          "eneswnwswnwsenenwnwnwwseeswneewsenese"
                          "neswnwewnwnwseenwseesewsenwsweewe"
                          "wseweeenwnesenwwwswnew")))
  (check-equal? (count-black-flips (get-positions test-inp)) 10)
  (check-equal? (length (get-black (get-positions test-inp))) 10)
  (check-equal? (set-count
                 (simulate-rules (list->set (get-black (get-positions test-inp)))))
                15)
  (check-equal? (run-100 (get-positions test-inp)) 2208)
  ;; Answers
  (check-equal? (count-black-flips (get-positions inp)) 436)
  (check-equal? (time (run-100 (get-positions inp))) 4133))
