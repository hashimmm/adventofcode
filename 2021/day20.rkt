#lang racket

(define (parse inp)
  (define algo (list->vector (string->list (first inp))))
  (define img
    (list->vector (map (compose list->vector string->list) (rest (rest inp)))))
  (values algo img))

(define (ref img x y)
  (define max-x (sub1 (vector-length (vector-ref img 0))))
  (define max-y (sub1 (vector-length img)))
  (if (or (< x 0) (< y 0) (> x max-x) (> y max-y))
      #f
      (vector-ref (vector-ref img y) x)))

(define (9-around algo step img x y)
  (for*/list ([y (in-inclusive-range (sub1 y) (add1 y))]
              [x (in-inclusive-range (sub1 x) (add1 x))])
    (or (ref img x y)
        (if (equal? (vector-ref algo 0) #\.)
            #\.
            (if (equal? (vector-ref algo 511) #\#)
                (if (= step 0) #\. #\#)
                ;; alternating case
                (if (even? step) #\. #\#))))))

(define (9->num l)
  (string->number (list->string (map (λ(x) (if (equal? x #\.) #\0 #\1)) l)) 2))

(define (enhance algo img steps)
  (for/fold ([img img])
            ([step (in-range steps)])
    (for/vector ([y (in-inclusive-range -1 (vector-length img))])
      (for/vector ([x (in-inclusive-range -1 (vector-length (vector-ref img 0)))])
        (vector-ref algo (9->num (9-around algo step img x y)))))))

(define (count-lit/1 algo img)
  (apply +
         (vector->list
          (vector-map (λ(row)
                        (vector-count (λ(x) (equal? x #\#)) row))
                      (enhance algo img 2)))))

(define (count-lit/2 algo img)
  (apply +
         (vector->list
          (vector-map (λ(row)
                        (vector-count (λ(x) (equal? x #\#)) row))
                      (enhance algo img 50)))))

(module+ test
  (require rackunit)
  (define inp
    "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#

#..#.
#....
##..#
..#..
..###")
  (define-values (algo img) (parse (string-split inp "\n")))
  (check-equal? (count-lit/1 algo img) 35)
  (check-equal? (count-lit/2 algo img) 3351))

(module+ main
  (define-values (algo img) (parse (with-input-from-file "day20.txt" port->lines)))
  (count-lit/1 algo img) ;5464
  (time (count-lit/2 algo img)) #;19228 )
