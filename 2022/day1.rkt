#lang racket

(define (read-inp) (file->string "day1-p1.txt"))

(define (parse inp)
  (map (λ(bunch)
         (map string->number (string-split bunch "\n")))
       (string-split inp "\n\n")))

(define (sum lst)
  (foldl + 0 lst))

(define (part1 bunches)
  (define sums (map sum bunches))
  (apply max sums))

(define (part2 bunches)
  (define sums (map sum bunches))
  (define sorted (sort sums >))
  (sum (take sorted 3)))

(module+ main
  (define bunches (parse (read-inp)))
  (part1 bunches)
  (part2 bunches))
