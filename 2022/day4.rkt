#lang racket

(define (read-inp)
  (map (λ(line)
         (define parts (string-split line ","))
         (map (λ(p) (map string->number (string-split p "-"))) parts))
       (file->lines "day4.txt")))

;; One range contains the other if the min of the starting points
;; and max of the ending points is the same as one of the ranges.
(define (contains? a1 a2)
  (define x (list (min (first a1) (first a2)) (max (second a1) (second a2))))
  (or (equal? x a1) (equal? x a2)))

;; The ranges overlap if the distance between the overall first and
;; overall last point is less than or eq to the sum of the sizes of
;; both ranges.
(define (overlaps? a1 a2)
  (<= (- (max (second a1) (second a2)) (min (first a1) (first a2)))
      (+ (- (second a1) (first a1))
         (- (second a2) (first a2)))))

(define (part1 inp)
  (count (λ(as) (apply contains? as)) inp))

(define (part2 inp)
  (count (λ(as) (apply overlaps? as)) inp))

(module+ main
  (define inp (read-inp))
  (part1 inp)
  (part2 inp))
