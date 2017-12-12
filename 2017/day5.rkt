#lang racket

;; Wow part 2 took a while.

;; PART 1

(define (escape loi)
  (define (move pos steps loi)
    (cond [(or (< pos 0) (>= pos (length loi)))
           steps]
          [else
           (move
            (+ pos (list-ref loi pos))
            (+ 1 steps)
            (list-update loi pos (λ(x) (+ x 1))))]))
  (move 0 0 loi))


(module+ test
  (require rackunit)
  (check-eq? (escape '(0 3 0 1 -3)) 5))

(module+ main
  (require 2htdp/batch-io)
  (define loi
    (map string->number (read-lines "day5-input")))
  (escape loi))


;; PART 2

(define (escape2 loi)
  (define (move pos steps loi)
    (cond [(or (< pos 0) (>= pos (length loi)))
           steps]
          [else
           (move
            (+ pos (list-ref loi pos))
            (+ 1 steps)
            (list-update loi pos (λ(x) (if (< x 3) (+ x 1) (- x 1)))))]))
  (move 0 0 loi))

(module+ test
  (check-eq? (escape2 '(0 3 0 1 -3)) 10))

(module+ main
  (escape2 loi))