#lang racket

(require srfi/1)

;; Part 1
(define (find-diffs input)
  (define with-nexts (zip input (rest input)))
  (count (λ(x) (> (second x) (first x))) with-nexts))

;; Part 2
(define (find-tri-sum-diffs input)
  (define triple-item-window (zip input (rest input) (rest (rest input))))
  (define triple-sums (map (λ(x) (apply + x)) triple-item-window))
  (define with-nexts (zip triple-sums (rest triple-sums)))
  (count (λ(x) (> (second x) (first x))) with-nexts))

(module+ test
  (require rackunit)
  (define input '(199
                  200
                  208
                  210
                  200
                  207
                  240
                  269
                  260
                  263))
  (check-equal? (find-diffs input) 7)
  (check-equal? (find-tri-sum-diffs input) 5))
