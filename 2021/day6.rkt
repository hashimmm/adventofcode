#lang racket

(define test-input "3,4,3,1,2")

(define (parse inp)
  (map string->number (string-split inp ",")))

(define (grow-fish fishes)
  (define new-borns (count (λ(x) (equal? x 0)) fishes))
  (define new-ages (map (λ(x) (if (equal? x 0) 6 (sub1 x))) fishes))
  (append new-ages (make-list new-borns 8)))

(define (grow-fishes/1 fishes)
  (for/fold ([current-fishes fishes])
            ([_ (in-range 80)])
    (grow-fish current-fishes)))

(define (grow-fishes/2 fishes)
  (define age-counts-init (for/fold ([age-counts (hash)])
                                    ([fish-age (in-list fishes)])
                            (hash-update age-counts fish-age add1 0)))
  (for/fold ([age-counts age-counts-init]
             #:result (apply + (hash-values age-counts)))
            ([_ (in-range 256)])
    (for/hash ([age (in-range 9)])
      (values age
              (cond [(= age 8) (hash-ref age-counts 0 0)]
                    [(= age 6) (+ (hash-ref age-counts 7 0) (hash-ref age-counts 0 0))]
                    [else (hash-ref age-counts (add1 age) 0)])))))

(module+ test
  (require rackunit)
  (check-equal? (length (grow-fishes/1 (parse test-input))) 5934))

(module+ main
  (define real-input (filter identity (parse (with-input-from-file "day6.txt" port->string))))
  (length (grow-fishes/1 real-input))
  (time (grow-fishes/2 real-input)))
