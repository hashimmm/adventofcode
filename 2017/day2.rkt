#lang racket

;; PART 1

(define (calc-checksum input)
  (define differences
    (for/list ([row input])
      (- (apply max row) (apply min row))))
  (apply + differences))

(module+ main
  (define input
    (for/list ([l (in-lines (open-input-file "day2-input"))])
      (map string->number (regexp-split "\t" l))))
  (displayln "Part 1: ")
  (calc-checksum input))

;; PART 2

(define (divisible-pair? x)
  (or (= 0 (remainder (first x) (second x)))
      (= 0 (remainder (second x) (first x)))))

(define (calc-checksum2 input)
  (define divisible-divisions
    (for/list ([row input])
      (define div-pair
        (first (filter divisible-pair? (combinations row 2))))
      (if (> (first div-pair) (second div-pair))
          (/ (first div-pair) (second div-pair))
          (/ (second div-pair) (first div-pair)))))
  (apply + divisible-divisions))

(module+ main
  (displayln "Part 2: ")
  (calc-checksum2 input))
