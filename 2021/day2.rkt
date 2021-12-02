#lang racket

(define (parse-input list-of-strings)
  (map (λ(line) (let ([parts (string-split line)])
                  (list (first parts) (string->number (second parts)))))
       list-of-strings))

;; Part 1
(define (depth*horiz/1 instructions)
  (for/fold ([depth 0]
             [horiz 0]
             #:result (* depth horiz))
            ([inst (in-list instructions)])
    (match-define (list action amt) inst)
    (cond [(equal? action "forward") (values depth (+ horiz amt))]
          [(equal? action "down") (values (+ depth amt) horiz)]
          [(equal? action "up") (values (- depth amt) horiz)])))

;; Part 2
(define (depth*horiz/2 instructions)
  (for/fold ([aim 0]
             [depth 0]
             [horiz 0]
             #:result (* depth horiz))
            ([inst (in-list instructions)])
    (match-define (list action amt) inst)
    (cond [(equal? action "forward") (values aim (+ depth (* aim amt)) (+ horiz amt))]
          [(equal? action "down") (values (+ aim amt) depth horiz)]
          [(equal? action "up") (values (- aim amt) depth horiz)])))

(module+ test
  (require rackunit)
  (define input '("forward 5"
                  "down 5"
                  "forward 8"
                  "up 3"
                  "down 8"
                  "forward 2"))
  (define parsed (parse-input input))
  (check-equal? (depth*horiz/1 parsed) 150)
  (check-equal? (depth*horiz/2 parsed) 900))

(module+ main
  (define input (port->lines (open-input-file "day2.txt")))
  (define parsed (parse-input input))
  (depth*horiz/1 parsed)
  (depth*horiz/2 parsed))
