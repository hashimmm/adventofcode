#lang racket

(require 2htdp/batch-io)

;(define input (regexp-split "\n" "ULL\nRRDDD\nLURDL\nUUUUD"))
(define input (read-lines 'stdin))

;; Make sure the input file doesn't have an extra new-line at the end!
;; Otherwise it repeats the last digit.

(struct key (val R L U D))
(define keypad
  (hash 1 (key 1 1 1 1 3)
        2 (key 2 3 2 2 6)
        3 (key 3 4 2 1 7)
        4 (key 4 4 3 4 8)
        5 (key 5 6 5 5 5)
        6 (key 6 7 5 2 'A)
        7 (key 7 8 6 3 'B)
        8 (key 8 9 7 4 'C)
        9 (key 9 9 8 9 9)
        'A (key 'A 'B 'A 6 'A)
        'B (key 'B 'C 'A 7 'D)
        'C (key 'C 'C 'B 8 'C)
        'D (key 'D 'D 'D 'B 'D)))

(define (move direction from)
  (cond [(equal? #\U direction)
         (key-U (hash-ref keypad from))]
        [(equal? #\D direction)
         (key-D (hash-ref keypad from))]
        [(equal? #\R direction)
         (key-R (hash-ref keypad from))]
        [(equal? #\L direction)
         (key-L (hash-ref keypad from))]))

(string-join
 (map ~s
      (reverse
       (foldl (lambda (line path)
                (cons
                 (foldl move (if (null? path) 5 (car path))
                        (string->list line))
                 path))
              '()
              input
              ))) "")
