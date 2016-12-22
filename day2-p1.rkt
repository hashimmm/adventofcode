#lang racket

(require 2htdp/batch-io)

;(define input "ULL\nRRDDD\nLURDL\nUUUUD")
(define input (read-lines 'stdin))

;; Make sure the input file doesn't have an extra new-line at the end!
;; Otherwise it repeats the last digit.

(define (move direction from)
  (cond [(equal? #\R direction)
         (if (member from '(3 6 9))
             from
             (+ from 1))]
        [(equal? #\L direction)
         (if (member from '(1 4 7))
             from
             (- from 1))]
        [(equal? #\U direction)
         (if (member from '(1 2 3))
             from
             (- from 3))]
        [(equal? #\D direction)
         (if (member from '(7 8 9))
             from
             (+ from 3))]))


(string-join
 (map number->string
      (reverse
       (foldl (lambda (line initials)
                (cons
                 (foldl move (if (null? initials) 5 (car initials))
                        (string->list line))
                 initials))
              '()
              ;(regexp-split "\n" input)  ;; read-lines already gives us a list
              input
              ))) "")