#lang racket/base

(require 2htdp/batch-io
         racket/list
         racket/string
         racket/match
         racket/bool)

(define lines (read-lines "day2-input-1"))

(define (contains-bw s x n1 n2)
  (define chars (string->list s))
  (define num-found (count (λ(v) (char=? v x)) chars))
  (and (>= num-found n1)
       (<= num-found n2)))

(define (valid-pw? pol+str)
  (match-define (list pol str) (string-split pol+str ": "))
  (match-define (list rng ltr) (string-split pol " "))
  (match-define (list mns mxs) (string-split rng "-"))
  (define chr (first (string->list ltr)))
  (define mn (string->number mns))
  (define mx (string->number mxs))
  (contains-bw str chr mn mx))

(define (has-at-one-of s x n1 n2)
  (define loc (string->list s))
  (xor (and (>= (length loc) n1) (char=? (list-ref loc (sub1 n1)) x))
       (and (>= (length loc) n2) (char=? (list-ref loc (sub1 n2)) x))))

(define (valid-pw-2? pol+str)
  (match-define (list pol str) (string-split pol+str ": "))
  (match-define (list rng ltr) (string-split pol " "))
  (match-define (list mns mxs) (string-split rng "-"))
  (define chr (first (string->list ltr)))
  (define mn (string->number mns))
  (define mx (string->number mxs))
  (has-at-one-of str chr mn mx))

(define (count-valid lo-pol+str)
  (count valid-pw? lo-pol+str))

(define (count-valid-2 lo-pol+str)
  (count valid-pw-2? lo-pol+str))


(module+ test
  (require rackunit)
  (check-true (contains-bw "sass" #\s 1 3))
  (check-true (contains-bw "asdd" #\s 1 3))
  (check-false (contains-bw "qwer" #\s 1 3))
  (check-false (contains-bw "ssss" #\s 1 3))

  ;; AOC answers:
  (check-equal? (count-valid lines) 546)
  (check-equal? (count-valid-2 lines) 275))

