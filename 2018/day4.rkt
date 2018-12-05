#lang racket

(require srfi/19)
(require math/statistics)
(require racket/trace)
(require (prefix-in rd: racket/date))

;; Really interesting thing with hash values...
;; It returns key/value pairs, so if value is a list,
;; then the key value pair is a proper list,
;; where the first item is the key and rest is the value!
;; This bit us only in part 2, we got by in part 1 because
;; of the fact that we were using mode.
;; In part 2 we're still using mode but we got a runtime type error.

(define input (file->lines "day4-input"))

(define (mode xs)
  (if (null? xs)
      #f
      (car (first (sort (hash->list (samples->hash xs))
                        (λ(x y) (> (cdr x) (cdr y))))))))

(define (mode-with-count xs)
  (if (null? xs)
      #f
      (first (sort (hash->list (samples->hash xs))
                   (λ(x y) (> (cdr x) (cdr y)))))))

(define (find-most-asleep-min guard guard-map)
  (mode (hash-ref guard-map guard)))

(define (most-asleep-guard guard-map)
  (car
   (first
    (sort (hash->list guard-map)
          (λ(x y)
            (> (length (cdr x)) (length (cdr y))))))))

(define (build-guard-map input)
  (define date-parsed-input (parse-dates input))
  (define sorted-input (sort-input-by-time date-parsed-input))
  (for/fold ([guard-map (hash)]
             [current-guard #f]
             #:result guard-map)
            ([date-and-line sorted-input])
    (define tag (substring (second date-and-line) 19 22))
    (define new-guard
      (if (equal? tag "Gua")
          (string->number (first
                           (regexp-match
                            #rx"[0-9]+"
                            (substring (second date-and-line) 26))))
          current-guard))
    (define current-min
      (date-minute (first date-and-line)))
    (define updated-mapping
      (cond [(equal? tag "wak")
             (hash-update guard-map
                          new-guard
                          (λ(x) (rem-one-each (range current-min 60) x))
                          '())]
            [(equal? tag "fal")
             (hash-update guard-map
                          new-guard
                          (λ(x) (append x (range current-min 60)))
                          '())]
            [else guard-map]))
    (values
     updated-mapping
     new-guard)))

(define (rem-one-each of-these from)
  (foldl (λ(x ret) (remove x ret))
         from
         of-these))

(define (parse-dates input)
  (for/list ([line (in-list input)])
    (define date
      (string->date
       (substring line 1 17)
       "~Y-~m-~d ~H:~M"))
    (list date line)))

(define (sort-input-by-time date-parsed-input)
  (sort date-parsed-input (lambda (x y)
                            (< (rd:date->seconds (first x)) (rd:date->seconds (first y))))))

(define guard-map
  (build-guard-map input))

;; Part 1
(define sleepyhead (most-asleep-guard guard-map))
(* sleepyhead
   (find-most-asleep-min
    sleepyhead
    guard-map))

;; Part 2
(define (most-slept-guard-minute-combo guard-map)
  (define guard-min-modes
    (filter-map (λ(x) (list (first x) (mode-with-count (rest x))))
                (hash->list guard-map)))
  (first
   (sort guard-min-modes
         (λ(x y) (> (cdr (second x)) (cdr (second y)))))))

(define sleepy-combo
  (most-slept-guard-minute-combo guard-map))

(* (first sleepy-combo) (car (second sleepy-combo)))
