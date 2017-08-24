#lang racket

(require data/integer-set)

;; Racket data structure packages ftw ^_^
;; Btw one nice way to do this is to sort the lower and upper ranges separately
;; and zip the sorted lists to make new pairs and you can iterate over these for the same
;; results.
;; Fastest way is to probably just do everything in one iteration.

(define blacklist
  (for/fold ([is (make-range)])
            ([line (in-lines (open-input-file "day20-input"))])
    (define numbers (filter-map string->number (regexp-split "-" line)))
    (if (null? numbers)
        is
        (union is (apply make-range numbers)))))

(define whitelist
  (subtract (make-range 0 4294967295)
                        blacklist))

;; Part 1

(stream-first whitelist)

;; Part 2

(count whitelist)
