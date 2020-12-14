#lang racket

(define inp (file->lines "day5-input-1"))

(define (seat->row-col s)
  (define-values (row-lst col-lst) (split-at (string->list s) 7)) 
  (values (weird-binary-string->number row-lst #\B)
          (weird-binary-string->number col-lst #\R)))

(define (seat->rownum s)
  (let-values ([(x _) (seat->row-col s)]) x))

(define (weird-binary-string->number s-lst upper-char)
  (define upper-lim (expt 2 (length s-lst)))
  (define (mid x y) (/ (+ x y) 2))
  (define (fb->row lst upper lower)
    (cond [(null? lst) lower]
          [(char=? upper-char (first lst))
           (fb->row (rest lst) upper (mid upper lower))]
          [else
           (fb->row (rest lst) (mid upper lower) lower)]))
  (fb->row s-lst upper-lim 0))

(define (seat-id s)
  (let-values ([(r c) (seat->row-col s)])
    (+ (* 8 r) c)))

(define (find-missing id-list)
  ;; A binary search seems to be in the spirit of things,
  ;; but going through the list is so much easier.
  (define sorted (sort id-list <))
  (let loop ([cur (first sorted)]
             [remaining (rest sorted)])
    (if (not (= (first remaining) (add1 cur)))
        (add1 cur)
        (loop (first remaining) (rest remaining)))))

(module+ test
  (require rackunit)
  (check-equal? (seat->rownum "FBFBBFF") 44)
  (check-equal? (seat->rownum "FBFBBFB") 45)
  (check-equal? (seat-id "FBFBBFFRLR") 357)
  (check-equal? (seat-id "BFFFBBFRRR") 567)
  (check-equal? (seat-id "FFFBBBFRRR") 119)
  (check-equal? (seat-id "BBFFBBFRLL") 820)

  ;; Answers
  (check-equal? (apply max (map seat-id inp)) 901)
  (check-equal? (find-missing (map seat-id inp)) 661))