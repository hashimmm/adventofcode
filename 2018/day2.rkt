#lang racket

(define input (file->lines "day2-input"))

; Part 1

(define (checksum ids)
  (define-values
    (twos-score threes-score)
    (get-scores ids))
  (* twos-score threes-score))

(define (get-scores ids)
  (for/fold ([twos-score 0]
             [threes-score 0])
            ([x (in-list ids)])
    (values
     (if (has-exactly-two? x) (add1 twos-score) twos-score)
     (if (has-exactly-three? x) (add1 threes-score) threes-score))))

(define (has-exactly-two? id)
  (member 2 (hash-values (counts id))))

(define (has-exactly-three? id)
  (member 3 (hash-values (counts id))))

(define (counts id)
  (for/fold ([h (hash)])
            ([x (in-list (string->list id))])
    (hash-update h x add1 0)))

(checksum input)


; Part 2

(define (correct-boxes-common ids)
  (list->string (common-letters (correct-boxes ids))))

(define (correct-boxes ids)
  (for/last ([id-pair (combinations (map string->list ids) 2)])
    #:final (differ-by-one? id-pair)
    id-pair))

(define (differ-by-one? id-pair)
  (= 1
     (for/fold ([differs 0])
               ([x (in-list (first id-pair))]
                [y (in-list (second id-pair))])
       #:break (> differs 1)
       (if (equal? x y) differs (add1 differs)))))

(define (common-letters id-pair)
  (filter (curryr member (second id-pair)) (first id-pair)))

(correct-boxes-common input)
