#lang racket

(define input (string->list (first (file->lines "day5-input"))))

(define (find-num-units polymer)
  (length (find-units polymer)))

(define (find-units polymer)
  (for/fold ([initial polymer])
            ([_ (in-naturals 1)])
    (define reduced (reduce-polymer initial))
    #:break (equal? reduced initial)
    reduced))

(define (reduce-polymer polymer)
  (define (reducer remaining)
    (cond [(null? remaining) remaining]
          [(null? (rest remaining)) remaining]
          [else
           (let ([x (first remaining)]
                 [y (second remaining)])
             (cond [(and (not (char=? x y)) (char-ci=? x y))
                    (reducer (rest (rest remaining)))]
                   [else
                    (cons x (reducer (rest remaining)))]))]))
  (reducer polymer))

(module+ test
  (require rackunit)
  (check-equal? (find-num-units (string->list "dabAcCaCBAcCcaDA")) 10))

;; part 1
(find-num-units input)

;; part 2

(define (num-shortest-by-removing polymer)
  (apply
   min
   (for/list ([type (in-list (all-types polymer))])
     (find-num-units (remove-type polymer type)))))

(define (all-types polymer)
  (remove-duplicates (map char-downcase polymer)))

(define (remove-type polymer type)
  (filter (λ(x) (not (char-ci=? x type))) polymer))

(num-shortest-by-removing input)
