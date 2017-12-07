#lang racket

(require srfi/1)

;; PART 1                                                            

(define (char->number c) (- (char->integer c) 48))

(define (cyclic-windowed lst n)
  (define (windower accum remaining first-n)
    (cond [(null? remaining) accum]
          [(< (length remaining) n)
           (define extra (- n (length remaining)))
           (windower (cons (append remaining
                                   (take first-n extra))
                           accum)
                     (rest remaining)
                     first-n)]
          [else
           (windower (cons (take remaining n) accum)
                     (rest remaining)
                     first-n)]))
  (reverse (windower '() lst (take lst n))))

(define (advent-p1 str)
  (apply +
         (map (λ(w) (if (= (first w) (second w)) (first w) 0))
              (cyclic-windowed (map char->number
                                    (string->list str))
                               2))))

(module+ test
  (require rackunit)
  (check-eq? (advent-p1 "1122") 3)
  (check-eq? (advent-p1 "1111") 4)
  (check-eq? (advent-p1 "1234") 0)
  (check-eq? (advent-p1 "91212129") 9))

(module+ main
  (define puzzle-input (read-line))
  (printf "Answer, part 1: ~a\n" (advent-p1 puzzle-input)))

; PART 2

(define (advent-p2 str)
  (define lst (map char->number (string->list str)))
  (define len (length lst))
  (define-values (left right) (split-at lst (/ len 2)))
  (define cycled (append right left))
  (apply + (map (λ(x) (if (= (first x) (second x)) (first x) 0))
                (zip lst cycled))))

(module+ test
  (check-eq? (advent-p2 "1212") 6)
  (check-eq? (advent-p2 "1221") 0)
  (check-eq? (advent-p2 "123425") 4)
  (check-eq? (advent-p2 "123123") 12)
  (check-eq? (advent-p2 "12131415") 4))

(module+ main
  (printf "Answer, part 2: ~a\n" (advent-p2 puzzle-input)))

