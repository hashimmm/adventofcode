#lang racket

(define (read-inp) (file->string "day6.txt"))

(define (no-dups? lst) (= (length lst) (length (remove-duplicates lst))))

(define (solve inp n)
  (define (acc remaining idx)
    (if (no-dups? (take remaining n))
        (+ idx n)
        (acc (rest remaining) (add1 idx))))
  (acc (string->list inp) 0))

;; alternative; uglier and faster
(define (solve2 inp n)
  (define cv (make-vector 26 0))
  (define iv (list->vector inp))
  (for/fold ([num 0])
            ([(c oi) (in-indexed (in-vector iv))])
    #:break (= (vector-count (λ(x) (= x 1)) cv) n)
    (define idx (- (char->integer c) 97))
    (vector-set! cv idx (add1 (vector-ref cv idx)))
    (when (>= oi n)
      (let ([old-idx (- (char->integer (vector-ref iv (- oi n))) 97)])
        (vector-set! cv old-idx (sub1 (vector-ref cv old-idx)))))
    (add1 num)))


(define (part1 inp) (solve inp 4))
(define (part2 inp) (solve inp 14))

(module+ main
  (part1 (read-inp))
  (part2 (read-inp))
  (define inp (string->list (read-inp)))
  (solve2 inp 4)
  (solve2 inp 14))
