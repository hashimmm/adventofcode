#lang racket

(define inp (file->list "day9-input-1"))

(define (windowed size l)
  (let loop ([windows '()]
             [remaining l])
    (if (< (length remaining) size)
        (reverse windows)
        (loop (cons (take remaining size) windows) (rest remaining)))))
  

(define (find-non-matching preamble-size l)
  (for/or ([(window value) (in-parallel (in-list (windowed preamble-size l))
                                        (in-list (drop l preamble-size)))])
    (and (not (ormap (λ(combi)
                       (match combi
                         [(list a a) #f]
                         [(list a b) (= (+ a b) value)]))
                     (combinations window 2)))
         value)))

;; Instead of checking all windows, one can start with the full list's sum,
;; and whenever sum > target, drop from the right (the value on the right always
;; always has it's constituents to its left anyway) and whenever sum < target,
;; drop from the left and add back to the right.
;;
;; For that algo, it makes much more sense to use vectors and vector copy.

(define (find-contiguous target l)
  (for*/or ([size (in-range 2 (length l))]
            [window (in-list (windowed size l))])
      (and (= (apply + window) target)
           window)))

(define (find-weakness l)
  (define target (find-non-matching 25 l))
  (define window (find-contiguous target l))
  (+ (apply max window) (apply min window)))

(module+ test
  (require rackunit)
  (check-equal? (windowed 2 '(1 2 3 4)) '((1 2) (2 3) (3 4)))
  ;; Answers.
  (check-equal? (find-non-matching 25 inp) 25918798)
  (check-equal? (time (find-weakness inp)) 3340942))
