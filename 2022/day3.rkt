#lang racket

(define (read-inp) (map string->list (file->lines "day3.txt")))

(define (priority c)
  (define ci (char->integer c))
  (if (>= ci 97)
      (- ci 96)
      (- ci 38)))

(define (sum lst)
  (foldl + 0 lst))

(define (part1 inp)
  (define priorities (map (λ(chars) (map priority chars)) inp))
  (define commons
    (map (λ(sack)
           (define-values (l r) (split-at sack (/ (length sack) 2)))
           (define common (set-intersect (list->set l) (list->set r)))
           (sum (set->list common)))
         priorities))
  (sum commons))

(define (groups sacks)
  (if (null? sacks)
      '()
      (cons (take sacks 3)
            (groups (drop sacks 3)))))

(define (part2 inp)
  (define priorities (map (λ(chars) (map priority chars)) inp))
  (define grouped (groups priorities))
  (define commons
    (map (λ(g)
           (define sets (map list->set g))
           (sum (set->list (apply set-intersect sets))))
         grouped))
  (sum commons))

(module+ test
  (require rackunit)
  (check-equal?
   (list
    (priority #\a)
    (priority #\z)
    (priority #\A)
    (priority #\Z))
   '(1
     26
     27
     52)))

(part1 (read-inp))
(part2 (read-inp))
