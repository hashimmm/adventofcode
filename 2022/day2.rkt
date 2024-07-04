#lang racket

(define (read-inp) (file->lines "day2.txt"))
(define-values (ROCK PAPER SCISSORS) (values 'rock 'paper 'scissors))

(define (translate move)
  (match move
    ["A" ROCK]
    ["B" PAPER]
    ["C" SCISSORS]
    ["X" ROCK]
    ["Y" PAPER]
    ["Z" SCISSORS]))

(define (score them me)
  (define result-score (play them me))
  (define shape-score (hash-ref (hash ROCK 1 PAPER 2 SCISSORS 3) me))
  (+ result-score shape-score))

(define (get-score them result)
  (define result-score (hash-ref (hash "X" 0 "Y" 3 "Z" 6) result))
  (define shape-score
    (match result
      ["X" (hash-ref (hash ROCK 3 PAPER 1 SCISSORS 2) them)]
      ["Y" (hash-ref (hash ROCK 1 PAPER 2 SCISSORS 3) them)]
      ["Z" (hash-ref (hash ROCK 2 PAPER 3 SCISSORS 1) them)]))
  (+ result-score shape-score))

(define (play them me)
  (cond [(draw? me them) 3]
        [(beats? me them) 6]
        [else 0]))

(define (draw? me them) (equal? me them))

(define (beats? me them)
  (cond
    [(and (equal? me ROCK) (equal? them SCISSORS)) #t]
    [(and (equal? me PAPER) (equal? them ROCK)) #t]
    [(and (equal? me SCISSORS) (equal? them PAPER)) #t]
    [else #f]))

(define (sum lst)
  (foldl + 0 lst))

(define (part1 game-list)
  (sum (map (λ(line)
              (let ([hands (string-split line)])
                (apply score (map translate hands))))
            game-list)))

(define (part2 game-list)
  (sum (map (λ(line)
              (match-define (list them result) (string-split line))
              (get-score (translate them) result))
            game-list)))

(module+ test
  (require rackunit)
  (check-equal?
   (part1 '("A Y"
            "B X"
            "C Z")) 15)
  (check-equal?
   (part2 '("A Y"
            "B X"
            "C Z")) 12))

(module+ main
  (define inp (read-inp))
  (part1 inp)
  (part2 inp))
