#lang racket

(struct M #;move (n from to) #:transparent)

(define (read-inp)
  (define lines (file->lines "day5.txt"))
  (define-values (stack-raw-lines move-lines+blank)
    (splitf-at lines (λ(l) (not (equal? l "")))))
  (define move-lines (rest move-lines+blank))
  (define stack-lines (map string->list stack-raw-lines))
  ;; select column idx by looking for `numeric?` values in the last row,
  ;; then put all values of that column into a list,
  ;; filtering for `alphabetic?` to avoid spaces and the number itself
  (define stacks
    (for/vector ([(ch i) (in-indexed (last stack-lines))]
                 #:when (char-numeric? ch))
      (filter-map
       (λ(stack-line) (let ([char (list-ref stack-line i)])
                        (and (char-alphabetic? char) char)))
       stack-lines)))
  (define moves
    (map (λ(line)
           (apply M (map string->number (regexp-match* "[0-9]+" line)))) move-lines))
  (values stacks moves))

(define (exec stacks move (method reverse))
  (define-values (moving remaining)
    (split-at (vector-ref stacks (sub1 (M-from move)))
              (M-n move)))
  (vector-set! stacks (sub1 (M-from move)) remaining)
  (vector-set! stacks (sub1 (M-to move))
               (append (method moving) (vector-ref stacks (sub1 (M-to move))))))

(define (part1 stacks moves)
  (for ([move moves])
    (exec stacks move))
  (list->string (vector->list (vector-map first stacks))))

(define (part2 stacks moves)
  (for ([move moves])
    (exec stacks move identity))
  (list->string (vector->list (vector-map first stacks))))

(module+ main
  (define-values (stacks moves) (read-inp))
  (define stacks2 (vector-copy stacks))
  (part1 stacks moves)
  (part2 stacks2 moves))
