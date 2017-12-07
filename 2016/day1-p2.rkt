#lang racket

;(define rawinput (read-line))
(define rawinput "R8, R4, R4, R8")

(define directions (map (lambda (x)
                          (match-define (pregexp #px"([RL])(\\d+)" (list _ dir num)) x)
                          (list dir (string->number num)))
                        (regexp-split ", " rawinput)))

(struct dir (sign axis) #:transparent)
(struct posn (x y) #:transparent)

(define turns
  (hash (dir + 'y) (hash "R" (dir + 'x) "L" (dir - 'x))
        (dir - 'y) (hash "R" (dir - 'x) "L" (dir + 'x))
        (dir + 'x) (hash "R" (dir - 'y) "L" (dir + 'y))
        (dir - 'x) (hash "R" (dir + 'y) "L" (dir - 'y))))


; TODO: make it efficient
(define-values (visited last-dn first-dup-pos)
  (for/fold ([visited (list (posn 0 0))]
             [last-dn (dir + 'y)]
             [dup #f])
            ([move (in-list directions)])
    #:break dup
    (match-define (list dir num) move)
    (define new-dn (hash-ref (hash-ref turns last-dn) dir))
    (define last (car visited))

    (define posn-adder
      (let ([sign (dir-sign new-dn)]
            [axis (dir-axis new-dn)])
        (cond [(equal? (dir-axis new-dn) 'x)
               (lambda (i)
                 (posn (sign (posn-x last) i)
                       (posn-y last)))]
              [else
               (lambda (i)
                 (posn (posn-x last)
                       (sign (posn-y last) i)))])))

    (define new-posns
      (for/list ([i (in-range num 0 -1)])
        (posn-adder i)))

    (let ([visited-now (append new-posns visited)])
      (values visited-now new-dn (check-duplicates visited-now)))))

(+ (abs (posn-x first-dup-pos)) (abs (posn-y first-dup-pos)))