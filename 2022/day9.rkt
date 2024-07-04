#lang racket

(define (read-inp)
  (parse-lines (file->lines "day9.txt")))

(define (parse-lines lines)
  (map (λ(line) (match line [(regexp #rx"(.) ([0-9]+)" (list _ c n))
                             (cons c (string->number n))]))
       lines))

(define (move-range s e)
  (if (< s e) (range (add1 s) e) (reverse (range (add1 e) s))))

(define (move-head dir mag pos)
  (match-define (cons x y) pos)
  (match dir
    ["R" (cons (+ x mag) y)]
    ["L" (cons (- x mag) y)]
    ["U" (cons x (+ y mag))]
    ["D" (cons x (- y mag))]
    [x (error (string-append "bad direction" x))]))

(define (move-toward head-pos* tail-pos)
  (match-define (cons hx hy) head-pos*)
  (match-define (cons tx ty) tail-pos)
  (cond
    ;; distance <= 1, tail doesn't move
    [(or (and (= hx tx) (= hy ty))
         (and (= hx tx) (= 1 (abs (- hy ty))))
         (and (= hy ty) (= 1 (abs (- hx tx))))
         (and (= 1 (abs (- hy ty))) (= 1 (abs (- hx tx)))))
     #f]
    [else
     (define tx* (cond [(= hx tx) tx] [(< hx tx) (sub1 tx)] [else (add1 tx)]))
     (define ty* (cond [(= hy ty) ty] [(< hy ty) (sub1 ty)] [else (add1 ty)]))
     (cons tx* ty*)]))  

(define (tail-trail head-pos* tail-pos)
  (let loop ([now tail-pos])
    (let ([next (move-toward head-pos* now)])
      (if (not next)
          '()
          (cons next (loop next))))))

(define (follow-trail tail-pos trail)
  (reverse
   (let loop ([tail* tail-pos]
              [trail* '()]
              [remaining trail])
     (if (null? remaining)
         trail*
         (let ([next (move-toward (first remaining) tail*)])
           (loop (or next tail*)
                 (if next (cons next trail*) trail*)
                 (rest remaining)))))))

(define (part1 inp)
  (for/fold ([visited (set (cons 0 0))]
             [head-pos (cons 0 0)]
             [tail-pos (cons 0 0)]
             #:result (set-count visited))
            ([dir+mag inp])
    (match-define (cons dir mag) dir+mag)
    (define head-pos* (move-head dir mag head-pos))
    (define tail-posns (tail-trail head-pos* tail-pos))
    (define tail-pos* (if (null? tail-posns) tail-pos (last tail-posns)))
    (values (set-union visited (list->set tail-posns))
            head-pos*
            tail-pos*)))

(define (part2 inp)
  (for/fold ([visited (set (cons 0 0))]
             [knots-pos (build-list 10 (λ(_) (cons 0 0)))]
             #:result (set-count visited))
            ([dir+mag inp])
    (match-define (cons dir mag) dir+mag)
    (define head-pos* (move-head dir mag (first knots-pos)))
    (define first-trail (cons (second knots-pos) (tail-trail head-pos* (second knots-pos))))
    (define-values (knots-pos* last-trail)
      (let loop ([trail first-trail]
                 [remaining (rest (rest knots-pos))]
                 [collected '()])
        (if (null? remaining)
            (values (cons head-pos* (cons (last first-trail) (reverse collected))) trail)
            (let* ([next (first remaining)]
                   [trail* (cons next (follow-trail next trail))])
              (loop trail*
                    (rest remaining)
                    (cons (last trail*) collected))))))
    (values (set-union visited (list->set last-trail))
            knots-pos*)))

(module+ main
  (part1 (read-inp))
  (part2 (read-inp)))

(define (draw visited)
  (define height
    (+ (abs (apply max (map cdr visited)))
       (abs (apply min (map cdr visited)))))
  (define width
    (+ (abs (apply max (map car visited)))
       (abs (apply min (map car visited)))))
  (define x-offset (abs (apply min (map car visited))))
  (define y-offset (abs (apply min (map cdr visited))))
  (define vecs
    (build-vector
     (add1 height)
     (λ(_) (make-vector (add1 width) '_))))
  (for ([posn visited])
    (match-define (cons x y) posn)
    (vector-set! (vector-ref vecs (+ y-offset y)) (+ x-offset x) '$))
  (list->vector (reverse (vector->list vecs))))

(module+ test
  (require rackunit)
  (check-equal?
   (part1 (parse-lines (string-split "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2" "\n")))
   13)

  (check-equal?
   (part2 (parse-lines (string-split "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20" "\n")))
   36))
