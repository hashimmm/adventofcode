#lang racket

(define (vector-slice v start end)
  (for/vector ([i (in-range start end)])
    (vector-ref v i)))

(define (vector-andmap lam v)
  (andmap lam (vector->list v)))
(define (vector-ormap lam v)
  (ormap lam (vector->list v)))

(define (parse input)
  (define lines (list->vector input))
  (define draws (map string->number (string-split (vector-ref lines 0) ",")))
  (define board-str
    (for/vector ([i (in-range (/ (sub1 (vector-length lines)) 6))])
      (let* ([start (+ (* i 5) i 2)]
             [end (+ start 5)])
        (vector-slice lines start end))))
  (define boards
    (vector-map (λ(table)
                  (vector-map (λ(s)
                                (list->vector (map string->number (string-split (string-trim s)))))
                              table))
                board-str))
  (values draws boards))

(struct m (n) #:transparent)
(define not-marked? (λ(x) (not (m? x))))
(define (transpose table)
  (for/vector ([i (in-range (vector-length (vector-ref table 0)))])
    (vector-map (λ(row) (vector-ref row i)) table)))
(define (play-draw number boards)
  (for/vector ([board (in-vector boards)])
    (vector-map (λ(row) (vector-map (λ(n) (if (equal? n number) (m n) n)) row)) board)))
(define (won? board)
  (and (or (vector-ormap (λ(row) (vector-andmap m? row)) board)
           (vector-ormap (λ(row) (vector-andmap m? row)) (transpose board)))
       board))
(define (which-won-first? boards)
  (for/or ([board (in-vector boards)])
    (won? board)))
(define (which-won-all? boards)
  (vector-filter won? boards))
(define (winning-total board draw)
  (* draw
     (for/sum ([row (in-vector board)])
       (apply + (filter not-marked? (vector->list row))))))

;; Part 1
(define (winning-board/1 draws boards)
  (let loop ([remaining-draws draws]
             [new-boards boards])
    (define current-draw (first remaining-draws))
    (define next-boards (play-draw current-draw new-boards))
    (define won (which-won-first? next-boards))
    (if won (winning-total won current-draw) (loop (rest remaining-draws) next-boards))))

;; Part 2
(define debug (λ(x) (begin (displayln x) x)))
(define (winning-board/2 draws boards)
  (let loop ([remaining-draws draws]
             [new-boards boards]
             [last-winning-total #f])
    (cond [(null? remaining-draws) last-winning-total]
          [(null? new-boards) last-winning-total]
          [else
           (let* ([current-draw (first remaining-draws)]
                  [next-boards (play-draw current-draw new-boards)]
                  [winning-boards (which-won-all? next-boards)])
             (loop (rest remaining-draws)
                   (vector-filter (λ(b) (not (vector-member b winning-boards))) next-boards)
                   (or (and (not (vector-empty? winning-boards))
                            (winning-total (vector-ref (vector-take-right winning-boards 1) 0) current-draw))
                       last-winning-total)))])))

(module+ test
  (require rackunit)
  (define input
    "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7")
  (define-values (draws boards) (parse (with-input-from-string input port->lines)))
  (check-equal? (winning-board/1 draws boards) 4512)
  (check-equal? (winning-board/2 draws boards) 1924))

(module+ main
  (define-values (draws boards) (parse (with-input-from-file "day4.txt" port->lines)))
  (time (winning-board/1 draws boards)) ;; 16674
  (time (winning-board/2 draws boards)) #;7075 )
