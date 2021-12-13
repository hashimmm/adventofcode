#lang racket

(define (parse inp)
  (define-values (dots folds)
    (splitf-at (filter non-empty-string? inp) (λ(s) (not (equal? #\f (string-ref s 0))))))
  (values (map (λ(x+y) (match-define (list n ...) (string-split x+y ","))
                 (map string->number n))
               dots)
          (map (λ(s)
                 (match s
                   [(regexp "fold along (.)=([0-9]+)" (list _ axis value))
                    (list axis (string->number value))]))
               folds)))

(define (fold-x line x+y)
  (match-define (list x y) x+y)
  (list (if (> line x)
            x
            (- x (* 2 (- x line))))
        y))

(define (fold-y line x+y)
  (match-define (list x y) x+y)
  (list x
        (if (> line y)
            y
            (- y (* 2 (- y line))))))

(define (do-fold dots fold)
  (define axis (first fold))
  (define value (second fold))
  (define foldf (if (equal? axis "x")
                    (λ(d) (fold-x value d))
                    (λ(d) (fold-y value d))))
  (remove-duplicates (map foldf dots)))

(define (fold/1 dots folds)
  (length (do-fold dots (first folds))))

(define (fold-all dots folds)
  (for/fold ([dots dots])
            ([fold (in-list folds)])
    (do-fold dots fold)))

(define (print-grid dots)
  (define max-x (add1 (apply max (map first dots))))
  (define max-y (add1 (apply max (map second dots))))
  (define dot-set (list->set dots))
  (define rows
    (for/list ([y (in-range max-y)])
      (list->string
       (for/list ([x (in-range max-x)])
         (if (set-member? dot-set (list x y)) #\# #\.)))))
  (string-join rows "\n"))

(module+ test
  (require rackunit)
  (define inp "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5")
  (define-values (dots folds) (parse (string-split inp "\n")))
  (check-equal? (fold/1 dots folds) 17)
  (displayln (print-grid (fold-all dots folds))))

(module+ main
  (define-values (dots folds) (parse (with-input-from-file "day13.txt" port->lines)))
  (fold/1 dots folds) #;818
  (displayln (print-grid (fold-all dots folds))) #;LRGPRECB )
