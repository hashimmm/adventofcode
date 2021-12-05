#lang racket

(struct XY (x y) #:transparent)
(struct Line (xy1 xy2) #:transparent)

(define (Line-x1 l) (XY-x (Line-xy1 l)))
(define (Line-x2 l) (XY-x (Line-xy2 l)))
(define (Line-y1 l) (XY-y (Line-xy1 l)))
(define (Line-y2 l) (XY-y (Line-xy2 l)))

(define (parse input-lines)
  (for/list ([input-line (in-list input-lines)])
    (match input-line
      [(pregexp "(\\d+),(\\d+) -> (\\d+),(\\d+)" (list _ nums ...))
       (match (map string->number nums)
         [(list x1 y1 x2 y2)
          (Line (XY x1 y1) (XY x2 y2))])])))

(define (horiz-or-vert? line)
  (or (equal? (Line-x1 line) (Line-x2 line))
      (equal? (Line-y1 line) (Line-y2 line))))

(define (line->set l) ;; for horiz, vert and diag lines
  (define x-range-pt (sort (list (Line-x1 l) (Line-x2 l)) <))
  (define y-range-pt (sort (list (Line-y1 l) (Line-y2 l)) <))
  (define range-pt (XY (first x-range-pt) (first y-range-pt)))
  (define step (if (or (equal? range-pt (Line-xy1 l))
                       (equal? range-pt (Line-xy2 l)))
                   1
                   -1))
  (define y-range-pt-normalized (if (= step 1) y-range-pt (reverse y-range-pt)))
  (define x-range (if (equal? (first x-range-pt) (second x-range-pt))
                      (in-cycle (in-value (first x-range-pt)))
                      (in-inclusive-range (first x-range-pt) (second x-range-pt))))
  (define y-range (if (equal? (first y-range-pt-normalized) (second y-range-pt-normalized))
                      (in-cycle (in-value (first y-range-pt-normalized)))
                      (in-inclusive-range (first y-range-pt-normalized) (second y-range-pt-normalized) step)))
  (for/set ([ix x-range]
            [iy y-range])
    (XY ix iy)))

;; Part 1
(define (count-overlapping-pts/1 lines)
  (for/fold ([all-points (set)]
             [overlapping-pts (set)]
             #:result (set-count overlapping-pts))
            ([line (in-list lines)]
             #:when (horiz-or-vert? line))
    (define new-line-set (line->set line))
    (define new-overlaps (set-intersect all-points new-line-set))
    (values (set-union all-points new-line-set)
            (set-union overlapping-pts new-overlaps))))

;; Part 2
(define (count-overlapping-pts/2 lines)
  (for/fold ([all-points (set)]
             [overlapping-pts (set)]
             #:result (set-count overlapping-pts))
            ([line (in-list lines)])
    (define new-line-set (line->set line))
    (define new-overlaps (set-intersect all-points new-line-set))
    (values (set-union all-points new-line-set)
            (set-union overlapping-pts new-overlaps))))

(module+ test
  (require rackunit)
  (define input
    "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2")
  (define lines (parse (with-input-from-string input port->lines)))
  (check-equal? (count-overlapping-pts/1 lines) 5)
  (check-equal? (count-overlapping-pts/2 lines) 12))

(module+ main
  (define lines (parse (with-input-from-file "day5.txt" port->lines)))
  (time (count-overlapping-pts/1 lines)) ; 6572
  (time (count-overlapping-pts/2 lines)) #;21466 )
