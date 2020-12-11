#lang racket/base

(require 2htdp/batch-io
         racket/list
         racket/string
         racket/match
         racket/bool)

(define lines (read-lines "day3-input-1"))

(define (path-ref path-pattern num)
  (define len (length path-pattern))
  (define 0-indexed (sub1 num))
  (define normalized-idx (remainder 0-indexed len))
  (list-ref path-pattern normalized-idx))

(define (tree-at-idx? path-pat idx)
  (char=? (path-ref path-pat idx) #\#))

(define (count-trees-on-slope path-pats)
  (count-trees-on-given-slope 3 1 path-pats))
;; Initial (working) impl for p1.
;  (for/fold ([count 0]
;             [cur-idx 1]
;             #:result count)
;            ([pat (in-list path-pats)])
;    (define next-idx (+ cur-idx 3))
;    (define pat-lst (string->list pat))
;    (if (tree-at-idx? pat-lst cur-idx)
;        (values (add1 count) next-idx)
;        (values count next-idx))))


(define (count-trees-on-given-slope slope-x slope-y path-pats)
  (for/fold ([count 0]
             [cur-idx-right 1]
             [cur-idx-down 0]
             #:result count)
            ([pat (in-list path-pats)])
    ;(displayln (list count cur-idx-right cur-idx-down))
    (define pat-lst (string->list pat))
    (cond [(= 0 (remainder cur-idx-down slope-y))
           (if (tree-at-idx? pat-lst cur-idx-right)
               (values (add1 count) (+ cur-idx-right slope-x) (add1 cur-idx-down))
               (values count (+ cur-idx-right slope-x) (add1 cur-idx-down)))]
          [else
           (values count cur-idx-right (add1 cur-idx-down))])))

(define sample-paths
  (list "..##......."
        "#...#...#.."
        ".#....#..#."
        "..#.#...#.#"
        ".#...##..#."
        "..#.##....."
        ".#.#.#....#"
        ".#........#"
        "#.##...#..."
        "#...##....#"
        ".#..#...#.#"))

(module+ test
  (require rackunit)
  (check-equal? (path-ref '(1 2 3) 1) 1)
  (check-equal? (path-ref '(1 2 3) 3) 3)
  (check-equal? (path-ref '(1 2 3) 4) 1)
  (check-equal? (path-ref '(1 2 3) 5) 2)
  (check-equal? (path-ref '(1 2 3) 6) 3)
  (check-equal? (path-ref '(1 2 3) 7) 1)

  (check-equal? (count-trees-on-slope sample-paths) 7)

  ;; Answers:
  (check-equal? (count-trees-on-slope lines) 211)
  (check-equal? 3584591857
                (*
                 (count-trees-on-given-slope  1 1 lines)
                 (count-trees-on-given-slope  3 1 lines)
                 (count-trees-on-given-slope  5 1 lines)
                 (count-trees-on-given-slope  7 1 lines)
                 (count-trees-on-given-slope  1 2 lines))))

