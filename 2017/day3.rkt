#lang racket

;; PART 1

;; Maybe some combination of sqrt, /2, floor and ceiling
;; would've done, too?
(define (containing-square n)
  (define (iter-squares layer side-length)
    (if (>= (sqr side-length) n)
        (values layer side-length)
        (iter-squares (+ layer 1) (+ side-length 2))))
  (iter-squares 1 1))

(define (find-distance data-num)
  (define-values (layer side-length)
    (containing-square data-num))
  (define top (sqr side-length))
  (define distance-from-closest-corner
    (apply min (map (λ(x) (abs (- x data-num)))
                    (map (λ(x) (- top (* x (- side-length 1))))
                         (range 4)))))
  (+ (- layer 1) (- (floor (/ side-length 2))
                    distance-from-closest-corner)))

(module+ test
  (require rackunit)
;  (check-eq? (containing-square 1) (values 1 1))
;  (check-eq? (containing-square 2) (values 2 3))
;  (check-eq? (containing-square 3) (values 2 3))
;  (check-eq? (containing-square 9) (values 2 3))
;  (check-eq? (containing-square 10) (values 3 5))
;  (check-eq? (containing-square 25) (values 3 5))
;  (check-eq? (containing-square 26) (values 4 7))
  (check-eq? (find-distance 1) 0)
  (check-eq? (find-distance 12) 3)
  (check-eq? (find-distance 23) 2)
  (check-eq? (find-distance 1024) 31))

(module+ main
  (find-distance (string->number (read-line))))


;; PART 2

;; A few things...
;; Using a list of lists or some similar matrix type thingy may have been
;; much simpler (or not), but I specifically wanted to do it this way.
;;
;; Also, the function organization is quite poor here; I was somewhat stuck
;; between moving the inner functions out or not and in the end didn't. Moving
;; them out would mean some repeated calculations and I was wondering how
;; to either move them out but avoid those calculations, or find a way
;; to test inner functions.
;;
;; Didn't do either properly in the end.
;;
;; Lastly, the for loop in my original answer function wasn't breaking!
;; I'm not familiar with Racket enough to know why.
;; Anyway so for the time being I found the answer in the REPL
;; separately.

(define v (make-vector 314722))

(define (bottom-right sq)
  sq)
(define (bottom-left sq side-length)
  (- sq (* 1 (- side-length 1))))
(define (top-left sq side-length)
  (- sq (* 2 (- side-length 1))))
(define (top-right sq side-length)
  (- sq (* 3 (- side-length 1))))

(define (get-st-neighbors idx)
  (cond [(= 1 idx)
         (values 4 8 6 2)]
        [else
         (begin
           (define-values (layer side-length) (containing-square idx))
           (define square (sqr side-length))
           (define last-square (sqr (- side-length 2)))
           (define next-square (sqr (+ side-length 2)))
           (define sq-bottom-right (bottom-right square))
           (define sq-bottom-left (bottom-left square side-length))
           (define sq-top-right (top-right square side-length))
           (define sq-top-left (top-left square side-length))
           (define right-edge?
             (or (= square idx)
                 (<= (+ 1 last-square) idx sq-top-right)))
           (define left-edge?
             (<= sq-top-left idx sq-bottom-left))
           (define top-edge?
             (<= sq-top-right idx sq-top-left))
           (define bottom-edge?
             (<= sq-bottom-left idx sq-bottom-right))

           ;; The order of conditions is important in the following definitions
           ;; because the conditions actually overlap
           (define left
             (cond [left-edge?
                    (- (bottom-left next-square (+ 2 side-length))
                       (+ (- sq-bottom-left idx) 1))]
                   [top-edge?
                    (+ 1 idx)]
                   [bottom-edge?
                    (- idx 1)]
                   [right-edge?
                    (cond [(= (+ 1 last-square) idx)
                           last-square]
                          [else
                           (+ (sqr (- 4 side-length))
                              (- (- idx last-square) 1))])]))
           (define right
             (cond [(= idx square)
                    (+ 1 idx)]
                   [right-edge?
                    (+ square
                       (- idx last-square)
                       1)]
                   [bottom-edge?
                    (+ 1 idx)]
                   [top-edge?
                    (- idx 1)]
                   [left-edge?
                    (- (bottom-left last-square (- side-length 2))
                       (- sq-bottom-left idx)
                       -1)]))
           (define top
             (cond [(= square idx)
                    (+ 1 last-square)]
                   [top-edge?
                    (+ (+ square 1 side-length)  ;; next-square-upper-right
                       (- idx (+ last-square -1 side-length))  ;; distance-upper-right
                       1)]
                   [left-edge?
                    (- idx 1)]
                   [right-edge?
                    (+ idx 1)]
                   [bottom-edge?
                    (+ (- last-square (- side-length 3))  ;; last-square-bottom-left
                       (- idx (- square (- side-length 1)))  ;; distance-bottom-left
                       -1)]))
           (define bottom
             (cond [(= idx (+ 1 last-square))
                    square]
                   [(= idx square)
                    (- next-square 1)]
                   [bottom-edge?
                    (+ (bottom-left next-square (+ 2 side-length))  ;; next-square-bottom-left
                       (- idx (- square (- side-length 1)))  ;; distance-bottom-left
                       1)]
                   [left-edge?
                    (+ idx 1)]
                   [right-edge?
                    (- idx 1)]
                   [top-edge?
                    (+ (top-right last-square (- side-length 2))  ;; last-square-upper-right
                       (- idx (+ last-square -1 side-length))  ;; distance-upper-right
                       -1)]))
           (values top bottom left right))]))

(define (get-neighbors idx)
  (define-values (top bottom left right) (get-st-neighbors idx))
  (define bottom-right
    (let-values ([(t b l r) (get-st-neighbors bottom)])
      r))
  (define bottom-left
    (let-values ([(t b l r) (get-st-neighbors bottom)])
      l))
  (define top-left
    (let-values ([(t b l r) (get-st-neighbors top)])
      l))
  (define top-right
    (let-values ([(t b l r) (get-st-neighbors top)])
      r))
  (define neighbors
    (list top bottom left right top-left top-right bottom-left bottom-right))
  neighbors)

(vector-set! v 0 1)
(vector-set! v 1 1)

#;(define (data-gt num)
  (for/last ([i (in-range 2 (vector-length v))])
    #:final (> (vector-ref v i) num)
    (displayln (vector-ref v i))
    (vector-set! v i (foldl + 0 (map (curry vector-ref v) (get-neighbors i))))
    (vector-ref v i)))

(define (data-gt num)
  (for ([i (in-range 2 312051)])
    (vector-set! v i (foldl + 0 (map (curry vector-ref v) (get-neighbors i))))))

(module+ test
  (check-equal? (get-neighbors 1) '(4 8 6 2 5 3 7 9))
  (check-equal? (get-neighbors 2) '(3 9 1 11 4 12 8 10))
  (check-equal? (get-neighbors 3) '(14 2 4 12 15 13 1 11))
  (check-equal? (get-neighbors 4) '(15 1 5 3 16 14 6 2))
  (check-equal? (get-neighbors 5) '(16 6 18 4 17 15 19 1))
  (check-equal? (get-neighbors 6) '(5 7 19 1 18 4 20 8))
  (check-equal? (get-neighbors 7) '(6 22 20 8 19 1 21 23))
  (check-equal? (get-neighbors 8) '(1 23 7 9 6 2 22 24))
  (check-equal? (get-neighbors 9) '(2 24 8 10 1 11 23 25))
  (check-equal? (get-neighbors 10) '(11 25 9 27 2 28 24 26))
  (check-equal? (get-neighbors 11) '(12 10 2 28 3 29 9 27))
  (check-equal? (get-neighbors 12) '(13 11 3 29 14 30 2 28))
  (check-equal? (get-neighbors 13) '(32 12 14 30 33 31 3 29))
  (check-equal? (get-neighbors 14) '(33 3 15 13 34 32 4 12))
  (check-equal? (get-neighbors 15) '(34 4 16 14 35 33 5 3))
  (check-equal? (get-neighbors 16) '(35 5 17 15 36 34 18 4))
  (check-equal? (get-neighbors 17) '(36 18 38 16 37 35 39 5))
  (check-equal? (get-neighbors 18) '(17 19 39 5 38 16 40 6))
  (check-equal? (get-neighbors 19) '(18 20 40 6 39 5 41 7))
  (check-equal? (get-neighbors 20) '(19 21 41 7 40 6 42 22))
  (check-equal? (get-neighbors 21) '(20 44 42 22 41 7 43 45))
  (check-equal? (get-neighbors 22) '(7 45 21 23 20 8 44 46))
  (check-equal? (get-neighbors 23) '(8 46 22 24 7 9 45 47))
  (check-equal? (get-neighbors 24) '(9 47 23 25 8 10 46 48))
  (check-equal? (get-neighbors 25) '(10 48 24 26 9 27 47 49)))

;(module+ main
;  (data-gt 312051))
