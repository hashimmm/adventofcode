#lang racket

(define inp (file->lines "day12-input-1"))

;; I heard people doing this stuff easily with complex numbers, but I'd
;; forgotten most of that.
;;
;; On a little thinking, I figured that "sin" of something gives me the x coeff
;; and "cos" of something gives me the y coeff.
;; Simple enough this way too, except for rounding things.

(define (remainder! m n)
  (let loop ([v (abs m)])
    (if (>= v n)
        (loop (- v n))
        (if (< m 0) (* -1 v) v))))

(define (run-insts insts initial-dir)
  (define funs
    (hash "N" (λ(n b x y) (values x (+ y n) b))
          "S" (λ(n b x y) (values x (- y n) b))
          "E" (λ(n b x y) (values (+ x n) y b))
          "W" (λ(n b x y) (values (- x n) y b))
          "F" (λ(n b x y) (values (+ x (* n (sin (degrees->radians b))))
                                  (+ y (* n (cos (degrees->radians b))))
                                  b))
          "L" (λ(n b x y) (values x
                                  y
                                  (remainder! (- b n) 360)))
          "R" (λ(n b x y) (values x
                                  y
                                  (remainder! (+ b n) 360)))))
  (for/fold ([x 0]
             [y 0]
             [bearing initial-dir])
            ([inst (in-list insts)])
    (match-let ([(regexp #rx"([NSEWLRF])([0-9]+)" (list _ fun arg))
                 inst])
      (define n (string->number arg))
      (define f (hash-ref funs fun))
      (f n bearing x y))))

(define (run-insts-2 insts (init-x 10.0) (init-y 1.0))
  (define r360 (λ(x) (remainder! x 360)))
  (define funs
    (hash "N" (λ(n bx by x y b)
                (let ([new-by (+ by n)])
                  (values bx new-by x y
                          (r360 (radians->degrees (atan (/ bx new-by)))))))
          "S" (λ(n bx by x y b)
                (let ([new-by (- by n)])
                  (values bx new-by x y
                          (r360 (radians->degrees (atan (/ bx new-by)))))))
          "E" (λ(n bx by x y b)
                (let ([new-bx (+ bx n)])
                  (values new-bx by x y
                          (r360 (radians->degrees (atan (/ new-bx by)))))))
          "W" (λ(n bx by x y b)
                (let ([new-bx (- bx n)])
                  (values new-bx by x y
                          (r360 (radians->degrees (atan (/ new-bx by)))))))
          "F" (λ(n bx by x y b) (values bx by (+ x (* n bx)) (+ y (* n by)) b))
          "L" (λ(n bx by x y b) (let ([new-b (r360 (+ b (- 360 n)))]
                                      [mag (sqrt (+ (* bx bx) (* by by)))])
                                  (values (* mag (sin (degrees->radians new-b)))
                                          (* mag (cos (degrees->radians new-b)))
                                          x y new-b)))
          "R" (λ(n bx by x y b) (let ([new-b (r360 (+ b n))]
                                      [mag (sqrt (+ (* bx bx) (* by by)))])
                                  (values (* mag (sin (degrees->radians new-b)))
                                          (* mag (cos (degrees->radians new-b)))
                                          x y new-b)))))
  (for/fold ([bx init-x]
             [by init-y]
             [x 0]
             [y 0]
             [b (radians->degrees (atan (/ init-x init-y)))]
             #:result (+ (abs x) (abs y)))
            ([inst (in-list insts)])
    ;(displayln (list bx by x y b))
    (match-let ([(regexp #rx"([NSEWLRF])([0-9]+)" (list _ fun arg))
                 inst])
      (define n (string->number arg))
      (define f (hash-ref funs fun))
      (f n bx by x y b))))
#|
(run-insts-2
   '("F1" "R90" "F1" "R90" "F1" "R90" "F1") 0 1)
|#
(module+ test
  (require rackunit)
  (define test-inp (map symbol->string '(F10
                                         N3
                                         F7
                                         R90
                                         F11)))
  (define-values (tx ty tb) (run-insts test-inp 90))
  (check-= (round (+ (abs tx) (abs ty))) 25 0.001)
  (check-= (round (run-insts-2 test-inp)) 286 0.001)
  ;;
  ;; Answers
  ;;
  (define-values (x y b) (run-insts inp 90))
  (check-= (round (+ (abs x) (abs y))) 636 0.001))