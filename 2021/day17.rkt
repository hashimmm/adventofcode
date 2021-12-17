#lang racket

(define (simulate vx0 vy0 tx-min tx-max ty-min ty-max)
  (let loop ([x 0]
             [y 0]
             [highest-y 0]
             [vx vx0]
             [vy vy0])
    (cond [(> x tx-max) #f]
          [(and (= vx 0) (< x tx-min)) #f]
          [(< y ty-min) #f]
          [(and (>= x tx-min) (<= x tx-max)
                (>= y ty-min) (<= y ty-max))
           highest-y]
          [else
           (loop (+ x vx)
                 (+ y vy)
                 (if (> y highest-y) y highest-y)
                 (if (= 0 vx)
                     vx
                     (* (sub1 (abs vx)) (if (> vx 0) 1 -1)))
                 (sub1 vy))])))

;; Note:
;; Very un-scientific selection of y-range

(define (find-highest/1 tx-min tx-max ty-min ty-max)
  (apply max (filter identity
                     (for*/list ([vx (in-inclusive-range (- tx-max) tx-max)]
                                 [vy (in-inclusive-range ty-min (* 2 (abs ty-max)))])
                       (simulate vx vy tx-min tx-max ty-min ty-max)))))

(define (find-possibilities/2 tx-min tx-max ty-min ty-max)
  (define points
    (for*/list ([vx (in-inclusive-range (- tx-max) tx-max)]
                [vy (in-inclusive-range ty-min (* 2 (abs ty-max)))])
      (if (simulate vx vy tx-min tx-max ty-min ty-max)
          (cons vx vy)
          #f)))
  (filter identity points))

(module+ test
  (require rackunit)
  (check-equal? (find-highest/1 20 30 -10 -5) 45)
  (check-equal? (length (find-possibilities/2 20 30 -10 -5)) 112))

(module+ main
  (find-highest/1 235 259 -118 -62) ;6903
  (length (find-possibilities/2 235 259 -118 -62)) #;2351)
