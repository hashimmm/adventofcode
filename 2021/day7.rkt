#lang racket

(define (parse input) (map string->number (string-split input ",")))

(define (find-required-fuel/1 target-pos inp)
  (apply + (map (λ(i) (abs (- i target-pos))) inp)))

(define (find-optimal-fuel-spend/1 inp)
  (apply min
         (for/list ([i (in-range (apply min inp) (apply max inp))])
           (find-required-fuel/1 i inp))))

(define (find-required-fuel/2 target-pos inp)
  (define (find-required-fuel-single target start)
    (let ([end (abs (- target start))]) (* (/ (add1 end) 2) end)))
  (apply + (map (λ(x) (find-required-fuel-single target-pos x))
                inp)))

(define (find-optimal-fuel-spend/2 inp)
  (apply min
         (for/list ([i (in-range (apply min inp) (apply max inp))])
           (find-required-fuel/2 i inp))))

(module+ test
  (require rackunit)
  (define test-input "16,1,2,0,4,2,7,1,2,14")
  (define inp (parse test-input))
  (check-equal? (find-optimal-fuel-spend/1 inp) 37)
  (check-equal? (find-optimal-fuel-spend/2 inp) 168))

(module+ main
  (define inp (parse (string-trim (with-input-from-file "day7.txt" port->string))))
  (find-optimal-fuel-spend/1 inp) #;356922
  (find-optimal-fuel-spend/2 inp))
