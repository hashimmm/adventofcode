#lang racket

(define lines (file->lines "input-day1.txt"))

(define mass-list (map string->number lines))

(define fuel-required-per-module
  (map (λ(mass)
         (- (floor (/ mass 3)) 2))
       mass-list))

(define total-fuel-required
  (for/sum ([i (in-list fuel-required-per-module)]) i))

(define (calc-fuel-required mass acc)
  (define required-for-this-mass (- (floor (/ mass 3)) 2))
  (if (< required-for-this-mass 0)
      acc
      (calc-fuel-required required-for-this-mass (+ acc required-for-this-mass))))

(define fuel-required-per-module-2  
  (map (λ(mass)
         (calc-fuel-required mass 0))
       mass-list))

(define total-fuel-required-2
  (for/sum ([i (in-list fuel-required-per-module-2)]) i))
