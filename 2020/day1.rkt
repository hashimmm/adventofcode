#lang racket/base

(require 2htdp/batch-io
         racket/list)

(define inp (read-lines "day1-input-1"))
(define numbers (map string->number inp))

(define (find-2-that-sum-to N)
  (let loop ([complements '()]
             [remaining numbers])
    (and (not (null? remaining))
         (let* ([num (first remaining)]
                [found (member num complements)])
           (if found
               (* num (- N num))
               (loop (cons (- N num) complements)
                     (rest remaining)))))))

(define (find-3-that-sum-to N)
  (let loop ([remaining numbers])
    (let* ([num (first remaining)]
           [target (- N num)]
           [found-prod (find-2-that-sum-to target)])
      (cond [found-prod
             (* num found-prod)]
            [(null? remaining) #f]
            [else
             (loop (rest remaining))]))))

(module+ test
  (require rackunit)

  ;; Post-hoc tests. (i.e. the impl. is already checked by AOC, this is
  ;; simply recording answers and useful only if we wish to change the impl.
  (check-equal? (find-2-that-sum-to 2020) 878724)
  (check-equal? (find-3-that-sum-to 2020) 201251610))
