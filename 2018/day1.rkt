#lang racket

(define input (file->lines "input-day1"))

;; Given "N +" or "N -", get a function that will take M and return "N +/- M"
(define (prep-with-op num op)
  (cond [(equal? op #\+) (curry + num)]
        [(equal? op #\-) (curry - num)]
        [else (error (format "Bad operator ~s" op))]))

;; Part 1
(for/fold ([res 0])
          ([term (in-list input)])
  (define op (string-ref term 0))
  (define num (string->number (substring term 1)))
  ((prep-with-op res op) num))

;; Part 2
(for/fold ([res 0]
           [seen (set)]
           #:result res)
          ([term (in-cycle (in-list input))]
           #:break (set-member? seen res))
  (define op (string-ref term 0))
  (define num (string->number (substring term 1)))
  (define newval ((prep-with-op res op) num))
  (values newval (set-add seen res)))


(module+ test
  (require typed/rackunit)
  (check-equal? ((prep-with-op 3 #\-) 2) 1)
  (check-equal? ((prep-with-op 1 #\+) 2) 3))
