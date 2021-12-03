#lang racket

(require srfi/1)

;; Part 1

(define (power-consumption gamma epsilon)
  (* gamma epsilon))

(define (c->num c) (string->number (string c)))

(define (most-common-char chars)
  (if (< (apply + (map c->num chars))
         (/ (length chars) 2))
      #\0
      #\1))

(define (gamma-rate-binary input)
  (define transposed
    (apply zip (map string->list input)))
  (define digits
    (map (λ(chars) (most-common-char chars)) transposed))
  (string-append "#b" (list->string digits)))

(define (gamma-rate-decimal bin-rate)
  (with-input-from-string bin-rate read))

(define (epsilon-rate-decimal gamma-bin-rate)
  (define epsilon-rate-str
    (list->string
     (for/list ([c (in-string gamma-bin-rate)])
       (cond [(equal? c #\1) #\0]
             [(equal? c #\0) #\1]
             [else c]))))
  (with-input-from-string epsilon-rate-str read))

(define (power-consumption/1 input)
  (define gamma-rate-bin (gamma-rate-binary input))
  (define gamma (gamma-rate-decimal gamma-rate-bin))
  (define epsilon (epsilon-rate-decimal gamma-rate-bin))
  (power-consumption gamma epsilon))


;; Part 2

(define (nth-bits input n)
  (map (λ(x) (string-ref x n)) input))

(define (least-common-char char-list)
  (if (equal? (most-common-char char-list) #\1) #\0 #\1))

(define (find-rating criteria-maker input)
  (let loop ([idx 0]
             [remaining-inputs input])
    (if (<= (length remaining-inputs) 1)
        (first remaining-inputs)
        (let ([consider-str (nth-bits remaining-inputs idx)])
          (let ([criteria (criteria-maker consider-str idx)])
            (loop (add1 idx)
                  (filter criteria remaining-inputs)))))))

(define (oxygen-generator-cm char-list idx)
  (define common-digit (most-common-char char-list))
  (λ(s)
    (equal? (string-ref s idx) common-digit)))

(define (co2-generator-cm char-list idx)
  (define least-common-digit (least-common-char char-list))
  (λ(s)
    (equal? (string-ref s idx) least-common-digit)))

(define (find-oxygen-rating input)
  (find-rating oxygen-generator-cm input))

(define (find-co2-rating input)
  (find-rating co2-generator-cm input))

(define (power-consumption/2 input)
  (power-consumption
   (with-input-from-string (string-append "#b" (find-oxygen-rating input)) read)
   (with-input-from-string (string-append "#b" (find-co2-rating input)) read)))

(module+ test
  (require rackunit)
  (define input '("00100"
                  "11110"
                  "10110"
                  "10111"
                  "10101"
                  "01111"
                  "00111"
                  "11100"
                  "10000"
                  "11001"
                  "00010"
                  "01010"))
  (check-equal? (power-consumption/1 input) 198)
  ;; part 2
  (check-equal? (find-oxygen-rating input) "10111")
  (check-equal? (find-co2-rating input) "01010")
  (check-equal? (power-consumption/2 input) 230))

(module+ main
  (define input (port->lines (open-input-file "day3.txt")))
  (power-consumption/1 input) ;; 3277364
  (power-consumption/2 input) #;5736383 )
