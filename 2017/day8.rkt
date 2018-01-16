#lang racket

(define != (λ(x y) (not (= x y))))

(define cmp-map
  (hash ">" >
        "<" <
        ">=" >=
        "<=" <=
        "!=" !=
        "==" =))
        
(define op-map
  (hash "inc" +
        "dec" -))

(define registers (make-hash))

(define (run-inst inst-line)
  (match-define (list reg op amount _ other-reg cmp other-amount)
    (string-split inst-line))
  (when ((hash-ref cmp-map cmp)
         (hash-ref! registers other-reg 0)
         (string->number other-amount))
    (hash-update! registers
                  reg
                  (λ(x)
                    ((hash-ref op-map op)
                     x
                     (string->number amount)))
                  0)))

(define (max-val)
  (apply max (hash-values registers)))

(module+ test
  (require rackunit)
  (define test-input "b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10")
  (define max-ever
    (for/fold ([max-ever 0])
              ([line (in-list (string-split test-input "\n"))])
      (run-inst line)
      (define new-max (max-val))
      (if (> new-max max-ever)
          new-max
          max-ever)))
  (check-equal? (hash-ref registers "a") 1)
  (check-equal? (hash-ref registers "c") -10)
  (check-equal? (max-val) 1)
  (check-equal? max-ever 10))

(module+ main
  (require 2htdp/batch-io)
  (define input (read-lines "day8-input"))
  (define max-ever
    (for/fold ([max-ever 0])
              ([line (in-list input)])
      (run-inst line)
      (define new-max (max-val))
      (if (> new-max max-ever)
          new-max
          max-ever)))
  (max-val)
  max-ever)
