#lang racket


(require 2htdp/batch-io)

(define input (read-lines 'stdin))

(define (valid-triangle? points)
  (match-define (list a b c) points)
  (and ((+ a b) . > . c)
       ((+ a c) . > . b)
       ((+ b c) . > . a)))

(define (to-nums s)
  (map string->number (regexp-split #px"\\W+" (string-trim s))))

(count (compose valid-triangle? to-nums) input)
