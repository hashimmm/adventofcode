#lang racket


(require 2htdp/batch-io)

(define raw-input (map (compose (curry map string->number)
                                (curry regexp-split #px"\\W+")
                                string-trim)
                       (read-lines 'stdin)))

(define-values (input _)
  (for/fold ([points '()]
             [trio '(() () ())])
            ([line (in-list raw-input)])
    (define updated-trio
      (list (cons (first line) (first trio))
            (cons (second line) (second trio))
            (cons (third line) (third trio))))
    (cond [(= 3 (length (first updated-trio)))
           (values (append updated-trio points) '(() () ()))]
          [else
           (values points updated-trio)])))

(define (valid-triangle? points)
  (match-define (list a b c) points)
  (and ((+ a b) . > . c)
       ((+ a c) . > . b)
       ((+ b c) . > . a)))

#;(displayln input)
(count valid-triangle? input)
