#lang racket

(require 2htdp/batch-io)

(define instructions (read-lines 'stdin))
(define last-line (length instructions))
last-line
(define program-counter 0)

(define (update-registers registers PC)
  (define command (list-ref instructions PC))
  #;(displayln command)
  (match-define (list instruction args ...) (regexp-split " " command))
  (cond
    [(equal? instruction "jnz")
     (if (= 0 (if (string->number (first args))
                  (string->number (first args))
                  (hash-ref registers (first args))))
         (values registers (+ PC 1))
         (values registers (+ PC (string->number (second args)))))]
    [else
     (values
       (cond [(equal? instruction "cpy")
              (if (string->number (first args))
                  (hash-set registers (second args) (string->number (first args)))
                  (hash-set registers (second args) (hash-ref registers (first args))))]
             [(equal? instruction "inc")
              (hash-set registers (first args) (+ 1 (hash-ref registers (first args))))]
             [(equal? instruction "dec")
              (hash-set registers (first args) (- (hash-ref registers (first args)) 1))])
       (+ PC 1))]))

;(for/fold ([registers (hash "a" 0 "b" 0 "c" 0 "d" 0)]  ; P 1
(for/fold ([registers (hash "a" 0 "b" 0 "c" 1 "d" 0)]   ; P 2
           [PC program-counter])
          ([x (in-range 0 10 0)])
  #:break (PC . >= . last-line)
  #;(displayln PC)
  #;(displayln registers)
  (update-registers registers PC))
