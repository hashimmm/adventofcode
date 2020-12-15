#lang racket

(require racket/generator)

(define inp (file->lines "day8-input-1"))

(define (parse-instructions str-list)
  (for/vector ([line (in-list str-list)])
    (with-input-from-string line port->list)))

(define machine%
  (class* object% (writable<%>) (super-new)
    (init-field memory
                [accumulator 0]
                [position 0])
    (define/public (custom-display port)
      (fprintf port "|machine: ~a ~a|" accumulator position))
    (define/public (custom-write port)
      (fprintf port "|machine: ~s ~s|" accumulator position))
    (define/public (next)
      (define inst (vector-ref memory position))
      (match inst
        [(list 'acc arg)
         (new machine%
              [memory memory]
              [accumulator (+ accumulator arg)]
              [position (add1 position)])]
        [(list 'jmp arg)
         (new machine%
              [memory memory]
              [accumulator accumulator]
              [position (+ position arg)])]
        [(list 'nop arg)
         (new machine%
              [memory memory]
              [accumulator accumulator]
              [position (add1 position)])]))))

(define (terminated? machine)
  (= (get-field position machine) (vector-length (get-field memory machine))))

;(send (new machine% [memory (vector '(acc +1))]) next)

(define (alter-iter insts)
  (generator ()
             (for ([i (in-range (vector-length insts))])
               (define inst (vector-ref insts i))
               (match inst
                 [(list (and inst (or 'jmp 'nop)) x)
                  (define copy (vector-copy insts))
                  (vector-set! copy i (list (if (equal? inst 'nop) 'jmp 'nop) x))
                  (yield copy)]
                 [_ (void)]))))

(define (stop-at-repeat instructions)
  (let loop ([state (new machine% [memory instructions])]
             [seen '(0)])
    (define next-state (send state next))
    (define next-pos (get-field position next-state))
    (if (member next-pos seen)
        next-state
        (loop next-state (cons next-pos seen)))))

(define (stop-at-repeat-or-terminate instructions)
  (let loop ([state (new machine% [memory instructions])]
             [seen '(0)])
    (define next-state (send state next))
    (define next-pos (get-field position next-state))
    (if (or (member next-pos seen) (terminated? next-state))
        next-state
        (loop next-state (cons next-pos seen)))))

(define (try-alters instructions)
  (for/last ([new-insts (in-producer (alter-iter instructions))])
    (define stop-state (stop-at-repeat-or-terminate new-insts))
    #:final (terminated? stop-state)
    stop-state))

(module+ test
  (require rackunit)
  ;; Answers:
  (check-equal?
   (get-field accumulator (stop-at-repeat (parse-instructions inp)))
   1384)
  (check-equal?
   (get-field accumulator (try-alters (parse-instructions inp)))
   761))
