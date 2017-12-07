#lang racket


(require racket/performance-hint)

;(define NUM-ELVES 5)
;(define NUM-ELVES 12)
;(define NUM-ELVES 64)
(define NUM-ELVES 3012210)



;; Part 1

(require data/queue)

(define elves1 (make-queue))
(for ([i (in-range 1 (+ 1 NUM-ELVES))])
  (enqueue! elves1 i))


(for ([i (in-range (- NUM-ELVES 2))])
  (define current-elf (dequeue! elves1))
  (dequeue! elves1)
  (enqueue! elves1 current-elf))

(dequeue! elves1)


;; Part 2

;; The following algorithm seems correct but way too slow even with the inlining
;; because of the HUGE number of iterations required.

;; So the actual part 2 is in another file.
#|
(struct elf (id active) #:transparent)
(define elves2 (build-vector NUM-ELVES (λ(x) (elf (add1 x) #t))))


(define-inline (goto-ref from-elf num)
  (let loop ([moved 0]
             [ref (- from-elf 1)])
    (define next-ref (remainder (+ 1 ref) NUM-ELVES))
    (cond
      [(= moved num)
       ref]
      [(elf-active (vector-ref elves2 next-ref))
       (loop (+ moved 1)
             next-ref)]
      [else
       (loop moved next-ref)])))

(for/fold ([current-elf 1])
          ([i (in-range (- NUM-ELVES 2))])
  (displayln current-elf)
  (define num-remaining (- NUM-ELVES i))
  (define steal-from-ref (goto-ref current-elf (exact-floor (/ num-remaining 2))))
  (vector-set! elves2 steal-from-ref (elf (+ steal-from-ref 1) #f))
  (+ 1 (goto-ref current-elf 1)))
|#