#lang racket

(require srfi/13)
(require 2htdp/batch-io)

(struct disc (id posns start) #:transparent)

;; The following works because capsule reaches disc at time disc-id
(define (get-posn-at-reach d drop-time)
  (remainder (+ (disc-start d)
                (+ (disc-id d) drop-time))
             (disc-posns d)))

(define (parse-lines lines)
  (map (λ(l)
         (match-define (list _ id _ posns _ _ _ _ _ _ _ start) (regexp-split " " l))
         (disc (string->number (string-drop id 1))
               (string->number posns)
               (string->number (string-drop-right start 1))))
       lines))

(define discs (parse-lines (read-lines 'stdin)))


;; Part 1

(define-values (tried-t _)
  (for/fold ([try-t 0]
             [passed #f])
            ([_ (in-range 0 2 0)])
    #:break passed
    (define answer
      (andmap
       (λ(d)
         (= 0
            (get-posn-at-reach d try-t)))
       discs))
    (if answer
        (values try-t #t)
        (values (+ 1 try-t) #f))))

tried-t


;; Part 2
(define discs2 (append discs (list (disc (+ 1 (length discs)) 11 0))))

(define-values (tried-t2 __)
  (for/fold ([try-t 0]
             [passed #f])
            ([_ (in-range 0 2 0)])
    #:break passed
    (define answer
      (andmap
       (λ(d)
         (= 0
            (get-posn-at-reach d try-t)))
       discs2))
    (if answer
        (values try-t #t)
        (values (+ 1 try-t) #f))))

tried-t2
