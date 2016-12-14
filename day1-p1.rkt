#lang racket

(define rawinput "L2, L3, L3, L4, R1, R2, L3, R3, R3, L1, L3, R2, R3, L3, R4, R3, R3, L1, L4, R4, L2, R5, R1, L5, R1, R3, L5, R2, L2, R2, R1, L1, L3, L3, R4, R5, R4, L1, L189, L2, R2, L5, R5, R45, L3, R4, R77, L1, R1, R194, R2, L5, L3, L2, L1, R5, L3, L3, L5, L5, L5, R2, L1, L2, L3, R2, R5, R4, L2, R3, R5, L2, L2, R3, L3, L2, L1, L3, R5, R4, R3, R2, L1, R2, L5, R4, L5, L4, R4, L2, R5, L3, L2, R4, L1, L2, R2, R3, L2, L5, R1, R1, R3, R4, R1, R2, R4, R5, L3, L5, L3, L3, R5, R4, R1, L3, R1, L3, R3, R3, R3, L1, R3, R4, L5, L3, L1, L5, L4, R4, R1, L4, R3, R3, R5, R4, R3, R3, L1, L2, R1, L4, L4, L3, L4, L3, L5, R2, R4, L2")
;(define rawinput "R5, L5, R5, R3")
(define directions (map (lambda (x)
                          (match-define (pregexp #px"([RL])(\\d+)" (list _ dir num)) x)
                          (list (string-ref dir 0) (string->number num)))
                        (regexp-split ", " rawinput)))

(define turns
  (hash 'north (hash #\R 'east  #\L  'west)
        'south (hash #\R 'west  #\L  'east)
        'east  (hash #\R 'south #\L 'north)
        'west  (hash #\R 'north #\L 'south)))


(define (make-moves direction distances turns-and-moves)
  (cond [(null? turns-and-moves) distances]
        [else
         (let* ([next-turn (caar turns-and-moves)]
                [next-move (cadar turns-and-moves)]
                [new-direction (hash-ref (hash-ref turns direction) next-turn)]
                [new-distances (hash-update distances new-direction
                                            (lambda (x) (+ x next-move)))])
           (make-moves new-direction new-distances (rest turns-and-moves)))]))


(define final-distances (make-moves 'north (hash 'north 0 'south 0 'east 0 'west 0) directions))
(define final-distance (+ (abs (- (hash-ref final-distances 'north)
                                  (hash-ref final-distances 'south)))
                          (abs (- (hash-ref final-distances 'east)
                                  (hash-ref final-distances 'west)))))
