#lang racket

(define rawinput (read-line))
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
