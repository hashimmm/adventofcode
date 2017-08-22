#lang racket

;; Well basically you play around this one and realize that if there are n items
;; on a floor, it takes 2*(n-1)-1 steps to take them all to the floor above.
;; And then you have your answer.

;; (Which is basically 2 steps per bringing 1 thing up, -3 because the last 2
;; are brought up in 1 step)

;; The proper way to do this is via search tree

;; TODO

(define (item-print item port mode)
  (write-string (item-name item) port))

(struct item (name element type)
  #:methods gen:custom-write
  [(define write-proc item-print)])

(define (move x from to lols)
  (map (λ(sub-list enum)
         (cond [(= enum from)
                (remove x sub-list)]
               [(= enum to)
                (cons x sub-list)]
               [else sub-list]))
       lols
       (range (length lols))))

(define (move-up-1 x lols)
  (define from (length (takef lols (λ(sub) (not (member x sub))))))
  (if (= (length lols) (+ from 1))
      lols
      (move x from (+ from 1) lols)))

(define (move-down-1 x lols)
  (define from (length (takef lols (λ(sub) (not (member x sub))))))
  (if (= 0 from)
      lols
      (move x from (- from 1) lols)))

(define (move-up-many loi lols)
  (foldl move-up-1 lols loi))

(define (move-down-many loi lols)
  (foldl move-down-1 lols loi))

;; TODO
(define (next-step lols)
  lols)
  

(define (count-moves-to-top lols)
  (define (counter lols acc)
    (cond [(andmap empty? (take lols (- (length lols) 1)))
           acc]
          [else
           (counter (next-step lols)
                    (+ 1 acc))]))
  (counter lols 0))
          
;polonium generator, a thulium generator, a thulium-compatible microchip, a promethium generator,
; a ruthenium generator, a ruthenium-compatible microchip, a cobalt generator,
; and a cobalt-compatible microchip.
;The second floor contains a polonium-compatible microchip and a promethium-compatible microchip.

(define PoG (item "PoG" 'po 'g))
(define TG (item "TG" 't 'g))
(define TM (item "TM" 't 'm))
(define PrG (item "PrG" 'pr 'g))
(define RG (item "RG" 'r 'g))
(define RM (item "RM" 'r 'm))
(define PM (item "PM" 'p 'm))
(define CG (item "CG" 'c 'g))
(define CM (item "CM" 'c 'm))
(define PoM (item "PoM" 'po 'm))
(define PrM (item "PrM" 'pr 'm))
(define E (item "E" 'hashim 'elevator))

(define floor-map
  `((,PoG ,TG ,TM ,PrG ,RG ,RM ,CG ,CM ,E)
    (,PoM ,PrM)
    ()
    ()))


(define (game-print game port mode)
  (write-string (string-append "Moves: " (number->string (game-moves game)) "\n") port)
  (print (first (game-states game)) port))

(struct game (states moves)
  #:mutable
  #:methods gen:custom-write
  [(define write-proc game-print)])

(define my-game (game (list floor-map) 0))

(define (move-up x-or-loi)
  (cond [(list? x-or-loi)
         (set-game-states! my-game (cons (move-up-many x-or-loi (first (game-states my-game)))
                                         (game-states my-game)))
         (set-game-moves! my-game (+ 1 (game-moves my-game)))
         my-game]
        [else
         (set-game-states! my-game (cons (move-up-1 x-or-loi (first (game-states my-game)))
                                         (game-states my-game)))
         (set-game-moves! my-game (+ 1 (game-moves my-game)))
         my-game]))

(define (move-down x-or-loi)
  (cond [(list? x-or-loi)
         (set-game-states! my-game (cons (move-down-many x-or-loi (first (game-states my-game)))
                                         (game-states my-game)))
         (set-game-moves! my-game (+ 1 (game-moves my-game)))
         my-game]
        [else
         (set-game-states! my-game (cons (move-down-1 x-or-loi (first (game-states my-game)))
                                         (game-states my-game)))
         (set-game-moves! my-game (+ 1 (game-moves my-game)))
         my-game]))

(define (undo)
  (set-game-moves! my-game (- (game-moves my-game) 1))
  (set-game-states! my-game (rest (game-states my-game)))
  my-game)

