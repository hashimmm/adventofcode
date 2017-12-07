#lang racket

;; Literally a one-character change from p1.
;; Makes me think maybe I shouldn't be copy-pasting.

;; Basically, we have a hash of hashes, "positions", and on each character,
;; we use the current column number, "index", to update positions' index' char-counts.

; Given a character, get a function that takes a hash
; and updates the count of that character in that hash
(define (count-updater c)
  (λ(x) (hash-update x c (λ(y) (+ y 1)) 0)))

; Fold over lines, accumulate into positions: a hash of column-numbers to char-counts
; char-counts are also hashes: of chars to counts
(define positions
 (sequence-fold
  (λ(positions line)
    (define locs (string->list line))
    ; Fold over current line + enumeration,
    ; for each char get a hash-updater,
    ; and use it to update positions' char-counts at the current enumeration.
    (foldl (λ(char index positions)
             (hash-update positions index (count-updater char) (hash)))
           positions
           locs (range (length locs))))
  (hash)
  (in-port read-line)))


;; Then we just iterate over the enumerated keys in order and sort for highest count
;; and get the topmost of each
(define pair-sorter
  (λ(x y)
    (cond [(= (cdr x) (cdr y)) (char<? (car x) (car y))]
          [else (< (cdr x) (cdr y))])))

(map first
     (map (λ(x) (sort (hash->list (hash-ref positions x)) pair-sorter))
          (range (hash-count positions))))
