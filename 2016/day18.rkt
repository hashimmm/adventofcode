#lang racket

;; Not that we really needed this but I figured let's see if this helps
(require racket/performance-hint)

(define INPUT "^.^^^.^..^....^^....^^^^.^^.^...^^.^.^^.^^.^^..^.^...^.^..^.^^.^..^.....^^^.^.^^^..^^...^^^...^...^.")
; Part 1
;(define NUM-ROWS 40)
; Part 2
(define NUM-ROWS 400000)

(define tileset-length (string-length INPUT))

(begin-encourage-inline
  (define/match (gen-tile left center right)
    [(#\^ #\^ #\.) #\^]
    [(#\. #\^ #\^) #\^]
    [(#\^ #\. #\.) #\^]
    [(#\. #\. #\^) #\^]
    [(_ _ _) #\.]))

; Might be faster?
(define (gen-next-tileset ts)
  (map (λ(l c r)
         (gen-tile l c r))
       (cons #\. (drop-right ts 1))
       ts
       (rest (append ts '(#\.)))))

#;(define (gen-next-tileset ts)
    (map (λ(x y)
           (cond [(= y 0)
                  (gen-tile #\. (first ts) (second ts))]
                 [(= y (- (length ts) 1))
                  (gen-tile (list-ref ts (- y 1)) (list-ref ts y) #\.)]
                 [else
                  (gen-tile (list-ref ts (- y 1)) (list-ref ts y) (list-ref ts (+ y 1)))]))
         ts
         (range (length ts))))

(let loop ([dot-count 0]
           [rownum 0]
           [tileset (string->list INPUT)])
  (if (= rownum NUM-ROWS)
      dot-count
      (loop
       (+ dot-count (count (λ(x) (equal? #\. x)) tileset))
       (+ rownum 1)
       (gen-next-tileset tileset))))