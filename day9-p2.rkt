#lang racket

;; If you remove the last loop and have a look at the "compression-tree"
;; you'll immediate grasp what's happening. Kinda simple in the end.

;; For an even better idea, change the "+" of unmarked-len into string-append,
;; and the 0 into "".

;; Also, due to the algo, marker-amt is always the same as the decompressed length of
;; marker-ctree, so that's why marker-amt ends up not being used.


;(define ls "ADVENT")
;(define ls "(3x3)XYZ")
;(define ls "X(8x2)(3x3)ABCY")
;(define ls "(27x12)(20x12)(13x14)(7x10)(1x12)A")
;(define ls "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN")
(define ls (read-line))

(struct ctree (marked unmarked-len) #:transparent)
(struct marker (amt times t) #:transparent)

(define (next-marker ls)
  (define re #px"\\((\\d+)x(\\d+)\\)")
  (define pos-re #px"\\(\\d+x\\d+\\)")
  (match-define (regexp re matches) ls)
  (match matches
    [#f (values #f #f #f #f)]
    [(list _ amt times)
     (let ([pos (regexp-match-positions pos-re ls)])
       (values (string->number amt) (string->number times)
               (caar pos) (cdr (last pos))))]))

(define compression-tree
  (let loop ([rem ls]
             [t (ctree '() 0)])
    (define-values (amt times mk1 mk2) (next-marker rem))
    (cond [(not amt)
           (struct-copy ctree t
                        [unmarked-len (+ (ctree-unmarked-len t) (string-length rem))])]
          [else
           (loop (substring rem (+ mk2 amt))
                 (ctree (cons (marker amt times
                                      (loop (substring rem mk2 (+ mk2 amt))
                                            (ctree '() 0)))
                              (ctree-marked t))
                        (+ (ctree-unmarked-len t) mk1)))])))

(let loop ([t compression-tree])
  (cond [(ctree? t)
         (+ (ctree-unmarked-len t) (loop (ctree-marked t)))]
        [(list? t)
         (foldl + 0 (map (λ(m) (* (marker-times m) (loop (marker-t m)))) t))]))
