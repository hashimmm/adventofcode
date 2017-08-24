#lang racket

(require 2htdp/batch-io)

(define-for-syntax (string-upper-case? s)
  (andmap char-upper-case? (string->list s)))

(define-match-expander $
  (lambda (stx)
    (syntax-case stx ()
      [($ words ...)
       (let ([wordlist (map (λ(x) (if (string-upper-case? x) (datum->syntax stx (string->symbol x)) x))
                            (map (compose symbol->string syntax-e) (syntax->list #'(words ...))))])
         #`(list #,@wordlist))])))

(define (swap-into-list x y)
  (list y x))
(define (rotate vec m dir)
  (define n (remainder m (vector-length vec)))
  (if (equal? dir "right")
      (vector-map! (λ(x y) y)
                   vec
                   (let-values ([(left right) (vector-split-at-right vec n)])
                     (vector-append right left)))
      (vector-map! (λ(x y) y)
                   vec
                   (let-values ([(left right) (vector-split-at vec n)])
                     (vector-append right left)))))

(define (stoc s)
  (if (char? s)
      s
      (string-ref s 0)))

(define/match (transform how voc)
  [(($ swap position X with position Y) voc)
   (let ([tmp (vector-ref voc X)])
     (vector-set! voc X (vector-ref voc Y))
     (vector-set! voc Y tmp))]
  [(($ swap letter X with letter Y) voc)
   (let ([loc-x (vector-member (stoc X) voc)]
         [loc-y (vector-member (stoc Y) voc)])
     (vector-set! voc loc-x (stoc Y))
     (vector-set! voc loc-y (stoc X)))]
  [(($ rotate LR X STEPS) voc)
   (rotate voc X LR)]
  [(($ rotate based on position of letter X) voc)
   (let ([idx (vector-member (stoc X) voc)])
     (rotate voc (+ idx (if (< idx 4) 1 2)) "right"))]
  [(($ reverse positions X through Y) voc)
   (vector-copy! voc X
                 (list->vector (reverse (vector->list (vector-drop (vector-take voc (+ 1 Y)) X)))))]
  [(($ move position X to position Y) voc)
   (let ([letter (vector-ref voc X)]
         [vec2 (vector-copy voc)])
     (vector-map! (λ(x y)
                    (if (< X Y)
                        (if (or (< y X) (> y Y))
                            x
                            (if (= y Y)
                                (stoc letter)
                                (vector-ref voc (+ 1 y))))   
                        (if (or (> y X) (< y Y))
                            x
                            (if (= y Y)
                                (stoc letter)
                                (vector-ref vec2 (- y 1))))))
                  voc
                  (build-vector (vector-length voc) identity)))])

;(transform (list "swap" "position" 1 "with" "position" 2) (list->vector (string->list "abcdefgh")))

;; Part 1

;(define pw (list->vector (string->list "abcde")))
(define pw (list->vector (string->list "abcdefgh")))

(define instructions (read-lines 'stdin))

(for ([line (in-list instructions)])
  (transform (map (λ(x) (let ([num (string->number x)])
                          (if num num x)))
                  (regexp-split " " line))
             pw))
pw


;; Part 2

(define scrambled (string->list "fbgdceah"))
(define scrambled-vec (list->vector (string->list "fbgdceah")))
(for/last ([try (in-permutations scrambled)])
  (define try-vec (list->vector try))
  (for ([line (in-list instructions)])
    (transform (map (λ(x) (let ([num (string->number x)])
                            (if num num x)))
                    (regexp-split " " line))
               try-vec))
  #:final (equal? try-vec scrambled-vec)
  (list->vector try))
