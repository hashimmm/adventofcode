#lang racket

(define inp (file->list "day10-input-1"))

(define (windowed size l)
  (let loop ([windows '()]
             [remaining l])
    (if (< (length remaining) size)
        (reverse windows)
        (loop (cons (take remaining size) windows) (rest remaining)))))

(define (find-differences nums)
  (define sorted (sort (cons 0 nums) <))
  (map (match-lambda [(list a b) (- b a)]) (windowed 2 sorted)))

(define (product-of-num-of-1s-and-3s l)
  (define-values (1s 3s)
    (for/fold ([1s 0] [3s 0]) ([n (in-list l)])
      (if (= n 1)
          (values (add1 1s) 3s)
          (values 1s (add1 3s)))))
  (* 1s (add1 3s)))

(define (memoize fun)
  (define memory (make-hash))
  (λ x
    (if (hash-has-key? memory x)
        (hash-ref memory x)
        (begin
          (let ([result (apply fun x)])
            (hash-set! memory x result)
            result)))))

;; It was a bit mind-boggling for me to come up with this :(
;; The cond with 4 was just for testing.
;; Basically it was something along the lines of:
;;   1. write down the patterns and get the figures
;;   2. figure that we can split this, somehow, into base + recur cases
;;   3. figure that it's like we have multiple possible next steps
;;      so we have to recur into all possible next steps...
;;   4. but at the same time, we have to subtract the ones we already just
;;      accounted for.
;;   5. ... Which obviously simplifies to removing the adds and subtracts.
;;
;; And THEN I search wolfram alpha for the sequence and find this is called
;; the tribonacci sequence!!!
(define (num-ways-to-reach num-1s)
  (cond [(= 1 num-1s)
         1]
        [(= 2 num-1s)
         2]
        [(= 3 num-1s)
         4]
        #;[(= 4 num-1s)
         7]
        #;[else
         (+ 2 4
            (- (num-ways-to-reach (- num-1s 3)) 1)
            (- (num-ways-to-reach (- num-1s 2)) 2)
            (- (num-ways-to-reach (- num-1s 1)) 3))]
        [else
         (+ (num-ways-to-reach (- num-1s 3))
            (num-ways-to-reach (- num-1s 2))
            (num-ways-to-reach (- num-1s 1)))]))

(define memo-num-ways (memoize num-ways-to-reach))

;; This I figured in minutes.. I figured this out before the num-1s part, and
;; tackled that separately first and came back to this.
;;
;; This was basically just a turn-the-crank thing... like, you know, there's
;; m ways to get to point p, and n to go from p to q, so you have to multiply
;; the two to get all of them.
;;
;; It was easily verified by manual calculation.
(define (num-ways-end l)
  (define diffs (find-differences l))
  (let loop ([remaining diffs])
    (define-values (1s next-3s) (splitf-at remaining (λ(x) (= 1 x))))
    (define next-1s (dropf next-3s (λ(x) (= 3 x))))
    (let ([ans (memo-num-ways (length 1s))])
      (if (null? next-1s)
          ans
          (* ans (loop next-1s))))))

#|
Looking around the interwebs...
There's another algo for part 2, which is roughly the same thing but depth-first:

(define memo (make-hash))
(define end (sub1 (vector-length arr)))
(define (get val)
  (cond [ (not (vector-member val arr)) 0 ]
        [ (= val (vector-ref arr end))  1 ]
        [ else (or (hash-ref memo val #f)
                   (let ([ x (+ (get (+ val 1)) (get (+ val 2)) (get (+ val 3))) ])
                     (hash-set! memo val x)
                     x)) ]))

All these years and recursion can _still_ look funny sometimes O_o
|#

(module+ test
  (require rackunit)
  (define sample-list
    '(28
      33
      18
      42
      31
      14
      46
      20
      48
      47
      24
      23
      49
      45
      19
      38
      39
      11
      1
      32
      25
      35
      8
      17
      7
      9
      4
      2
      34
      10
      3))

  (check-equal? (product-of-num-of-1s-and-3s (find-differences sample-list)) 220)

  (define sample-list-2
    '(16
      10
      15
      5
      1
      11
      7
      19
      6
      12
      4))

  (check-equal? (product-of-num-of-1s-and-3s (find-differences sample-list-2)) 35)

  (check-equal? (num-ways-end sample-list-2) 8)
  (check-equal? (num-ways-end sample-list) 19208)

  ;; Answers:
  (check-equal? (product-of-num-of-1s-and-3s (find-differences inp)) 1836)
  (check-equal? (time (num-ways-end inp)) 43406276662336))
