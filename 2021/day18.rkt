#lang racket

(struct P (l r) #:transparent)

(define (parse-line s)
  (define as-list (with-input-from-string (string-replace s "," " ") read))
  (define (list->P l)
    (if (number? l)
        l
        (P (list->P (first l))
           (list->P (second l)))))
  (list->P as-list))

(define (addP l r)
  (reduce (P l r)))

(define (reduce p)
  (define exploded (explode p))
  (define splut (if (equal? p exploded)
                    (split exploded)
                    exploded))
  (if (equal? splut p)
      p
      (reduce splut)))

(define (explode p)
  ;; Prepare a stack of boxes :)
  (define stack '())
  (define (stack-up! n)
    (define b (box n))
    (set! stack (cons b stack))
    b)
  ;; add-left is basically just add to the top box
  (define (add-l! n)
    (unless (null? stack)
      (set-box! (car stack) (+ n (unbox (car stack))))))
  ;; Until traversal reaches depth 4, each node is stacked up
  ;; "!" is the "process this node" function
  (define ! stack-up!)
  ;; On reaching depth-4, next node will have a number added, then,
  ;; processing is a no-op
  (define (make-add-r! n)
    (λ(x) (set! ! identity) (+ n x)))
  ;; On reaching depth 4, set next node to have a number added,
  ;; and the node-4 trigger becomes a normal traversal.
  ;; "x" is the "traverse" function.
  ;; "x!" is the explode-this-node! function
  (define (x! m n)
    (add-l! m)
    (set! x! (λ(m n) (P (x m 5)
                        (x n 5))))
    (set! ! (make-add-r! n))
    0)
  ;; Simply traverse using the functions defined above and
  ;; everything "just works" :)
  (define (x p-or-n d)
    (match* (d p-or-n)
      [(4 (P m n))
       (x! m n)]
      [(n (P l r))
       (P (x l (add1 d))
          (x r (add1 d)))]
      [(n i)
       (! i)]))
  ;; And in the end, throw out our stack of boxes.
  (define (resolve p)
    (match p
      [(P l r)
       (P (resolve l) (resolve r))]
      [(box n) n]
      [n n]))
  ;; Voila
  (resolve (x p 0)))

(define (split p)
  (define (! n)
    (if (>= n 10)
        (let ()
          (set! ! identity)
          (P (floor (/ n 2))
             (ceiling (/ n 2))))
        n))
  (define (splitter p)
    (match p
      [(P l r)
       (P (splitter l)
          (splitter r))]
      [n
       (! n)]))
  (splitter p))

(define (magnitude p-or-n)
  (match p-or-n
    [(P l r)
     (+ (* 3 (magnitude l)) (* 2 (magnitude r)))]
    [n n]))

(define (sum/1 inputs)
  (for/fold ([so-far (first inputs)])
            ([next (in-list (rest inputs))])
    (addP so-far next)))

(define (mag-of-sum/1 inputs)
  (magnitude (sum/1 inputs)))

(define (largest-mag-of-2/2 inputs)
  (define c (combinations inputs 2))
  (define reverse-c (map reverse c))
  (apply max
         (for/list ([l+r (in-list (append c reverse-c))])
           (magnitude (apply addP l+r)))))

(module+ test
  (require rackunit)
  (check-equal? (addP (parse-line "[[[[4,3],4],4],[7,[[8,4],9]]]") (P 1 1))
                (P (P (P (P 0 7) 4) (P (P 7 8) (P 6 0))) (P 8 1)))
  (check-equal? (sum/1 (list (P 1 1) (P 2 2) (P 3 3) (P 4 4)))
                (P (P (P (P 1 1) (P 2 2)) (P 3 3)) (P 4 4)))
  (check-equal? (sum/1 (list (P 1 1) (P 2 2) (P 3 3) (P 4 4) (P 5 5)))
                (P (P (P (P 3 0) (P 5 3)) (P 4 4)) (P 5 5)))
  (check-equal?
   (sum/1 (map parse-line (string-split "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
[7,[5,[[3,8],[1,4]]]]
[[2,[2,2]],[8,[8,1]]]
[2,9]
[1,[[[9,3],9],[[9,0],[0,7]]]]
[[[5,[7,4]],7],1]
[[[[4,2],2],6],[8,7]]")))
   (parse-line "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]")))

(module+ main
  (define inputs (map parse-line (with-input-from-file "day18.txt" port->lines)))
  (time (mag-of-sum/1 inputs)) ;; 4072
  (time (largest-mag-of-2/2 inputs)) #;4483 )
