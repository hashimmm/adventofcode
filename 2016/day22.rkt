#lang racket

(require srfi/13)

(struct node (x y used avail) #:prefab)

(define input (rest (rest (port->list read-line (open-input-file "day22-input")))))

(define nodes
  (for/list ([line (in-list input)])
    (define tokens (regexp-split #px" +" line))
    (define path-tokens (regexp-split "-" (first tokens)))
    (node (string->number (string-drop (list-ref path-tokens 1) 1))
          (string->number (string-drop (list-ref path-tokens 2) 1))
          (string->number (string-drop-right (list-ref tokens 2) 1))
          (string->number (string-drop-right (list-ref tokens 3) 1)))))

;; Part 1

(define viable-pairs
  (filter-map
   (λ(node-pair)
     (match node-pair
       [(list nodeA nodeB)
        (cond  [(and ((node-used nodeA) . > . 0)
                     ((node-used nodeA) . <= . (node-avail nodeB)))
                (list nodeA nodeB)]
               [(and ((node-used nodeB) . > . 0)
                     ((node-used nodeB) . <= . (node-avail nodeA)))
                (list nodeB nodeA)]
               [else #f])]))
   (combinations
    nodes
    2)))

(length viable-pairs)

;; Diagram for hand-solving part 2

;; 64 is the data in the goal node,
;; 91 is the available space in the "slot" node.

(for ([x (in-list nodes)])
  (when (= 0 (node-y x))
    (displayln ""))
  (cond [(and (equal? 0 (node-x x)) (equal? 0 (node-y x)))
         (display "S")]
        [(and (equal? 37 (node-x x)) (equal? 0 (node-y x)))
         (display "G")]
        [(> (node-avail x) 64)
         (display "_")]
        [(> (node-used x) 91)
         (display "#")]
        [else
         (display ".")])
  (display " "))

;; So basically the answer is:
(+ 6 (- 24 3) 8 (* 5 36))

;; Bad attempt: the blacklist construction is wrong,
;; and the algo's too slow anyway.
#|

;; Manually checked
(define max-x 37)
(define max-y 23)

(define (viable-pair? nfrom nto)
  (member (list nfrom nto) viable-pairs))

(define (adjacent? nfrom nto)
  (or (and (= (node-x nfrom) (node-x nto))
           (= 1 (abs (- (node-y nfrom) (node-y nto)))))
      (and (= (node-y nfrom) (node-y nto))
           (= 1 (abs (- (node-x nfrom) (node-x nto)))))))

(define (adjacent-nodes n)
  (filter (λ(x) (or (< 0 (node-x x)) (< 0 (node-y x))
                    (> max-x (node-x x)) (> max-y (node-y x))))
          (list (node (+ 1 (node-x n)) (node-y n) (node-used n) (node-avail n))
                (node (- (node-x n) 1) (node-y n) (node-used n) (node-avail n))
                (node (node-x n) (+ (node-y n) 1) (node-used n) (node-avail n))
                (node (node-x n) (- (node-y n) 1) (node-used n) (node-avail n)))))

(define (viable-tos nfrom)
  (filter-map (λ(x) (if (equal? (first x) nfrom)
                        (second x)
                        #f))
              viable-pairs))

(define (my-min l)
  (define filtered-l (filter identity l))
  (if (null? filtered-l)
      #f
      (apply min (filter identity l))))

(define (my-add x y)
  (and x y (+ x y)))

(define (shortest-path bl nfrom nto)
  (cond [(equal? nfrom nto)
         0]
        [(viable-pair? nfrom nto)
         (if (adjacent? nfrom nto)
             1
             (my-add 1
                     (my-min (map (curry shortest-path (cons nfrom bl) nfrom)
                                  (adjacent-nodes nto)))))]
        [else
         (let* (;[nfrom-targets (viable-tos nfrom)]
                [nto-targets-all (viable-tos nto)]
                [nto-targets (remove* bl nto-targets-all)]
                ;[common (intersect nfrom-targets nto-targets)]
                )
           (my-add (my-min (map (curry shortest-path (cons nto bl) nto)
                                nto-targets))
                   (my-min (map (curry shortest-path (cons nfrom bl) nfrom)
                                (adjacent-nodes nto)))))]))

(shortest-path '()
               (node 37 0 64 27)
               (node 0 0 70 22))
|#