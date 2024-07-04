#lang racket

(define (read-inp)
  (define (parse s)
    (with-input-from-string (string-replace s "," " ") read))
  (for/list ([s (string-split (file->string "day13.txt") "\n\n")])
    (map parse (string-split s "\n"))))

(define (part1 pairs)
  (for/sum ([(p idx) (in-indexed pairs)]
            #:when (member (right-o? p) '(cont l)))
    (add1 idx)))

(define (right-o? p)
  (match-define (list left right) p)
  (define (compare l r)
    (cond [(and (list? l) (list? r)) (right-o? (list l r))]
          [(and (number? l) (number? r))
           (cond [(< l r) 'l]
                 [(> l r) 'r]
                 [else 'c])]
          [else
           (right-o? (list (if (number? l) (list l) l)
                           (if (number? r) (list r) r)))]))
  (let loop ([l left] [r right])
    (cond [(and (null? l) (null? r)) 'c]
          [(null? l) 'l]
          [(null? r) 'r]
          [else
           (let ([result (compare (first l) (first r))])
             (if (equal? 'c result)
                 (loop (rest l) (rest r))
                 result))])))

(define (part2 packets)
  (define dividers '( ((2)) ((6)) ))
  (define with-dividers (append dividers (append-map identity packets)))
  (define sorted-packets (sort with-dividers (λ(l r) (member (right-o? (list l r)) '(l c)))))
  (apply * (for/list ([(p idx) (in-indexed sorted-packets)] #:when (member p dividers)) (add1 idx))))

(part1 (read-inp))
(part2 (read-inp))


;; aside

;; find the bug in the following code:

#;(define (right-order? p)
  (match p
    [(list (list) (list)) 'cont]
    [(list (list) _) 'l]
    [(list _ (list)) 'r]
    [(list (list first-l rest-l ...) (list first-r rest-r ...))
     (match (cons first-l first-r)
       [(cons (? number? num-l) (? number? num-r))
        (cond [(< num-l num-r) 'l]
              [(> num-l num-r) 'r]
              [else (right-order? (list rest-l rest-r))])]
       [(cons (? number? num-l) list-r)
        (right-order? (list (list num-l) list-r))]
       [(cons list-l (? number? num-r))
        (right-order? (list list-l (list num-r)))]
       [_
        (let ([check-firsts (right-order? (list first-l first-r))])
          (if (equal? 'cont check-firsts)
              (right-order? (list rest-l rest-r))
              check-firsts))])]))
