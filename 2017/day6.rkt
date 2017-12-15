#lang racket


(define (indexed lst)
  (define (indexer idx lst)
    (if (null? lst)
        lst
        (cons (cons idx (first lst)) (indexer (+ 1 idx) (rest lst)))))
  (indexer 0 lst))

(define (find-max lst #:key [key identity])
  (define (finder remaining current-max)
    (if (null? remaining)
        current-max
        (finder (rest remaining)
                (if (> (key (first remaining)) (key current-max))
                    (first remaining) 
                    current-max))))
  (finder lst (first lst)))


;; PART 1

(define (count-til-loop blocks)
  (define len (length blocks))
  (define (find-iters idx max-idx max-val)
    ;; Figure out how many times we'll pass over idx x to distribute n
    (define-values (q r) (quotient/remainder max-val len))
    (+ q (if (or (and (<= idx max-idx)
                      (<= (+ idx (- len max-idx)) r))
                 (and (> idx max-idx)
                      (<= (- idx max-idx) r)))
             1
             0)))
  
  (define (distribute blocks arrangements count)
    (cond [(set-member? arrangements blocks)
           count]
          [else
           (define indexed-blocks (indexed blocks))
           (match-define (cons max-idx max-val)
             (find-max indexed-blocks #:key cdr))
           (distribute
            (map (λ(x)
                   (match-define (cons idx val) x)
                   (if (= idx max-idx)
                       (find-iters idx max-idx max-val)
                       (+ val (find-iters idx max-idx max-val))))
                 indexed-blocks)
            (set-add arrangements blocks)
            (+ count 1))]))
  (distribute blocks (set) 0))

(module+ test
  (require rackunit)
  (check-equal? (count-til-loop '(0 2 7 0)) 5))

(module+ main
  (define input (map string->number (regexp-split "\t" (read-line))))
  (count-til-loop input))



;; PART 2

(define (loop-length blocks)
  (define len (length blocks))
  (define (find-iters idx max-idx max-val)
    ;; Figure out how many times we'll pass over idx x to distribute n
    (define-values (q r) (quotient/remainder max-val len))
    (+ q (if (or (and (<= idx max-idx)
                      (<= (+ idx (- len max-idx)) r))
                 (and (> idx max-idx)
                      (<= (- idx max-idx) r)))
             1
             0)))
  
  (define (distribute blocks arrangements count)
    (define found-at (index-of arrangements blocks))
    (cond [found-at
           (+ 1 found-at)]
          [else
           (define indexed-blocks (indexed blocks))
           (match-define (cons max-idx max-val)
             (find-max indexed-blocks #:key cdr))
           (distribute
            (map (λ(x)
                   (match-define (cons idx val) x)
                   (if (= idx max-idx)
                       (find-iters idx max-idx max-val)
                       (+ val (find-iters idx max-idx max-val))))
                 indexed-blocks)
            (cons blocks arrangements)
            (+ count 1))]))
  (distribute blocks '() 0))

(module+ test
  (check-equal? (loop-length '(0 2 7 0)) 4))

(module+ main
  (loop-length input))
