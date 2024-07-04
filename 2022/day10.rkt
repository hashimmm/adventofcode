#lang racket
(require data/queue)

(define (read-inp) (map (λ(line) (let ([split (string-split line)])
                                   (cons (first split) (map string->number (rest split)))))
                        (file->lines "day10.txt")))

(define (part1 insts)
  (define interesting-cycles '(20 60 100 140 180 220))
  (define results (make-hash))
  (let loop ([remaining insts]
             [cycle 1]
             [X 1]
             [q '()])
    (when (member cycle interesting-cycles)
      (hash-set! results cycle X))
    (cond [(or (and (null? remaining) (null? q)) (= cycle 220))
           (void)]
          [(null? q)
           (match (first remaining)
             [(list "noop")
              (loop (rest remaining)
                    (add1 cycle)
                    X
                    q)]
             [(list "addx" n)
              (loop (rest remaining)
                    (add1 cycle)
                    X
                    (cons n q))])]
          [else
           (match q
             [(list n _ ...)
              (loop remaining
                    (add1 cycle)
                    (+ X n)
                    (rest q))])]))
  (foldl (λ(x acc) (+ acc (* (car x) (cdr x)))) 0 (hash->list results)))

(module+ main
  (part1 (read-inp)))

(define (part2 insts)
  (define (add-pixel pixels cycle X)
    (define pxn (modulo (sub1 cycle) 40))
    (if (member pxn (list (sub1 X) X (add1 X)))
        (let () (enqueue! pixels '&) pixels)
        (let () (enqueue! pixels '_) pixels)))
  (define final-pixels
    (let loop ([remaining insts]
               [cycle 1]
               [X 1]
               [q '()]
               [pixels (make-queue)])
      (cond [(or (and (null? remaining) (null? q)))
             pixels]
            [else
             (define pixels* (add-pixel pixels cycle X))
             (cond
               [(null? q)
                (match (first remaining)
                  [(list "noop")
                   (loop (rest remaining)
                         (add1 cycle)
                         X
                         q
                         pixels*)]
                  [(list "addx" n)
                   (loop (rest remaining)
                         (add1 cycle)
                         X
                         (cons n q)
                         pixels*)])]
               [else
                (match q
                  [(list n _ ...)
                   (loop remaining
                         (add1 cycle)
                         (+ X n)
                         (rest q)
                         pixels*)])])])))
  (define (safe-split-at lst n)
    (if (<= n (length lst)) (split-at lst n) (values lst '())))
  (let loop ([remaining (queue->list final-pixels)])
    (if (null? remaining)
        (void)
        (let-values ([(these next) (safe-split-at remaining 40)])
          (displayln these)
          (loop next)))))

(module+ main
  (part2 (read-inp)))
