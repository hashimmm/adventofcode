#lang racket

(struct monkey (worries op test inspected) #:transparent)

(define (read-inp)
  (define file-str (file->string "day11.txt"))
  (define monkey-sections (map (λ(section) (string-split section "\n"))
                               (string-split file-str "\n\n")))
  (map (λ(lines)
         (define param-strs (map (λ(str) (second (string-split str ": "))) (rest lines)))
         (define worries (map string->number (string-split (first param-strs) ", ")))
         (define op
           (match (second (string-split (second param-strs) " = "))
             [(regexp "old (.) (.+)" (list _ operator operand))
              (let ([prim-op (if (equal? operator "+") + *)])
                (λ(x) (prim-op x (if (equal? operand "old") x (string->number operand)))))]))
         (define test
           (λ(x)
             (if (= 0 (modulo x (string->number (last (string-split (third param-strs) " ")))))
                 (string->number (last (string-split (fourth param-strs))))
                 (string->number (last (string-split (fifth param-strs)))))))
         (monkey worries
                 op
                 test
                 0))
       monkey-sections))

(define (monkey-business monkeys)
  (apply * (take (sort (map monkey-inspected monkeys) >) 2)))

(define (play-round monkeys (reliever (λ(x) (floor (/ x 3)))))
  (define queues (make-hash))
  (define monkeys*
    (for/list ([(m idx) (in-indexed monkeys)])
      (define items (append (monkey-worries m) (reverse (hash-ref queues idx '()))))
      (hash-set! queues idx '())
      (define inspected (length items))
      (for ([item items])
        (define calcd-worry ((monkey-op m) item))
        (define relieved-worry (reliever calcd-worry))
        (define where-to
          ((monkey-test m) relieved-worry))
        (hash-update! queues where-to (λ(x) (cons relieved-worry x)) '()))
      (monkey '()
              (monkey-op m)
              (monkey-test m)
              (+ (monkey-inspected m) inspected))))
  (for/list ([(m idx) (in-indexed monkeys*)])
    (monkey (reverse (hash-ref queues idx '()))
            (monkey-op m)
            (monkey-test m)
            (monkey-inspected m))))

(define ms (for/fold ([monkeys (read-inp)])
                     ([i (in-range 20)])
             (play-round monkeys)))

(monkey-business ms)

(define ms2 (for/fold ([monkeys (read-inp)])
                      ([i (in-range 10000)])
              (play-round monkeys (λ(x) (modulo x 9699690)))))  ;; Simply multiply all the division test figures

(monkey-business ms2)
