#lang racket

(define inp (file->lines "day7-input-1"))

(struct color (adj type) #:transparent)
(struct to-spec (amt col) #:transparent)

(define (parse-rule r)
  (define trimmed (string-trim r "."))
  (define as-list (string-split trimmed))
  (define from (apply color (take as-list 2)))
  ;; skip the "bags contain" part
  (define remaining
    (string-split (string-join (drop as-list 4) " ") ", "))
  (define to
    (if (equal? (first remaining) "no other bags")
        '()
        (map (λ(to-str)
               (define parts (string-split to-str))
               (to-spec (string->number (first parts))
                        (color (second parts) (third parts))))
             remaining)))
  (values from to))

(define (make-color-containment-graph rules)
  (for/hash ([r (in-list rules)])
    (define-values (from to)
      (parse-rule r))
    (values from (map to-spec-col to))))

(define (make-containment-graph rules)
  (for/hash ([r (in-list rules)])
    (parse-rule r)))

(define (check-reachable-from color-graph col [already-found '()] [remaining #f])
  ;; Basically, look at col only the first time.
  ;; Check and store where col is found.
  ;; Iteratively:
  ;;   Check and store wherever col's containers are found.
  ;;   Until no new containers are found.
  (define to-check
    (if (not remaining) (list col) remaining))
  (define found-in (for/fold ([so-far '()])
                             ([(k v) (in-hash color-graph)])
                     (if (and (not (member k already-found))
                              (ormap (λ(x) (member x v)) to-check))
                         (cons k so-far)
                         so-far)))
  (if (null? found-in)
      already-found
      (check-reachable-from color-graph col (append found-in already-found) found-in)))

(define (count-must-carry graph col)
  (define spec-list (hash-ref graph col))
  (apply +
         (apply + (map (λ(spec) (to-spec-amt spec)) spec-list))
         (map (λ(spec)
                (* (to-spec-amt spec)
                   (count-must-carry graph (to-spec-col spec))))
              spec-list)))

(module+ test
  (require rackunit)

  (define test1 "shiny purple bags contain 2 pale blue bags, 1 wavy fuchsia bag, 5 pale salmon bags.")
  (define test2 "shiny purple bags contain no other bags.")

  (let-values ([(from to) (parse-rule test2)])
    (check-equal? from (color "shiny" "purple"))
    (check-equal? to '()))
  
  (let-values ([(from to) (parse-rule test1)])
    (check-equal? from (color "shiny" "purple"))
    (check-equal? to (list
                      (to-spec 2 (color "pale" "blue"))
                      (to-spec 1 (color "wavy" "fuchsia"))
                      (to-spec 5 (color "pale" "salmon")))))

  ;(make-color-containment-graph (list test1))

  (define test-inp-1
#<<HERE
light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.
HERE
    )
  (define test-inp-1-list (string-split test-inp-1 "\n"))
  (check-equal?
   (count-must-carry (make-containment-graph test-inp-1-list) (color "shiny" "gold"))
   32)

  (define test-inp-2
#<<HERE
shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags.
HERE
    )
  (define test-inp-2-list (string-split test-inp-2 "\n"))
  (check-equal?
   (count-must-carry (make-containment-graph test-inp-2-list) (color "shiny" "gold"))
   126)

  ;; Answers:
  (check-equal? (length (check-reachable-from (make-color-containment-graph inp)
                                              (color "shiny" "gold")))
                179)
  (define cg (make-containment-graph inp))
  (check-equal? 
   (count-must-carry cg (color "shiny" "gold"))
   18925))
