#lang racket

(require math/statistics)
(require srfi/1)

(define (parse inp)
  (define template (string->list (first inp)))
  (define rules
    (map (λ(s) (string-split s " -> ")) (rest (rest inp))))
  (define rule-hash
    (for/hash ([from+to (in-list rules)])
      (match from+to
        [(list from to)
         (values (string->list from)
                 (string-ref to 0))])))
  (values template rule-hash))

(define (step loc rules)
  (reverse
   (let loop ([cur (first loc)]
              [rem (rest loc)]
              [new '()])
     (if (null? rem)
         (cons cur new)
         (let ([to-insert (hash-ref rules (list cur (first rem)) #f)])
           (loop (first rem)
                 (rest rem)
                 (if to-insert
                     (cons to-insert (cons cur new))
                     (cons cur new))))))))

(define (diff/1 loc rules)
  (define final (for/fold ([loc loc]) ([_ (in-range 10)]) (step loc rules)))
  (define counts (map cdr (hash->list (samples->hash final))))
  (- (apply max counts) (apply min counts)))

(define (diff/2 loc rules num-steps)
  (define all-letters
    (set-union (list->set loc)
               (list->set (hash-values rules))
               (list->set (apply append (hash-keys rules)))))

  (define combis (cartesian-product (set->list all-letters) (set->list all-letters)))

  (define all-counts
    (for/hash ([l+r (in-list combis)])
      (values l+r 0)))

  (define expansions
    (for/hash ([k+v (in-hash-pairs rules)])
      (match k+v
        [(cons (and key (list l r)) m)
         (values key
                 (list (list l m) (list m r)))])))

  (define rule-list (hash->list expansions))

  (define combination-producers
    (for/hash ([l+r (in-list combis)])
      (values l+r
              (filter-map (λ(rule)
                            (and (member l+r (cdr rule))
                                 (car rule)))
                          rule-list))))

  (define increasers
    (for/hash ([l+r (in-list combis)])
      (values l+r
              (λ(counts)
                (apply + (map (λ(x) (hash-ref counts x))
                              (hash-ref combination-producers l+r)))))))
  
  (define (step all-counts)
    (for/hash ([(l+r count) (in-hash all-counts)])
      (values l+r
              ((hash-ref increasers l+r) all-counts))))

  (define initial-counts
    (for/fold ([c all-counts])
              ([l+r (in-list (zip loc (rest loc)))])
      (hash-update c l+r (λ(x) (add1 x)))))

  (define letter-producing-pairs
    (for/fold ([lpp (hash)])
              ([(pair c) (in-hash rules)])
      (hash-update lpp c (λ(l) (cons pair l)) '())))

  (define (step-letter letter-counts pair-counts)
    (for/hash ([(c count) (in-hash letter-counts)])
      (values c
              (+ (hash-ref letter-counts c 0)
                 (apply + (map (λ(pair) (hash-ref pair-counts pair))
                               (hash-ref letter-producing-pairs c '())))))))

  (define initial-letter-counts
    (for/hash ([c (in-set all-letters)])
      (values c (count (λ(x) (equal? x c)) loc))))

  (define final (for/fold ([cur-counts initial-counts]
                           [letter-counts initial-letter-counts]
                           #:result letter-counts)
                          ([_ (in-range num-steps)])
                  (values (step cur-counts)
                          (step-letter letter-counts cur-counts))))

  (- (apply max (hash-values final)) (apply min (hash-values final))))

(module+ test
  (require rackunit)
  (define inp "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C
")
  (define-values (loc rules) (parse (string-split inp "\n")))
  (check-equal? (diff/1 loc rules) 1588)
  (check-equal? (diff/2 loc rules 10) 1588)
  (check-equal? (diff/2 loc rules 40) 2188189693529))

(module+ main
  (define-values (loc rules) (parse (with-input-from-file "day14.txt" port->lines)))
  (diff/1 loc rules) ;;2797
  (diff/2 loc rules 40) #;2926813379532 )
