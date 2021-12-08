#lang racket

(define (parse input)
  (map (λ(line)
         (match-define (list digits-str out-str) (string-split line " | "))
         (match-define (list digits ...) (string-split digits-str))
         (match-define (list out ...) (string-split out-str))
         (list digits out))
       input))

(define finders (hash 1 (λ(x) (= 2 (string-length x)))
                      4 (λ(x) (= 4 (string-length x)))
                      7 (λ(x) (= 3 (string-length x)))
                      8 (λ(x) (= 7 (string-length x)))))

(define (find-output-appearances/1 inp)
  (for/sum ([digits-and-out (in-list inp)])
    (define out (second digits-and-out))
    (count (λ(x) (ormap (λ(f) (f x)) (hash-values finders))) out)))

(define (figure-out-remaining-reprs inp)
  ;   t
  ; tl tr
  ;   m
  ; bl br
  ;   b
  (define representations (make-hash))
  (for ([i+f (in-hash-pairs finders)])
    (match-define (cons i f) i+f)
    (hash-set! representations i (ormap (λ(x) (and (f x) x)) inp)))
  (define tr+br (string->list (hash-ref representations 1)))
  (define t (findf (λ(c) (not (member c tr+br)))
                   (string->list (hash-ref representations 7))))
  (define 0/6/9 (filter (λ(x) (= 6 (string-length x))) inp))
  (define 6-repr
    (findf (λ(x) (= 4 (length (filter (λ(c) (not (member c (cons t tr+br))))
                                              (string->list x)))))
           0/6/9))
  (hash-set! representations 6 6-repr)
  (define 0-and-9 (filter (λ(x) (not (equal? 6-repr x))) 0/6/9))
  (define 9-repr
    (findf (λ(s) (andmap (λ(c) (member c (string->list s))) (string->list (hash-ref representations 4))))
                   0-and-9))
  (hash-set! representations 9 9-repr)
  (define 0-repr (findf (λ(x) (not (equal? 9-repr x))) 0-and-9))
  (hash-set! representations 0 0-repr)
  (define 2/3/5 (filter (λ(x) (= 5 (string-length x))) inp))
  (define 3-repr (findf (λ(s) (andmap (λ(c) (member c (string->list s)))
                                      (string->list (hash-ref representations 7))))
                        2/3/5))
  (hash-set! representations 3 3-repr)
  (define tr (findf (λ(c) (not (member c (string->list 6-repr)))) tr+br))
  (define br (findf (λ(c) (not (equal? c tr))) tr+br))
  (define 5-and-2 (filter (λ(x) (not (equal? 3-repr x))) 2/3/5))
  (define 2-repr (findf (λ(x) (ormap (λ(c) (equal? c tr)) (string->list x)))
                                5-and-2))
  (hash-set! representations 2 2-repr)
  (hash-set! representations 5 (findf (λ(x) (not (equal? 2-repr x))) 5-and-2))
  (for/hash ([n+s (in-hash-pairs representations)])
    (match-define (cons n s) n+s)
    (values (list->set (string->list s)) (number->string n))))

(define (outputs-sum/2 inp)
  (for/sum ([digits-and-out (in-list inp)])
    (match-define (list digits out) digits-and-out)
    (define digit-reprs (figure-out-remaining-reprs digits))
    (define out-digits (map (λ(s) (hash-ref digit-reprs (list->set (string->list s)))) out))
    (string->number (string-join out-digits ""))))    

(module+ test
  (require rackunit)
  (define test-inp "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce")
  (check-equal? (find-output-appearances/1 (parse (string-split test-inp "\n"))) 26)
  (check-equal? (outputs-sum/2 (parse (string-split test-inp "\n"))) 61229))

(module+ main
  (define inp (parse (with-input-from-file "day8.txt" port->lines)))
  (find-output-appearances/1 inp) ;412
  (outputs-sum/2 inp) #;978171 )
