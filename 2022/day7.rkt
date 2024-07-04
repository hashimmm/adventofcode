#lang racket

(define (read-inp) (file->lines "day7.txt"))

(define (build-sizes inp)
  (define sizes (make-hash))
  (for/fold ([cur-path '()]
             [cur-path-seen? #f])
            ([cmd inp])
    (define cmd-parts (string-split cmd))
    (match cmd-parts
      [(list "$" "cd" "..")
       (values (rest cur-path) #f)]
      [(list "$" "cd" "/")
       (values '("/") #f)]
      [(list "$" "cd" path)
       (values (cons path cur-path) #f)]
      [(list "$" "ls")
       (values cur-path
               (hash-has-key? sizes (reverse cur-path)))]
      [(list "dir" _)
       (values cur-path cur-path-seen?)]
      [(list size file)
       (unless cur-path-seen?
         (let loop ([key (reverse cur-path)]
                    [size-num (string->number size)])
           (unless (null? key)
             (hash-update! sizes key (λ(x) (+ x size-num)) 0)
             (loop (drop-right key 1) size-num))))
       (values cur-path cur-path-seen?)]
      [_ (error (string-append "invalid cmd " cmd))]))
  sizes)

(module+ test
  (require rackunit)
  (define test-inp
    "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k"))

(module+ test
  (define test-sizes (build-sizes (string-split test-inp "\n")))
  (check-equal? (foldl + 0 (hash-values test-sizes)) 73410244))

(define (part1 inp)
  (define sizes (build-sizes inp))
  (foldl + 0 (filter (λ(s) (<= s 100000)) (hash-values sizes))))

(module+ test
  (check-equal? (part1 (string-split test-inp "\n")) 95437))

(module+ main
  (part1 (read-inp)))

(define (part2 inp)
  (define sizes (build-sizes inp))
  (define total-space 70000000)
  (define required 30000000)
  (define used-space (hash-ref sizes '("/")))
  (define free-space (- total-space used-space))
  (define min-size-to-delete (- required free-space))
  (apply min (filter (λ(s) (>= s min-size-to-delete)) (hash-values sizes))))

(module+ test
  (check-equal? (part2 (string-split test-inp "\n")) 24933642))

(module+ main
  (part2 (read-inp)))
