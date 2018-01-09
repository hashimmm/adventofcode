#lang racket

;; This is quite horrible, I know.

;; PART 1

(struct node-def (node weight lifted) #:prefab)

(define (parse line)
  (match-define (list _ node weight lifts-str)
    (regexp-match #rx"([a-z]+) \\(([0-9]+)\\)( -> .*)?" line))
  (define lifted
    (if (not lifts-str)
        '()
        (regexp-split ", " (string-trim lifts-str " -> " #:right? #f))))
  (node-def node (string->number weight) lifted))


(define (parse-all str-defs)
  (for/list ([line (in-list (regexp-split "\n" str-defs))])
    (parse line)))


(define (find-root nodes)
  (define roots (map node-def-node nodes))
  (define lifteds (append* (map node-def-lifted nodes)))
  (first (filter (λ(x) (not (member x lifteds))) roots)))


(module+ test
  (require rackunit)
  (define input
"pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)")
  (define nodes (parse-all input))
  (define root-node (find-root nodes))
  (check-equal? root-node "tknk"))

(module+ main
  (require 2htdp/batch-io)
  (define nodes (parse-all (read-file "day7-input")))
  (define root-node (find-root nodes))
  (displayln root-node))

(define (get-node node lon)
  (cond [(equal? node (node-def-node (first lon)))
         (first lon)]
        [else
         (get-node node (rest lon))]))

(define (make-tree nodes root)
  (define (iterate tree-so-far next-nodes)
    (cond [(empty? next-nodes)
           tree-so-far]
          [else
           (define children (map (curryr get-node nodes) (node-def-lifted (first next-nodes))))
           (iterate
            (hash-set tree-so-far
                      (first next-nodes)
                      children)
            (rest next-nodes))]))
  (iterate (hash) nodes))

(define (find-weight node-tree node)
  (define (sub-weight node)
    (define own-weight (node-def-weight node))
    (if (empty? (hash-ref node-tree node))
        own-weight
        (apply + (cons own-weight
                       (map sub-weight
                            (hash-ref node-tree node))))))
  (sub-weight node))


(define (odd-weighted tree nodes)
  (define weights (map (curry find-weight tree) nodes))
  (let loop ([rem-nodes nodes]
             [rem-weights weights]
             [next-3 (take weights 3)])
    (cond [(apply = next-3)
           (if (= 3 (length rem-weights))
               #f
               (loop (drop rem-nodes 2)
                     (drop rem-weights 2)
                     (take (drop rem-weights 2) 3)))]
          [(= (first rem-weights) (second rem-weights))
           (third rem-nodes)]
          [(= (first rem-weights) (third rem-weights))
           (second rem-nodes)]
          [else
           (first rem-nodes)])))


(define (find-odd node-tree root siblings)
  (define new-siblings (hash-ref node-tree root))
  (define odd-node (odd-weighted node-tree new-siblings))
  (define odds-children (hash-ref node-tree odd-node '()))
  (cond [(not odd-node)
         (values
          root
          (find-weight node-tree root)
          (find-weight node-tree
                       (first (memf (λ(x) (not (equal? root x)))
                                    siblings))))]
        [(empty? odds-children)
         (values
          odd-node
          (find-weight node-tree odd-node)
          (find-weight node-tree
                       first (memf (λ(x) (not (equal? odd-node x)))
                                        (hash-ref node-tree root))))]
        [else
         (find-odd node-tree odd-node new-siblings)]))


(module+ test
  (define node-tree (make-tree nodes root-node))
  (check-equal? (length (hash-keys node-tree)) (length nodes))
  (define root-node-def (get-node root-node nodes))
  (check-equal? (find-weight node-tree root-node-def) 778)
  (define-values (odd-node odd-weight normal-weight)
    (find-odd node-tree root-node-def '()))
  (define difference (- normal-weight odd-weight))
  (check-equal? difference -8)
  (displayln (+ (node-def-weight odd-node) difference)))

(module+ main
  (define node-tree (make-tree nodes root-node))
  (define root-node-def (get-node root-node nodes))
  (define-values (odd-node odd-weight normal-weight)
    (find-odd node-tree root-node-def '()))
  (define difference (- normal-weight odd-weight))
  (+ (node-def-weight odd-node) difference))
