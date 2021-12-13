#lang racket
;; TODO: would be a good exercise to do this based on continuations.
(define (parse lines)
  (for/fold ([paths (hash)])
            ([s (in-list lines)])
    (define from-to (string-split s "-"))
    (hash-update (hash-update paths (first from-to) (λ(l) (cons (second from-to) l)) '())
                 (second from-to) (λ(l) (cons (first from-to) l)) '())))

(define (is-small? cave)
  (andmap char-lower-case? (string->list cave)))

(define (find-distinct-paths/1 paths)
  (define (find paths from visited)
    (cond [(equal? "end" from) 1]
          [(set-member? visited from) 0]
          [(is-small? from)
           (apply +
                  (map (λ(cave) (find paths cave (set-add visited from)))
                       (hash-ref paths from)))]
          [else
           (apply +
                  (map (λ(cave) (find paths cave visited))
                       (hash-ref paths from)))]))
  (find paths "start" (set)))

(define (find-distinct-paths/2 paths)
  (define (find paths from visited doubling)
    (cond [(equal? "end" from) 1]
          [(set-member? visited from) ;; implies is-small?
           (cond [(equal? "start" from) 0]
                 [(not doubling)  ;; doubling not set (#f), set current node as doubling.
                  (apply +
                     (map (λ(cave) (find paths cave (set-add visited from) from))
                          (hash-ref paths from)))]
                 [else 0])]
          [(is-small? from)
           (apply +
                  (map (λ(cave) (find paths cave (set-add visited from) doubling))
                       (hash-ref paths from)))]
          [else
           (apply +
                  (map (λ(cave) (find paths cave visited doubling))
                       (hash-ref paths from)))]))
  (find paths "start" (set) #f))

(module+ test
  (require rackunit)
  (define test-inp-1 "start-A
start-b
A-c
A-b
b-d
A-end
b-end")
  (check-equal? (find-distinct-paths/1 (parse (string-split test-inp-1 "\n"))) 10)

  (define test-inp-2 "fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW")

  (check-equal? (find-distinct-paths/1 (parse (string-split test-inp-2 "\n"))) 226)
  (check-equal? (find-distinct-paths/2 (parse (string-split test-inp-2 "\n"))) 3509))

(module+ main
  (define cave-paths (parse (with-input-from-file "day12.txt" port->lines)))
  (find-distinct-paths/1 cave-paths) ;;5228
  (find-distinct-paths/2 cave-paths) #;131228 )
