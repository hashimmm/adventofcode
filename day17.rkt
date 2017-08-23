#lang racket

(require srfi/1)
(require openssl/md5)
(require memoize)

(define (my-md5 str)
  (md5 (open-input-string str)))

;; Test inputs
;(define INPUT "ihgpwlah")
;(define INPUT "kglvqrro")
;(define INPUT "ulqzkmiv")

;; My input
(define INPUT "pxxbnzuo")

(define START-POSN '(0 0))
(define GOAL '(3 3))

(define/memo (hash-for-path path)
  (my-md5 (string-append INPUT (list->string path))))

(define (variations posn)
  (match-define (list x y) posn)
  (list `((,(- x 1) ,y) ,#\L)
        `((,(+ x 1) ,y) ,#\R)
        `((,x ,(- y 1)) ,#\U)
        `((,x ,(+ y 1)) ,#\D)))

(define (valid? posn+dir)
  (define posn (first posn+dir))
  (andmap (λ(p) (and (not (negative? p))
                         (p . < . 4)))
              posn))

(define (valid-variations posn)
  (filter valid? (variations posn)))

(define (open? letter)
  (member letter (list #\b #\c #\d #\e #\f)))

(define (get-next-pos pos dir)
  (match-define (list x y) pos)
  (cond [(equal? #\U dir)
         `(,x ,(- y 1))]
        [(equal? #\D dir)
         `(,x ,(+ y 1))]
        [(equal? #\L dir)
         `(,(- x 1) ,y)]
        [(equal? #\R dir)
         `(,(+ x 1) ,y)]))

(struct pos-vector (pos path) #:transparent)

(define dir-idx-hash (hash #\U 0 #\D 1 #\L 2 #\R 3))

(define (valid-directions pv)
  (match-define (pos-vector posn path-taken) pv)
  (define hash-for-this-state (hash-for-path path-taken))
  (define next-posns (valid-variations posn))
  (if
   (equal? (pos-vector-pos pv) '(3 3))
   '()
   (filter-map
    (λ(posn+dir)
      (match-define (list next-pos next-dir) posn+dir)
      (if (open? (string-ref hash-for-this-state (hash-ref dir-idx-hash next-dir)))
          (pos-vector (get-next-pos posn next-dir)
                      (append path-taken `(,next-dir)))
          #f))
    next-posns)))

(define (pos-goal? p) (equal? GOAL (pos-vector-pos p)))


;; Part 1

(define (vault-bfs)
  (define initial-pv (pos-vector '(0 0) '()))
  (define (bfs pv next)
    (if (pos-goal? pv)
        pv
        (bfs (first next)
             (append (rest next)
                     (valid-directions (first next))))))
  (bfs initial-pv (valid-directions initial-pv)))

(define answer-pv (vault-bfs))
(list->string (pos-vector-path answer-pv))


;; Part 2

;; PS Gotta love how the only real difference between dfs and bfs is which way
;; the append is done for the "next" list
;; PPS Not that it makes any difference whether I did a bfs or dfs. But earlier
;; I went bfs thinking maybe not all paths terminate, and we needed to find the shortest
;; (first) one, but the question in part 2 implied this terminates
;; guaranteed and we needed to be exhaustive anyway so dfs.

(define (path-length pv)
  (length (pos-vector-path pv)))

(define (vault-dfs)
  (define initial-pv (pos-vector '(0 0) '()))
  (define (dfs pv next longest-yet)
    (cond [(empty? next)
           (if (and (pos-goal? pv) ((path-length pv) . > . (path-length longest-yet)))
               pv
               longest-yet)]
          [(pos-goal? pv)
           (dfs (first next)
                (append (valid-directions (first next)) (rest next))
                (if ((path-length pv) . > . (path-length longest-yet))
                    pv
                    longest-yet))]
          [else
           (dfs (first next)
                (append (valid-directions (first next)) (rest next))
                longest-yet)]))
  (dfs initial-pv (valid-directions initial-pv) initial-pv))

(define answer-pv2 (vault-dfs))
(path-length answer-pv2)
