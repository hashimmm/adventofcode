#lang racket

(require openssl/md5)
(require data/queue)

(define (my-md5 str)
  (with-input-from-string
      str
    (λ() (md5 (current-input-port)))))

;; Reason for off-by-one is given later below.
(define salt "ihaygndm")  ; 15036, actual answer 15035
;(define salt "abc")  ; my answer: 22729, supposed to be: 22728

(define (equal2? a . b)
  (if (null? b)
      (error "equal2? requires 2 or more arguments")
      (andmap (λ(x) (equal? a x)) b)))

(define (make-input idx)
  (string-append salt (number->string idx)))

(define (triple? x)
  (for/or ([n (in-range (- (string-length x) 2))])
    (if (equal2? (string-ref x n) (string-ref x (+ 1 n)) (string-ref x (+ 2 n)))
        (string-ref x n)
        #f)))

(define (has-five? of-char s)
  (define x (string->list s))
  (let loop ([remaining x])
    (define new-remaining (member of-char remaining))
    (cond [(or (not new-remaining) ((length new-remaining) . < . 5))
           #f]
          [(apply equal2? (take new-remaining 5))
           #t]
          [else
           (loop (rest new-remaining))])))


(define hashes (make-queue))


;; So the answer is idx - 1 because we break AFTER the iteration with 64 keys starts,
;; which is 1 index later than the one in which the 64th key was found... Stupid off-by-one :(

;; Part 1

#;(define _
  (for ([x (in-range 1000)])
    (enqueue! hashes (my-md5 (make-input x)))))

#;(define-values (idx k)
  (for/fold ([index 0]
             [keys '()])
            ([_ (in-range 0 2 0)])
    #:break (= 64 (length keys))
    (define current-hash (dequeue! hashes))
    (define tripled-char (triple? current-hash))
    (enqueue! hashes (my-md5 (make-input (+ index 1000))))
    (values
     (+ index 1)
     (if (and tripled-char (ormap (λ(x) (has-five? tripled-char x)) (queue->list hashes)))
         (cons current-hash keys)
         keys))))


;; Part 2

(define (make-stretched-hash idx)
  (for/fold ([current-hash (my-md5 (make-input idx))])
            ([_ (in-range 2016)])
    (my-md5 current-hash)))

(define _
  (for ([x (in-range 1000)])
    (enqueue! hashes (make-stretched-hash x))))

(define-values (idx k)
  (for/fold ([index 0]
             [keys '()])
            ([_ (in-range 0 2 0)])
    #:break (= 64 (length keys))
    (define current-hash (dequeue! hashes))
    (define tripled-char (triple? current-hash))
    (enqueue! hashes (make-stretched-hash (+ index 1000)))
    (values
     (+ index 1)
     (if (and tripled-char (ormap (λ(x) (has-five? tripled-char x)) (queue->list hashes)))
         (cons current-hash keys)
         keys))))
