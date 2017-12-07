#lang racket

(define DISK-LENGTH 272)
(define INPUT "11011110011011101")

(define input-list (string->list INPUT))

(define (invert loc)
  (map (λ(c) (if (equal? #\0 c) #\1 #\0)) loc))

(define (next-fill loc)
  (append loc (cons #\0 (invert (reverse loc)))))

(define (dragon-check loc)
  (if (odd? (length loc))
      loc
      (dragon-check
       (map (λ(poc)
              (if (equal? (first poc) (second poc))
                  #\1
                  #\0))
            (reverse
             (foldl (λ(x y init-or-acc)
                      (if (even? y)
                          (cons x init-or-acc)
                          (cons (list (first init-or-acc) x) (rest init-or-acc))))
                    '()
                    loc
                    (range (length loc))))))))

;; Part 1

(list->string
 (dragon-check
  (let loop ([current-data input-list])
    (if ((length current-data) . >= . DISK-LENGTH)
        (take current-data DISK-LENGTH)
        (loop (next-fill current-data))))))

;; Part 2

(define DISK-LENGTH2 35651584)
(list->string
 (dragon-check
  (let loop ([current-data input-list])
    (if ((length current-data) . >= . DISK-LENGTH2)
        (take current-data DISK-LENGTH2)
        (loop (next-fill current-data))))))
