#lang racket

(require openssl/md5)

;; TODO: why was this giving a bad result? :S
(define door-id "abc")

(define pwd
  (for/fold ([pwd (apply hash (foldl  (λ(x y) (append (list x #f) y))
                                      '()
                                      (range 8)))])
            ([i (in-naturals)])
    #:break (andmap identity (hash-values pwd))
    (define s (string-append door-id (number->string i)))
    (define hexdigest (md5 (open-input-string s)))
    (define pos (string->number (substring hexdigest 5 6)))
    (cond [(and (string-prefix? hexdigest "00000")
                pos (> 8 pos) (not (hash-ref pwd pos)))
           (displayln hexdigest)
           (displayln s)
           (hash-set pwd pos (substring hexdigest 6 7))]
          [else pwd])))

(for ([x (in-range 8)])
    (display (hash-ref pwd x)))

