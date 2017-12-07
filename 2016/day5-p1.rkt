#lang racket

(require openssl/md5)

;; TODO: why was this giving a bad result? :S
;(define door-id (read-line))
(define door-id "abc")

(for/fold ([pwd ""])
          ([i (in-naturals)])
  #:break (= 8 (string-length pwd))
  (define s (string-append door-id (number->string i)))
  (define hexdigest (md5 (open-input-string s)))
  (cond [(string-prefix? hexdigest "00000")
         (displayln hexdigest)
         (displayln s)
         (string-append pwd (substring hexdigest 5 6))]
        [else pwd]))
