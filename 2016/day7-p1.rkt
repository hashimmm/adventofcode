#lang racket

;; Credit to Mathew Butterick, I looked at his solution to find a neat way of separating all bracket
;; and non-bracket parts. I did have something similar in mind first but didn't know of regexp-match*
;; and then I tried to use a single regex but that missed edge cases.

(define (supports-tls? s)
  (define (2palindrome s) (regexp-match #px"(.)(?!\\1)(.)\\2\\1" s))
  (define bracket-pattern #px"(\\[.*?\\])")
  (define bracketed (regexp-match* bracket-pattern s))
  (define unbracketed (regexp-split bracket-pattern s))
  (and (ormap 2palindrome unbracketed)
       (not (ormap 2palindrome bracketed))))

#|
(supports-tls? "abba[mnop]qrst")
;#t
(supports-tls? "abcd[bddb]xyyx")
;#f
(supports-tls? "aaaa[qwer]tyui")
;#f
(supports-tls? "ioxxoj[asdfgh]zxcvbn")
;#t
|#

(sequence-count supports-tls? (in-port read-line))
