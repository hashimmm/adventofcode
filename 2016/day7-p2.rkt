#lang racket

(define (list->window-stream ls n)
  (cond [(< (length ls) n) empty-stream]
        [else
         (stream-cons (take ls n) (cond [(= (length ls) n) empty-stream]
                                        [else (list->window-stream (rest ls) n)]))]))


(define (supports-ssl? s)

  (define (abas s)
    (define char-stream (list->window-stream (string->list s) 3))
    (define aba-list
      (stream-filter (λ(x)
                       (and (equal? (first x) (third x))
                            (not (equal? (first x) (second x)))
                            x))
                     char-stream))
    (stream->list aba-list))

  (define (corresp-bab? hyps aba)
    (define bab
      (list->string (list (second aba) (first aba) (second aba))))
    (ormap (curryr string-contains? bab) hyps))

  (define bracket-pattern #px"(\\[.*?\\])")
  (define hypernet (regexp-match* bracket-pattern s))
  (define supernet (regexp-split bracket-pattern s))
  (displayln hypernet)
  (displayln supernet)
  (displayln (append-map abas supernet))
  (ormap (curry corresp-bab? hypernet) (append-map abas supernet)))

#|
(supports-ssl? "aba[bab]xyz")
;#t
(supports-ssl? "xyx[xyx]xyx")
;#f
(supports-ssl? "aaa[kek]eke")
;#t
(supports-ssl? "zazbzeke[bzb]cdb")
;#t
|#

(sequence-count supports-ssl? (in-port read-line))
