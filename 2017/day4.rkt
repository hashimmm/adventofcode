#lang racket

(require 2htdp/batch-io)

(define passphrases (read-lines "day4-p1-input"))

;; PART 1

(length
 (filter (λ(pp)
           (let ([words (regexp-split " " pp)])
             (= (length words) (length (set->list (list->set words))))))
         passphrases))

;; PART 2

(define (anagrams word)
  (map list->string (permutations (string->list word))))

(length
 (filter (λ(pp)
           (let ([words (regexp-split " " pp)])
             ;; Take the current word,
             ;; get its anagrams,
             ;; check if the remaining list has any of those anagrams,
             ;; get next word, til end or match.
             (let loop ([remaining (rest words)]
                        [current (first words)])
               (cond [(empty? remaining)
                      #t]
                     [(ormap (λ(x) (member x (anagrams current)))
                             remaining)
                      #f]
                     [else
                      (loop (rest remaining) (first remaining))]))))
         passphrases))
