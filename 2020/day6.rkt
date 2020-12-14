#lang racket

(define inp (file->string "day6-input-1"))

;; Would've been a good idea to use set-union
(define (count-yes g)
  (define combined (string-replace g "\n" ""))
  (length (remove-duplicates (string->list combined))))

;; Would've been a good idea to use set-intersect
(define (count-all-yes g)
  (define combined (string-replace g "\n" ""))
  (define mem-wise (string-split g "\n"))
  (define all-ques (remove-duplicates (string->list combined)))
  (length (filter (λ(quest)
                    (andmap (λ(ans-set) (member quest (string->list ans-set)))
                            mem-wise))
                  all-ques)))

(define (make-groups inp)
  (string-split inp "\n\n"))

(module+ test
  (require rackunit)
  ;; Answers:
  (check-equal? (apply + (map count-yes (make-groups inp))) 6335)
  (check-equal? (apply + (map count-all-yes (make-groups inp))) 3392))
