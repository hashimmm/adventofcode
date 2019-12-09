#lang racket


(define (valid-number? num)
  (define digit-list (map char->integer (string->list (number->string num))))
  (define-values (found-decrease found-equal last)
    (for/fold ([found-decrease #f]
               [found-equal #f]
               [last (first digit-list)])
              ([digit (in-list (rest digit-list))])
      #:break found-decrease
      (values (digit . < . last)
              (or found-equal (digit . = . last))
              digit)))
  (and (not found-decrease) found-equal))

;; still wrong

(define (valid-number2? num)
  (define digit-list (map char->integer (string->list (number->string num))))
  (define-values (found-decrease found-2-equal last-was-equal last)
    (for/fold ([found-decrease #f]
               [found-2-equal 'no]
               [last-was-equal #f]
               [last (first digit-list)])
              ([digit (in-list (rest digit-list))])
      #:break found-decrease
      (let ([current-is-equal (digit . = . last)])
        (values (digit . < . last)
                (cond [(symbol=? found-2-equal 'yes)
                       'yes]
                      [(and (symbol=? found-2-equal 'maybe) (not current-is-equal))
                       'yes]
                      [(and (symbol=? found-2-equal 'maybe) current-is-equal)
                       'no]
                      [(and last-was-equal current-is-equal)
                       'no]
                      [(and (symbol=? found-2-equal 'no) current-is-equal)
                       'maybe]
                      [else
                       'no])
                current-is-equal
                digit))))
  (and (not found-decrease)
       (or (symbol=? found-2-equal 'yes)
           (symbol=? found-2-equal 'maybe))))

(define (count-valid from to)
  (define (counter current last so-far)
    (cond [(current . = . last)
           (if (valid-number? current) (add1 so-far) so-far)]
          [else
           (if (valid-number? current)
               (counter (add1 current) last (add1 so-far))
               (counter (add1 current) last so-far))]))
  (counter from to 0))

(define (count-valid2 from to)
  (define (counter current last so-far)
    (cond [(current . = . last)
           (if (valid-number2? current) (add1 so-far) so-far)]
          [else
           (if (valid-number2? current)
               (counter (add1 current) last (add1 so-far))
               (counter (add1 current) last so-far))]))
  (counter from to 0))
