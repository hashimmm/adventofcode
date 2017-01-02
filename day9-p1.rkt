#lang racket

(define ls (string->list (read-line)))

(define (marker-maker til next)
  (define last (if (not (null? til)) (first til) #f))
  (cond [(null? til)
         (if (equal? next #\() (list next) '())]
        [(equal? #\( last)
         (if (char-numeric? next) (cons next til) '())]
        [(and (char-numeric? last) (not (member #\x til)))
         (if (or (char-numeric? next) (equal? #\x next)) (cons next til) '())]
        [(equal? #\x last)
         (if (char-numeric? next) (cons next til) '())]
        [(and (char-numeric? last) (member #\x til))
         (if (or (char-numeric? next) (equal? #\) next)) (cons next til) '())]))

(define (marker-complete marker)
  (cond [(null? marker) #f]
        [(equal? #\) (first marker))
         (list->string (reverse marker))]
        [else #f]))

(define decompressed
  (let loop ([marker-matching '()]
             [remaining ls]
             [decompressed '()])
    (cond [(null? remaining) decompressed]
          [else
           (define marker-now (marker-maker marker-matching (first remaining)))
           (define string-marker (marker-complete marker-now))
           (cond [string-marker
                  (match-define (regexp #px"\\((\\d+)x(\\d+)\\)"
                                        (list _ num-s repeat-s)) string-marker)
                  (define num (string->number num-s))
                  (define repeat (string->number repeat-s))
                  (define single (take (rest remaining) num))
                  (define repeated (for/list ([_ (in-range repeat )]) single))
                  (loop '()
                        (drop (rest remaining) num)
                        (append* decompressed repeated))]
                 [(null? marker-now)
                  (loop '()
                        (rest remaining)
                        (append decompressed marker-matching (list (first remaining))))]
                 [else
                  (loop marker-now
                        (rest remaining)
                        decompressed)])])))

(length decompressed)
