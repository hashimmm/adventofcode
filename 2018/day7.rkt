#lang racket

;; TODO: do this in datalog

(define input (file->lines "day7-input"))

(define (find-dependencies input)
  (for/fold ([deps (hash)])
            ([line input])
    (define-values (before after) (parse line))
    (hash-update deps before (curry cons after) '())))

(define (invert-dependencies deps)
  (for/fold ([inverted-deps (hash)])
            ([key-and-values (hash-values deps)])
    (define key (car key-and-values))
    (define values (cdr key-and-values))
    (foldl
     (λ(x updated-inverted-deps)
       (hash-update updated-inverted-deps x (curry cons key) '()))
     inverted-deps
     values)))

(define (parse line)
  (values (string-ref line 5) (string-ref line 36)))

(define (find-roots deps)
  (define mentioned-as-after (apply append (hash-values deps)))
  (filter (λ(x) (not (member x mentioned-as-after)))
          (hash-keys deps)))

(define (find-next deps currents made inv-deps pending)
  (define all-nexts
    (apply append (map (λ(x) (hash-ref deps x '()))
                       currents)))
  (define-values (do-able-nexts todo-later)
    (splitf-at
     (append all-nexts pending)
     (λ(x) (or (not (hash-ref inv-deps x #f))
               (andmap (curryr member made) (hash-ref inv-deps x))))))
  (values do-able-nexts todo-later))

(define (construct-steps deps roots inv-deps)
  (define (inner deps roots made to-do)
    (cons
     (sort roots char<?)
     (let-values ([(next later) (find-next deps roots made inv-deps to-do)])
       (if (null? next)
           '()
           (inner deps
                  next
                  (append made roots)
                  later)))))
  (inner deps roots '() '()))

(define dependencies (find-dependencies input))
(define inverted-dependencies (invert-dependencies dependencies))
(define roots (find-roots dependencies))

(define step-list (construct-steps dependencies roots inverted-dependencies))
