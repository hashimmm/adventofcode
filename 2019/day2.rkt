#lang racket

(define (process-program program)
  (define (processor state next)
    (cond [(= (value-at state next) 99)
           state]
          [(= (value-at state next) 1)
           (processor (add-positions state
                                     (value-at state (+ 1 next))
                                     (value-at state (+ 2 next))
                                     (value-at state (+ 3 next)))
                      (+ 4 next))]
          [(= (value-at state next) 2)
           (processor (mul-positions state
                                     (value-at state (+ 1 next))
                                     (value-at state (+ 2 next))
                                     (value-at state (+ 3 next)))
                      (+ 4 next))]))
  (processor program 0))

(define (value-at state next)
  (vector-ref state next))

(define (add-positions state x-pos y-pos store)
  (vector-set! state store (+ (value-at state x-pos) (value-at state y-pos)))
  state)

(define (mul-positions state x-pos y-pos store)
  (vector-set! state store (* (value-at state x-pos) (value-at state y-pos)))
  state)

(define (parse-program txt)
  (define as-lst (string-split txt ","))
  (list->vector (map string->number as-lst)))

(define parse-and-process (compose process-program parse-program))

(define (main1)
  (define input-program-txt
    (file->string "input-day2.txt"))
  (define input-program (parse-program input-program-txt))
  (vector-set! input-program 1 12)
  (vector-set! input-program 2 2)
  (define output-state (process-program input-program))
  (vector-ref output-state 0))

(define (parse-and-process-sample-program-with-input inp1 inp2)
  (define input-program-txt
    (file->string "input-day2.txt"))
  (define input-program (parse-program input-program-txt))
  (vector-set! input-program 1 inp1)
  (vector-set! input-program 2 inp2)
  (define output-state (process-program input-program))
  (vector-ref output-state 0))


"
How I arrived at the answer for part 2:

> (parse-and-process-sample-program-with-input 3 1)
1166547
> (parse-and-process-sample-program-with-input 4 1)
1443027
> (parse-and-process-sample-program-with-input 4 2)
1443028
> (parse-and-process-sample-program-with-input 4 2)
1443028
> (parse-and-process-sample-program-with-input 6 2)
1995988
> (parse-and-process-sample-program-with-input 6 1)
1995987
> (parse-and-process-sample-program-with-input 5 1)
1719507
> (parse-and-process-sample-program-with-input 6 1)
1995987
> (parse-and-process-sample-program-with-input 5 1)
1719507
> (- 19690720 1719507)
17971213
> (parse-and-process-sample-program-with-input 12 1)
3654867
> (parse-and-process-sample-program-with-input 120 1)
33514707
> (parse-and-process-sample-program-with-input 50 1)
14161107
> (parse-and-process-sample-program-with-input 60 1)
16925907
> (parse-and-process-sample-program-with-input 90 1)
25220307
> (parse-and-process-sample-program-with-input 75 1)
21073107
> (parse-and-process-sample-program-with-input 65 1)
18308307
> (parse-and-process-sample-program-with-input 70 1)
19690707
> (parse-and-process-sample-program-with-input 70 14)
19690720
> (+ 14 (* 100 70))
7014
"
