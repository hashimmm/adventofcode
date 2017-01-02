#lang racket

(define SCREEN-ROWS 6)
(define SCREEN-COLS 50)

(define screen (build-vector SCREEN-ROWS (λ(_) (make-vector SCREEN-COLS 'off))))

(define (make-cyclic-referrer size)
  (λ(x)
    (modulo x size)))

(define get-screen-col-ref (make-cyclic-referrer SCREEN-COLS))
(define get-screen-row-ref (make-cyclic-referrer SCREEN-ROWS))

(define (screen-row-ref rownum)
  (vector-ref screen (get-screen-row-ref rownum)))
(define (screen-col-ref rownum colnum)
  (vector-ref (screen-row-ref rownum) (get-screen-col-ref colnum)))
(define (screen-col-set! rownum colnum value)
  (vector-set! (screen-row-ref rownum) (get-screen-col-ref colnum) value))

(define (rect A B)
  (for* ([x (in-range A)]
         [y (in-range B)])
    (screen-col-set! y x 'on)))

(define (cycle-right rownum)
  (for/fold ([last (screen-col-ref rownum -1)])
            ([i (in-range SCREEN-COLS)])
    (let ([current (screen-col-ref rownum i)])
      (screen-col-set! rownum i last)
      current)))

(define (cycle-down colnum)
  (for/fold ([last (screen-col-ref -1 colnum)])
            ([i (in-range SCREEN-ROWS)])
    (let ([current (screen-col-ref i colnum)])
      (screen-col-set! i colnum last)
      current)))

(define (rotate-row rownum-A amount-B)
  (for ([_ (in-range amount-B)])
    (cycle-right rownum-A)))

(define (rotate-column colnum-A amount-B)
  (for ([_ (in-range amount-B)])
    (cycle-down colnum-A)))

(define/match (run-command s)
  [((regexp #rx"rect ([0-9]+)x([0-9]+)" (list _ args ...)))
   (apply rect (map string->number args))]
  [((regexp #rx"rotate row y=([0-9]+) by ([0-9]+)" (list _ args ...)))
   (apply rotate-row (map string->number args))]
  [((regexp #rx"rotate column x=([0-9]+) by ([0-9]+)" (list _ args ...)))
   (apply rotate-column (map string->number args))])


(for ([command (in-port read-line)])
  (run-command command))

(foldl + 0
       (vector->list (vector-map (curry vector-count (λ(x) (eq? x 'on)))
                                 screen)))

(for ([row (in-vector screen)])
  (displayln (list->string (vector->list (vector-map (λ(x) (if (eq? x 'on) #\# #\.)) row)))))