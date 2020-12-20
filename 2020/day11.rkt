#lang racket

;; Note when running in DrRacket: To run on the full input, requires 256M memory.
;;
;; Game-of-life style thing should "obviously" map to threads, right?
;;
;; So this threads and async channels thingy brings ordering problems.
;;
;; Basically, when I first tried this code, it gave me different answers every time.
;; First of all, I got the requirements wrong:
;;   It says SIMULTANEOUSLY.
;; First I had a reactive version, an "actor", on update, notified its neighbours.
;; But that doesn't model the "SIMULTANEOUS" part.
;; Second, I made it so that the actor loop collects values for all neighbours,
;; updates its state, broadcasts the update (regardless of actual change) to all
;; neighbours.
;; This was "correct" in some sense, but since these are threads and async-channels,
;; and even worse, I was keeping a hash (as opposed to a queue), so I had no track of
;; the last value.
;; So I still got different answers.
;; The solution for me was to requeue messages of a future term...
;; And tada!
;; Thanks Mr Beazley for the RAFT course :)

(require racket/async-channel)

(define inp (vector-map (compose list->vector string->list) (list->vector (file->lines "day11-input-1"))))

(define empty #\L)
(define floor #\.)
(define occupied #\#)
(define empty? (λ(x) (equal? x empty)))
(define floor? (λ(x) (equal? x floor)))
(define occupied? (λ(x) (equal? x occupied)))

(struct $posn (x y) #:transparent)

(define (neighbours layout posn)
  (define xs (range (max (sub1 ($posn-x posn)) 0)
                    (min (+ ($posn-x posn) 2) (vector-length (vector-ref layout 0)))))
  (define ys (range (max (sub1 ($posn-y posn)) 0)
                    (min (+ ($posn-y posn) 2) (vector-length layout))))
  (filter (λ(x) (not (equal? posn x)))
          (map (λ(x) (apply $posn x))
               (cartesian-product xs ys))))

(define (next-state state adjs)
  (cond [(and (empty? state)
              (andmap (λ(s) (or (empty? s) (floor? s))) adjs))
         occupied]
        [(and (occupied? state)
              (>= (length (filter occupied? adjs))
                  4))
         empty]
        [else
         state]))

(define (make-mailboxes layout)
  (for*/hash ([(x row) (in-indexed (in-vector layout))]
              [(seat col) (in-indexed (in-vector x))])
    (values ($posn col row)
            (make-async-channel))))

(define (make-close-evt)
  (define -c- (make-channel))
  (define close-evt (thread (λ() (channel-get -c-))))
  (define (closef) (channel-put -c- 'close))
  (define (closed?) (thread-dead? close-evt))
  (values closef closed? close-evt))

(define (make-actors layout mailboxes)
  (define height (vector-length layout))
  (define width (vector-length (vector-ref layout 0)))
  (define total (* width height))
  (define-values (closef closed? close-evt) (make-close-evt))
  (define -result- (make-channel))
  (define -c- (make-channel))
  (define controller
    (thread
     (λ()
       (let loop ([states (make-hash)]
                  [dups (make-hash)])
         (match (channel-get -c-)
           [(list posn state)
            (cond [(equal? (hash-ref states posn #f) state)
                   (hash-set! dups posn #t)
                   (if (and (= (hash-count dups) total) (andmap identity (hash-values dups)))
                       (begin (closef)
                              (channel-put -result- states))
                       (loop states dups))]
                  [else
                   (hash-set! states posn state)
                   (hash-set! dups posn #f)
                   (loop states dups)])])))))
  (values
   -result-
   (for*/hash ([(x row) (in-indexed (in-vector layout))]
               [(seat col) (in-indexed (in-vector x))])
     (define my-state seat)
     (define my-posn ($posn col row))
     (values
      my-posn
      (thread
       (λ()
         (define -my-inbox-
           (hash-ref mailboxes my-posn))
         (define -my-neighbours-
           (for/list ([p (in-list (neighbours layout my-posn))])
             (hash-ref mailboxes p)))
         (for ([-n- (in-list -my-neighbours-)])
           (async-channel-put -n- (list 0 my-posn my-state)))
         (for/fold ([my-state my-state]
                    [neighbour-states (hash)]
                    [my-term 0])
                   ([msg (in-producer (λ() (async-channel-get -my-inbox-)))])
           #:break (closed?)
           (match msg
             [(list term posn state)
              (if (> term my-term)
                  (begin (async-channel-put -my-inbox- msg)
                         (values my-state neighbour-states my-term))
                  (let ()
                    (define -addr- (hash-ref mailboxes posn))
                    (define updated-neighbours (hash-set neighbour-states posn state))
                    (if (= (hash-count updated-neighbours) (length -my-neighbours-))
                        (let ([updated-state (next-state my-state (hash-values updated-neighbours))])
                          (channel-put -c- (list my-posn updated-state))
                          (for ([-n- (in-list -my-neighbours-)])
                            (async-channel-put -n- (list (add1 my-term) my-posn updated-state)))
                          (values updated-state
                                  (hash)
                                  (add1 my-term)))
                        (values my-state
                                updated-neighbours
                                my-term))))]))))))))

(define (count-occupied-result -r-)
  (define result (channel-get -r-))
  (count occupied? (hash-values result)))

;(define mailboxes (make-mailboxes inp))
;(define-values (-res- actors) (make-actors inp mailboxes))
;(count-occupied-result -res-)

(module+ test
  (require rackunit)
  (check-equal?
   (neighbours (vector (vector 1 2 3 4) 2 3 4) ($posn 2 2))
   (list ($posn 1 1) ($posn 1 2) ($posn 1 3) ($posn 2 1) ($posn 2 3) ($posn 3 1) ($posn 3 2) ($posn 3 3)))

  ;; test input
  (define test-inp
    (vector-map
     (compose list->vector string->list)
     (list->vector
      '("L.LL.LL.LL"
        "LLLLLLL.LL"
        "L.L.L..L.."
        "LLLL.LL.LL"
        "L.LL.LL.LL"
        "L.LLLLL.LL"
        "..L.L....."
        "LLLLLLLLLL"
        "L.LLLLLL.L"
        "L.LLLLL.LL"))))

  (define test-mailboxes (make-mailboxes test-inp))
  (define-values (-r- test-actors) (make-actors test-inp test-mailboxes))
  (check-equal? (count-occupied-result -r-) 37)
  )
