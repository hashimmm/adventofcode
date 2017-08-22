#lang racket

;; This one's even more needlessly convoluted than day9, oops :(

(struct bot (id vals job) #:transparent)
(struct value-action (val bot-num) #:transparent)
(struct bda (bot-num high-num high-to low-num low-to) #:transparent)

(define (bot-ready? b) (and ((length (bot-vals b)) . > . 1) b))

(define (bot-low b)
  (match-define (list v1 v2 v3 ...) (reverse (bot-vals b)))
  (if (v1 . < . v2)
      v1
      v2))

(define (bot-high b)
  (match-define (list v1 v2 v3 ...) (reverse (bot-vals b)))
  (if (v1 . >= . v2)
      v1
      v2))

#;(define instructions
  (list (value-action 5 2)
        (bda 2 0 'bot 1 'bot)
        (value-action 3 1)
        (bda 1 0 'bot 1 'output)
        (bda 0 0 'output 2 'output)
        (value-action 2 2)))

(define instructions
  (for/list ([line (in-port read-line)])
    (match line
      [(regexp #px"value (\\d+) goes to bot (\\d+)" (list _ val bot-num))
       (value-action (string->number val)
                     (string->number bot-num))]
      [(regexp #px"bot (\\d+) gives low to (output|bot) (\\d+) and high to (output|bot) (\\d+)"
               (list _ bot-num low-to low-num high-to high-num))
       (bda (string->number bot-num)
            (string->number high-num)
            (string->symbol high-to)
            (string->number low-num)
            (string->symbol low-to))])))


(define (give-out o val)
  (cons val o))

(define (give-bot b val)
  (struct-copy bot b [vals (cons val (bot-vals b))]))

(define (give x x-id val bots outputs)
  (cond [(list? x) (values bots (hash-set outputs x-id (give-out x val)))]
        [(bot? x) (values (hash-set bots x-id (give-bot x val)) outputs)]))

(define bots
  (for/fold ([bots (hash)])
            ([action (in-list instructions)])
    (match action
      [(value-action val bot-num)
       (hash-update bots bot-num (curryr give-bot val)
                    (bot bot-num null null))]
      [(struct bda _)
       (hash-update bots
                    (bda-bot-num action)
                    (λ(b) (struct-copy bot b [job action]))
                    (bot (bda-bot-num action) null null))])))

(define (next-ready bot-table [idx 0])
  (define-values (left right) (split-at (hash-values bot-table) idx))
  (ormap bot-ready? (append right left)))

(define (empty id x)
  (cond [(symbol=? x 'bot) (bot id '() '())]
        [(symbol=? x 'output) '()]))

(define-values (bot1761 outputs)
  (let loop ([b (next-ready bots)]
             [bots bots]
             [outputs (hash)]
             [idx 0])
    (define targets (hash 'output outputs 'bot bots))
    (define outkeys (hash-keys outputs))
    ;(displayln b)
    ;(displayln idx)
    (cond [(not b) 'none]
          [(and (member 0 outkeys) (member 1 outkeys) (member 2 outkeys))
           (values b outputs)]
          [else
           (match-define (bda _ high-num high-to low-num low-to) (bot-job b))
           (define target-high
             (hash-ref (hash-ref targets high-to) high-num (empty high-num high-to)))
           (define target-low
             (hash-ref (hash-ref targets low-to) low-num (empty low-num low-to)))
           (define-values (new-bot new-bots new-outputs)
             (for/fold ([nb b]
                        [bots bots]
                        [outputs outputs])
                       ([remaining (in-range (- (length (bot-vals b)) 1) 0 -1)]
                        [val (in-list (reverse (bot-vals b)))]
                        [val2 (in-list (drop (reverse (bot-vals b)) 1))])
               #:break (remaining . < . 1)
               ;(displayln nb)
               ;(displayln outputs)
               (match-define (list lower higher) (sort (list val val2) <))
               (define-values (next-bots next-outputs)
                 (call-with-values
                  (λ() (give target-high high-num higher bots outputs))
                  (curry give target-low low-num lower)))
               (values (struct-copy bot nb [vals (drop (bot-vals nb) 2)])
                       next-bots next-outputs)))
           (define new-idx (remainder (+ idx 1) (hash-count bots)))
           (define newer-bots (hash-set new-bots (bot-id new-bot) new-bot))
           (loop (next-ready newer-bots new-idx)
                 newer-bots
                 new-outputs
                 new-idx)])))

outputs
(* (first (hash-ref outputs 1))
   (first (hash-ref outputs 0))
   (first (hash-ref outputs 2)))
