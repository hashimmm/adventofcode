#lang racket

(define-namespace-anchor a)

(require racket/performance-hint)

;; There's plenty of other things that would improve performance btw;
;; but holy crap this is slow :D Solves part 1, part 2 is for solving by hand.

;(define input (read-lines 'stdin))

(define input
  '("cpy a b"
    "dec b"
    "cpy a d"
    "cpy 0 a"
    "cpy b c"
    "inc a"
    "dec c"
    "jnz c -2"   ; Set a to a + c, c to 0
    "dec d"
    "jnz d -5"   ; Set d to 0, a to (a + (d*b)), c to 0
    "dec b"
    "cpy b c"
    "cpy c d"    ; 48 5 5 5
    "dec d"
    "inc c"
    "jnz d -2"   ; Set d to 0, c = c + d = 48 5 10 0
    "tgl c"
    "cpy -16 c"
    "jnz 1 c"    ; 394 1 1 0
    "cpy 75 c"   ; 394 1 75 0
    "jnz 97 d"   ; 394 1 75 97
    "inc a"
    "inc d"
    "jnz d -2"   ; 491 1 75 0
    "inc c"      ; 491 1 74 0
    "jnz c -5")) ;

#;(define input '("cpy 2 a"
                "tgl a"
                "tgl a"
                "tgl a"
                "cpy 1 a"
                "dec a"
                "dec a"))

(define last-line (length input))
last-line
(define program-counter 0)

(struct instruction (fun name args) #:transparent)

(define (get-num r x)
  (let ([try-num (string->number x)])
    (or try-num (hash-ref r x))))

(define cpy
  '(lambda (i r pc x y)
     (if (string->number y)
         (values i r (+ 1 pc))
         (values i
                 (hash-set r y (get-num r x))
                 (+ 1 pc)))))

(define jnz
  '(lambda (i r pc x y)
     (let ([x (get-num r x)]
           [y (get-num r y)])
       (if (= 0 x)
           (values i r (+ 1 pc))
           (values i r (+ pc y))))))

(define inc
  '(lambda (i r pc x)
     (if (string->number x)
         (values i r (+ 1 pc))
         (values i
                 (hash-set r x (+ 1 (hash-ref r x)))
                 (+ 1 pc)))))

(define dec
  '(lambda (i r pc x)
     (if (string->number x)
         (values i r (+ 1 pc))
         (values i (hash-set r x (- (hash-ref r x) 1)) (+ 1 pc)))))

(define tgl
  '(lambda (i r pc x)
     (let ([x (get-num r x)])
       (cond [(>= (+ x pc) (length i))
              (values i r (+ 1 pc))]
             [else
              (define inst (list-ref i (+ x pc)))
              (match-define (instruction fun name args) inst)
              (values (if (= (length (first (rest fun))) 4)
                          ; 1-argument function
                          (if (equal? name "inc")
                              (list-set i (+ x pc) (instruction dec "dec" args))
                              (list-set i (+ x pc) (instruction inc "inc" args)))
                          ; 2-argument function
                          (if (equal? name "jnz")
                              (list-set i (+ x pc) (instruction cpy "cpy" args))
                              (list-set i (+ x pc) (instruction jnz "jnz" args))))
                      r (+ 1 pc))]))))

(define instructions
  (for/list ([line (in-list input)])
    (match-define (list command args ...) (regexp-split " " line))
    (instruction (eval (string->symbol command) (namespace-anchor->namespace a)) command args)))

(begin-encourage-inline
  (define (run i r pc)
;    (call-with-exception-handler
;     (λ(x) (values i r (+ pc 1)))
;     (λ()
       (let ([inst (list-ref i pc)])
         ;(displayln (instruction-fun inst))
         (apply (eval (instruction-fun inst) (namespace-anchor->namespace a))
                (append (list i r pc) (instruction-args inst))))))

(for/fold ([i instructions]
           [registers (hash "a" 12 "b" 0 "c" 0 "d" 0)]
           [PC program-counter])
          ([x (in-range 0 10 0)])
  #:break (or (PC . >= . last-line) (PC . < . 0))
  ;(displayln PC)
  ;(displayln registers)
  ;(displayln (map instruction-name instructions))
  (run i registers PC))
