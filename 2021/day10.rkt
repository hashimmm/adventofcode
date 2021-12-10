#lang racket

(define closers
  (hash #\( #\)
        #\[ #\]
        #\{ #\}
        #\< #\>))

(define (matching-closer? to-match token)
  (equal? (hash-ref closers
                    to-match
                    #f)
          token))

(define (opening? token)
  (member token '(#\( #\[ #\< #\{)))

(struct bad (token) #:transparent)

(define (parse-line/1 line)
  (define loc (string->list line))
  (define (parser state remaining)
    (if (null? remaining)
        state
        (let ([next (first remaining)])
          (if (opening? next)
              (parser (cons next state) (rest remaining))
              (cond [(null? state) (bad next)]
                    [(not (matching-closer? (first state) next)) (bad next)]
                    [else
                     (parser (rest state) (rest remaining))])))))
  (parser '() loc))

(define scores
  (hash #\) 3
        #\] 57
        #\} 1197
        #\> 25137))

(define (parse/1 inp)
  (define parsed-lines (map parse-line/1 inp))
  (for/sum ([line-or-bad (in-list parsed-lines)])
    (if (not (bad? line-or-bad))
        0
        (hash-ref scores (bad-token line-or-bad)))))

(define (get-closers state)
  (for/list ([c (in-list state)])
    (hash-ref closers c)))

(define (get-completion-score closer-chars)
  (for/fold ([score 0])
            ([c (in-list closer-chars)])
    (+ (* 5 score)
       (hash-ref (hash #\) 1
                       #\] 2
                       #\} 3
                       #\> 4)
                 c))))

(define (parse/2 inp)
  (define parsed-lines (map parse-line/1 inp))
  (define lines-only (filter (λ(x) (not (bad? x))) parsed-lines))
  (define completions (map get-closers lines-only))
  (define scores (map get-completion-score completions))
  (list-ref (sort scores <) (/ (sub1 (length scores)) 2)))

(module+ test
  (require rackunit)
  (define inp "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]")
  (check-equal? (parse/1 (string-split inp "\n")) 26397)
  (check-equal? (parse/2 (string-split inp "\n")) 288957))

(module+ main
  (define inp (with-input-from-file "day10.txt" port->lines))
  (parse/1 inp) ;; 392097
  (parse/2 inp) #;4263222782 )
