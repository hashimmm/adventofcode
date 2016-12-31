#lang typed/racket


(struct room ([enc-name : String]
              [sector-id : Number]
              [checksum : String]) #:transparent)


(define (rclist->number [lons : (Listof Char)])
  (define (ctoi [c : Char]) (- (char->integer c) 48))
  (foldl (λ ([x : Char] [n : Number] [t : Number])
          (+ (* (ctoi x) n) t))
        0
        lons
        (map (λ ([x : Number]) (expt 10 x)) (range (length lons)))))


(define (make-room [desc : String]) : room
  (define char-cdr (inst cdr Char Char))
  (define rev-desc (reverse (string->list desc)))
  (define-values (rev-cs rem)
    (splitf-at (char-cdr rev-desc) (λ ([x : Char]) (not (equal? x #\[)))))
  (define-values (rev-id rem2)
    (splitf-at (char-cdr rem) (λ ([x : Char]) (not (equal? x #\-)))))
  (define rev-name (drop rem2 1))
  (room (list->string (reverse rev-name))
        (rclist->number rev-id)
        (list->string (reverse rev-cs))))


(define (find-checksum [name : String])
  (define counts-table
    (sequence-fold
     (λ ([h : (HashTable Char Integer)] [x : Char])
       (if (equal? x #\-)
           h
           (hash-update h x
                        (λ ([x : Integer]) (+ x 1))
                        (λ () 0))))
     (ann (hash) (HashTable Char Integer))
     (in-string name)))
  (define counts-list (hash->list counts-table))

  (list->string
   (map (inst car Char Integer)
        (take (sort counts-list
                    (λ ([x : (Pairof Char Integer)]
                        [y : (Pairof Char Integer)])
                      (cond [(= (cdr x) (cdr y)) (char<? (car x) (car y))]
                            [else (> (cdr x) (cdr y))])))
              5))))


(define (real-room [r : room])
  (match-define (room enc-name sector-id checksum) r)
  (cond [(equal? (find-checksum enc-name) checksum) r]
        [else #f]))


(sequence-fold (λ ([i : Number] [x : String])
                 (define r (real-room (make-room x)))
                 (if r
                     (+ i (room-sector-id r))
                     i))
               0
               (in-port read-line))
