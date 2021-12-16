#lang racket

(require data/bit-vector)

(define (bv->num bv)
  (string->number (bit-vector->string bv) 2))

(define (parse-literal bv)
  (let loop ([remaining (bit-vector->list bv)]
             [size 0]
             [collected '()])
    (if (false? (first remaining))
        (Lit (bv->num (list->bit-vector (append collected (take (drop remaining 1) 4))))
             (+ size 5))
        (loop (drop remaining 5)
              (+ size 5)
              (append collected (take (drop remaining 1) 4))))))

(struct Pkt (v t size x) #:transparent)
(struct Lit (v size) #:transparent)

(define (parse-pkt packet)
  (define version (bv->num (bit-vector-copy packet 0 3)))
  (define type-id (bv->num (bit-vector-copy packet 3 6)))
  (define value
    (cond
      [(= type-id 4) (parse-literal (bit-vector-copy packet 6))]
      [else
       (let ([length-type (bit-vector-ref packet 6)])
         (if (false? length-type)
             (let ([total-length (bv->num (bit-vector-copy packet 7 (+ 7 15)))])
               (for/fold ([sub-pkts '()]
                          [size-offset 0]
                          #:result (reverse sub-pkts))
                         ([_ (in-naturals)])
                 #:break (= size-offset total-length)
                 (define p (parse-pkt (bit-vector-copy packet (+ 7 15 size-offset))))
                 (values (cons p sub-pkts)
                         (+ size-offset (Pkt-size p)))))
             (let ([total-sub-pkts (bv->num (bit-vector-copy packet 7 (+ 7 11)))])
               (for/fold ([sub-pkts '()]
                          [size-offset 0]
                          #:result (reverse sub-pkts))
                         ([_ (in-naturals)])
                 #:break (= (length sub-pkts) total-sub-pkts)
                 (define p (parse-pkt (bit-vector-copy packet (+ 7 11 size-offset))))
                 (values (cons p sub-pkts)
                         (+ size-offset (Pkt-size p)))))))]))
  (Pkt version
       type-id
       (if (= type-id 4)
           (+ 6 (Lit-size value))
           (apply + (cons (+ 6 1 (if (false? (bit-vector-ref packet 6)) 15 11))
                          (map Pkt-size value))))
       value))

(define (version-sum/1 pkt)
  (let loop ([so-far (Pkt-v pkt)]
             [values (Pkt-x pkt)])
    (if (or (null? values)
            (Lit? values))
        so-far
        (loop (+ so-far (version-sum/1 (first values)))
              (rest values)))))

(define (hex->bv hex-char)
  (string->bit-vector
   (~a (number->string (string->number (string hex-char) 16) 2)
       #:min-width 4 #:left-pad-string "0" #:align 'right)))

(define (parse inp)
  (define as-bvs (map hex->bv (string->list inp)))
  (define big-list (append-map bit-vector->list as-bvs))
  (list->bit-vector big-list))

(define (eval/2 pkt)
  (match pkt
    [(Pkt _ 4 _ v) (Lit-v v)]
    [(Pkt _ 0 _ lop)
     (apply + (map eval/2 lop))]
    [(Pkt _ 1 _ lop)
     (apply * (map eval/2 lop))]
    [(Pkt _ 2 _ lop)
     (apply min (map eval/2 lop))]
    [(Pkt _ 3 _ lop)
     (apply max (map eval/2 lop))]
    [(Pkt _ 5 _ lop)
     (if (> (eval/2 (first lop)) (eval/2 (second lop))) 1 0)]
    [(Pkt _ 6 _ lop)
     (if (< (eval/2 (first lop)) (eval/2 (second lop))) 1 0)]
    [(Pkt _ 7 _ lop)
     (if (= (eval/2 (first lop)) (eval/2 (second lop))) 1 0)]))

(module+ test
  (require rackunit)
  (check-equal? (version-sum/1 (parse-pkt (parse "8A004A801A8002F478"))) 16)
  (check-equal? (version-sum/1 (parse-pkt (parse "C0015000016115A2E0802F182340"))) 23)
  (check-equal? (version-sum/1 (parse-pkt (parse "620080001611562C8802118E34"))) 12)
  (check-equal? (version-sum/1 (parse-pkt (parse "A0016C880162017C3686B18A3D4780"))) 31)
  (check-equal? (eval/2 (parse-pkt (parse "9C0141080250320F1802104A08"))) 1)
  (check-equal? (eval/2 (parse-pkt (parse "CE00C43D881120"))) 9)
  (check-equal? (eval/2 (parse-pkt (parse "880086C3E88112"))) 7)
  (check-equal? (eval/2 (parse-pkt (parse "04005AC33890"))) 54)
  (check-equal? (eval/2 (parse-pkt (parse "9C005AC2F8F0"))) 0))

(module+ main
  (define pkt (parse-pkt (parse (first (with-input-from-file "day16.txt" port->lines)))))
  (version-sum/1 pkt) ;891
  (eval/2 pkt) #;673042777597 )
