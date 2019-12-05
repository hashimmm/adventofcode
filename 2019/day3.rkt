#lang racket

;; TODO: this works, but get-path is done at bad points in the code,
;; leading to duplicate work.

(require srfi/1)

(struct DVector (dir mtude) #:transparent)
(struct Pt (x y) #:transparent)

(define (parse-direction-vecs line)
  (map (λ(direction-str)
         (DVector (substring direction-str 0 1)
                  (string->number (substring direction-str 1))))
       (string-split line ",")))

(define (get-path directions)
  (for/fold ([path-so-far '()]
             [current-x 0]
             [current-y 0]
             #:result path-so-far)
            ([dvec (in-list directions)])
    (let* ([mtude (DVector-mtude dvec)]
           [dir (DVector-dir dvec)]
           [next-points
            (build-list mtude
                        (λ(idx)
                          (match dir
                            ["R"
                             (Pt (+ current-x 1 idx)
                                 current-y)]
                            ["L"
                             (Pt (- current-x 1 idx)
                                 current-y)]
                            ["D"
                             (Pt current-x
                                 (- current-y 1 idx))]
                            ["U"
                             (Pt current-x
                                 (+ current-y 1 idx))])))]
           [last-x
            (match dir ["D" current-x] ["U" current-x]
              ["R" (+ current-x mtude)] ["L" (- current-x mtude)])]
           [last-y
            (match dir ["R" current-y] ["L" current-y]
              ["U" (+ current-y mtude)] ["D" (- current-y mtude)])])
      (values (append path-so-far
                      next-points)
              last-x
              last-y))))

(define (get-closest-intersection wire1 wire2)
  (define common-points
    (get-intersections wire1 wire2))
  (define closest-pt
    (argmin (λ(pt) (+ (abs (Pt-x pt)) (abs (Pt-y pt)))) (set->list common-points)))
  (values closest-pt
          (+ (abs (Pt-x closest-pt)) (abs (Pt-y closest-pt)))))

(define (parse-and-get-closest-intersection line1 line2)
  (get-closest-intersection (parse-direction-vecs line1)
                            (parse-direction-vecs line2)))

(define (get-intersections wire1 wire2)
  (define wire1-points-set
    (list->set (get-path wire1)))
  (define wire2-points-set
    (list->set (get-path wire2)))
  (define common-points
    (set-intersect wire1-points-set wire2-points-set))
  common-points)

(define (length-path-to-point wire pt)
  (+ 1 (length (takef wire (λ(wire-pt) (not (equal? wire-pt pt)))))))

(define (get-intersection-with-fewest-combined-steps wire1 wire2)
  (define common-pts (set->list (get-intersections wire1 wire2)))
  (define path1 (get-path wire1))
  (define path2 (get-path wire2))
  (define distances-to-points
    (map (λ(common-pt)
           (cons (length-path-to-point path1 common-pt)
                 (length-path-to-point path2 common-pt)))
         common-pts))
  (define intersections-and-distances (zip common-pts distances-to-points))
  (argmin (λ(intersection-pt-and-distance)
            (+ (car (second intersection-pt-and-distance))
               (cdr (second intersection-pt-and-distance))))
          intersections-and-distances))

(define (parse-and-get-intersection-with-fewest-combined-steps line1 line2)
  (get-intersection-with-fewest-combined-steps (parse-direction-vecs line1)
                                               (parse-direction-vecs line2)))
