#lang racket

;; Done, but still:
;; TODO: Why do I get duplicates in my visited list? I thought I didn't, but I did.

(require srfi/60)

(define FAVORITE-NUMBER 1364)
;(define FAVORITE-NUMBER 10)

(define (reached-goal? x y)
  (and (= x 31) (= y 39)))

#;(define (reached-goal? x y)
  (and (= x 7) (= y 4)))

;x*x + 3*x + 2*x*y + y + y*y

(define (space? x y)
  (even? (bit-count
          (+ FAVORITE-NUMBER
             (* x x) (* 3 x) (* 2 x y) y (* y y)))))

(define (space-pt? pt)
  (space? (first pt) (second pt)))

;                                                              
;                                                              
;                                                              
;                                                              
;  ;;;;;;                          ;;                   ;;     
;  ;;;;;;;                         ;;                   ;;     
;  ;;    ;;                        ;;                  ;;;     
;  ;;    ;;    ;;;;    ;;;; ;;;; ;;;;;;;;            ;;;;;     
;  ;;    ;;  ;;;;;;;;  ;;;;;;;;; ;;;;;;;;            ;; ;;     
;  ;;;;;;;;   ;    ;;    ;;;       ;;                   ;;     
;  ;;;;;;      ;;;;;;    ;;        ;;                   ;;     
;  ;;         ;;;;;;;    ;;        ;;                   ;;     
;  ;;        ;;    ;;    ;;        ;;   ;               ;;     
;  ;;        ;;   ;;;    ;;        ;;   ;;              ;;     
;  ;;        ;;;;;;;;  ;;;;;;       ;;;;;             ;;;;;;   
;  ;;         ;;;; ;;  ;;;;;;        ;;;              ;;;;;;   
;                                                              
;                                                              
;                                                              
;                                                              


;; DFS version, won't work, need to change so that it terminates
(define (+! a . b)
  (if (or (not a) (member #f b))
      #f
      (apply + (cons a b))))
(define (min! a . b)
  (apply min (filter (λ(x) x) (cons a b))))

(define (maze-search-dfs)
  (define (dfs x y)
    (cond [(reached-goal? x y)
           1]
          [(not (space? x y))
           #f]
          [else
           (min! (+! 1 (dfs (+ x 1) y))
                 (+! 1 (dfs (- x 1) y))
                 (+! 1 (dfs x (+ y 1)))
                 (+! 1 (dfs x (- y 1))))]))
  (dfs 1 1))


;; BFS version, works
;; The reason we have to subtract 1 is that our starting point is in the
;; to-explore list (aka next-list), and we get further starting points from that one.
;; So by the time that point enters our visited list, for example, if '(1 1) were our
;; goal, the goal reaches our visited list in the second iteration, where steps is 1,
;; where we've actually taken zero steps.
(define (get-variations pt)
  (match-define (list x y) pt)
  (filter
   (λ(x) (andmap (λ(x) (not (negative? x))) x))
   `((,(+ x 1) ,y)
     (,(- x 1) ,y)
     (,x ,(+ y 1))
     (,x ,(- y 1)))))

(define (maze-search-bfs)
  (define (bfs num-moves next-list visited)
    (if (not (empty? (filter (λ(pt) (reached-goal? (first pt) (second pt)))
                             visited)))
        (- num-moves 1)
        (bfs
         (+ 1 num-moves)
         (append-map
          (λ(pt)
            (filter (λ(x)
                      (and (space-pt? x) (not (member x visited))))
                    (get-variations pt)))
          next-list)
         (append next-list visited))))
  (bfs 0 '((1 1)) '()))



           
;; Display map
#;(define x
  (map (λ(xy)
       (match-define (list y x) xy)
       (when (= x 0)
         (display "\n"))
       (if (space? x y)
           (display ".")
           (display "#")))
     (cartesian-product (range 7) (range 10))))



;                                                              
;                                                              
;                                                              
;                                                              
;  ;;;;;;                          ;;                  ;;;;    
;  ;;;;;;;                         ;;                 ;;;;;;;  
;  ;;    ;;                        ;;                 ;;   ;;  
;  ;;    ;;    ;;;;    ;;;; ;;;; ;;;;;;;;            ;;    ;;  
;  ;;    ;;  ;;;;;;;;  ;;;;;;;;; ;;;;;;;;            ;;    ;;  
;  ;;;;;;;;   ;    ;;    ;;;       ;;                     ;;   
;  ;;;;;;      ;;;;;;    ;;        ;;                    ;;    
;  ;;         ;;;;;;;    ;;        ;;                   ;;     
;  ;;        ;;    ;;    ;;        ;;   ;              ;;      
;  ;;        ;;   ;;;    ;;        ;;   ;;            ;;   ;;  
;  ;;        ;;;;;;;;  ;;;;;;       ;;;;;            ;;;;;;;;  
;  ;;         ;;;; ;;  ;;;;;;        ;;;             ;;;;;;;;  
;                                                              
;                                                              
;                                                              
;                                                              


;; Similar to the error in part 1, we have to start with -1, because
;; with the algo in the function, at zero iterations we've visited zero points,
;; and at 1 iteration we have visited '(1 1), where we've actually visited '(1 1)
;; at 0 iterations.


(define (max-pts)
  (define (bfs moves visited next)
    (if (= 50 moves)
        (length (remove-duplicates visited))
        (bfs
         (+ 1 moves)
         (append visited next)
         (append-map
          (λ(pt)
            (filter (λ(x)
                      (and (space-pt? x) (not (member x visited))))
                    (get-variations pt)))
          next))))
  (bfs -1 '() '((1 1))))