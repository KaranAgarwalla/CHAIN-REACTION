#lang racket
(provide (all-defined-out))
(require "settings.rkt")
;Strategies applied to write board value
;;;;If the board is a won game, the value is 10000.
;;;;If the board is a lost game, the value is -10000.
;;;;For every non-critical orb, for every enemy critical cell surrounding the orb, subtract 4 minus the critical mass of that cell from the value.
;;;;For every critical orb, for every enemy critical cell surrounding the orb, add 7 minus the critical mass of that cell from the value if the next turn to be played is of the same color as that of the board
;;;;For every critical orb, for every enemy critical cell surrounding the orb, subt 7 minus the critical mass of that cell from the value if the next turn to be played is of the opp color as that of the board
;;;;In case that the orb has no critical enemy cells in its adjacent cells at all, add 1 to the value if it is an edge cell or 2 if it is a corner cell.
;;;;For every orb of the player's color, add 1 to the value.
;;;;For every contiguous blocks of critical cells of the player's color, add 2*number of cells in the block to the score.
;;;;For corner cell give 3
;;;;For enemy corner call subtract 3

(define (make-2d-vector1 r c initial)
  (build-vector r (lambda (x) (make-vector c initial))))
(define (2d-vector-ref1 vec x y)
  (vector-ref (vector-ref vec y) x))
(define (2d-vector-set1! vec x y val)
  (let([v (vector-ref vec y)])
    (vector-set! v x val)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (opp-color color)
  (if(equal? color PLAYER1-COLOR) PLAYER2-COLOR
     PLAYER1-COLOR))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define m M)
(define n N)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define(getCvalue x y vect)
  (car (cdr (cdr (2d-vector-ref1 vect x y)))))
(define(getcolor x y vect)
  (car (2d-vector-ref1 vect x y)))
(define(getvalue x y vect)
  (car (cdr (2d-vector-ref1 vect x y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define(contiguous-blocks x y)
  (define(f1 l)
    (if(< (+ x 1) n) (cons (cons (+ x 1) y) l) l))
  (define(f2 l)
    (if(>= (- x 1) 0) (cons (cons (- x 1) y) l) l))
  (define(f3 l)
    (if(< (+ y 1) m) (cons (cons x (+ y 1)) l) l))
  (define(f4 l)
    (if(>= (- y 1) 0) (cons (cons x (- y 1)) l) l))
  (f1 (f2 (f3 (f4 '())))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define(board-value color vect c)
  ;;;;;;;;;;;;;;;;;;;;;;
  (define(won-game)
    (define (f1 y-count)                                                       ;f1 and f2 functions are made to iterate over whole vector
      (define (f2 x-count)
        (if(= x-count n) (f1 (+ y-count 1))                                    ;in win game situation it returns 10000 else 0
           (if(equal? (getcolor x-count y-count vect) (opp-color color)) 0
              (f2 (+ x-count 1)))))
      (if(= y-count m) 10000 
         (f2 0)))
    (f1 0))
  ;;;;;;;;;;;;;;;;;;;;;;
  (define(lost-game)
    (define (f1 y-count)
      (define (f2 x-count)                                                     ;f1 and f2 functions are made to iterate over whole vector
        (if(= x-count n) (f1 (+ y-count 1))
           (if(equal? color (getcolor x-count y-count vect)) 0                 ;in loose game situation it returns -10000 else 0 
              (f2 (+ x-count 1)))))
      (if(= y-count m) -10000 
         (f2 0)))
    (f1 0))
  ;;;;;;;;;;;;;;;;;;;;;;;
  (define(orb-of-same-color)
    (define(f1 y-count)
      (define (f2 x-count)                         
        (if(= x-count n) (f1 (+ y-count 1))
           (if(equal? color (getcolor x-count y-count vect)) (+ (getvalue x-count y-count vect) (f2 (+ x-count 1)))
              (f2 (+ x-count 1)))))
      (if(= y-count m) 0
         (f2 0)))
    (f1 0))
  ;;;;;;;;;;;;;;;;;;;;;;;
  (define(contiguous-critical-cells)
    (define(f1 y-count)
      (define (f2 x-count)                                          ;f1 and f2 functions are made to iterate over whole vector
        (if(= x-count n) (f1 (+ y-count 1))
           (let([l (contiguous-blocks x-count y-count)])
             (define(f3 l1 no-critical-cells?)
               (if(null? l1)
                  (if no-critical-cells?    (cond[(= (length l) 2) 2]
                                                 [(= (length l) 3) 1]                 ;it basically computes all those strategies mentioned above that are related to
                                                 [else 0])                            ;contiguous critical cells
                      0)
                                            
                  (let([temp (getvalue (car (car l1)) (cdr (car l1)) vect)])
                    (if(= temp (getCvalue (car (car l1)) (cdr (car l1)) vect))
                       (if(equal? color (getcolor (car (car l1)) (cdr (car l1)) vect)) (+ (* 2 temp) (f3 (cdr l1) no-critical-cells?))
                          (if(= (getvalue x-count y-count vect) (getCvalue x-count y-count vect)) (if(= c 0) (- (f3 (cdr l1) #f) (- 7 temp)) (+ (f3 (cdr l1) #f) (- 7 temp)))
                                                                                                      (- (f3 (cdr l1) #f) (- 4 temp))))
                       (f3 (cdr l1) no-critical-cells?)))))
             (if(equal? color (getcolor x-count y-count vect)) (+ (f3 l #t) (f2 (+ x-count 1)))
                (f2 (+ x-count 1))))))
      (if(= y-count m) 0
         (f2 0)))
    (f1 0))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define(corner-cell)                                ;it evaluates the strategy related to corner cell
    (let([sum 0])
      (begin (if(equal? color (getcolor 0 0 vect)) (set! sum (+ sum 3)) (if(equal? color 'empty) (set! sum sum) (set! sum (- sum 3))))
             (if(equal? color (getcolor (- n 1) 0 vect)) (set! sum (+ sum 3)) (if(equal? color 'empty) (set! sum sum) (set! sum (- sum 3))))
             (if(equal? color (getcolor (- n 1) (- m 1) vect)) (set! sum (+ sum 3)) (if(equal? color 'empty) (set! sum sum) (set! sum (- sum 3))))
             (if(equal? color (getcolor 0 (- m 1) vect)) (set! sum (+ sum 3)) (if(equal? color 'empty) (set! sum sum) (set! sum (- sum 3))))
             sum)))
  (+ (won-game) (lost-game) (orb-of-same-color) (contiguous-critical-cells) (corner-cell)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define(quiescent-state color vect)
  (define(f1 x y)                                                ;it returns the value of maximum number of enemy cells that will break when any critical cell is break down
    (let([v (make-2d-vector1 m n #f)]                            ;v stores #t and #f
         [opp-chain 0])                                          ;#t implies the cell with that coordinate will break down and vice-versa for #f 
      (define(f2 x1 y1)                                
        (let([l (contiguous-blocks x1 y1)])                      ;f2 substitutes initial vector with proper substituted values
          (define(f3 x3 y3)
            (let([l2 (contiguous-blocks x3 y3)])
              (define (f5 l3)
                (if(null? l3) 0
                   (if(equal? (2d-vector-ref1 v (car (car l3)) (cdr (car l3))) #t) (+ 1 (f5 (cdr l3)))
                      (f5 (cdr l3)))))
              (f5 l2)))
          (define(f4 l1)
            (if(null? l1) 0
               (let([x2 (car (car l1))]
                    [y2 (cdr (car l1))])
                 (if(equal? (2d-vector-ref1 v x2 y2) #t) (f4 (cdr l1))
                    (if(> (+ (f3 x2 y2) (getvalue x2 y2 vect)) (getCvalue x2 y2 vect)) (begin(2d-vector-set1! v x2 y2 #t)
                                                                                   (f4 (cdr l1))
                                                                                   (f2 x2 y2))
                       (f4 (cdr l1)))))))
          (f4 l)))
      
      (define (count1 y-count)                                           ;count1 and count are made to iterate over the vector and hence return the no. of #t
        (define(count2 x-count)                                          ;values for enemy cells
          (if(= x-count n) (count1 (+ y-count 1))
             (if(equal? (2d-vector-ref1 v x-count y-count) #t)
                (if(not (equal? color (getcolor x-count y-count vect))) 
                   (begin(set! opp-chain (+ opp-chain 1))
                         (count2 (+ x-count 1))) (count2 (+ x-count 1)))
                (count2 (+ x-count 1)))))
        (if(= y-count m) 0
           (count2 0)))
           
      (define (f6)
        (if(equal? (2d-vector-ref1 v 0 0) #t)                          ;f6 is made to give extra importance to corner cells(counted as two)
           (if(not (equal? color (getcolor 0 0 vect))) 
              (set! opp-chain (+ opp-chain 1)) 0)
           0)
        (if(equal? (2d-vector-ref1 v (- n 1) 0) #t)
           (if(not (equal? color (getcolor (- n 1) 0 vect))) 
              (set! opp-chain (+ opp-chain 1)) 0)
           0)
        (if(equal? (2d-vector-ref1 v 0 (- m 1)) #t)
           (if(not (equal? color (getcolor 0 (- m 1) vect))) 
              (set! opp-chain (+ opp-chain 1)) 0)
           0)
        (if(equal? (2d-vector-ref1 v (- n 1) (- m 1)) #t)
           (if(not (equal? color (getcolor (- n 1) (- m 1) vect))) 
              (set! opp-chain (+ opp-chain 1)) 0)
           0))
      
      (if(and (equal? color (getcolor x y vect)) (= (getvalue x y vect) (getCvalue x y vect)))
         (begin (2d-vector-set1! v x y #t)
                (f2 x y)
                (count1 0)
                (f6)
                
                opp-chain)
         0)))
  (let([c 0]) 
    (define(f7 y-count)  
      (define(f8 x-count)
        (if(= x-count n) (f7 (+ y-count 1))
           (let([temp (f1 x-count y-count)])                           ;c stores the required value
             (if(> temp c) (begin (set! c temp) (f8 (+ x-count 1)))
                (f8 (+ x-count 1))))))
      (if(= y-count m) c
         (f8 0)))
    (f7 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define(decision color vect)
  (let([X -1]
       [Y -1]
       [value -100000]
       [init-time (current-inexact-milliseconds)])
    (define(change-vector x y color vect)
  (let([value (getvalue x y vect)]                                                ;change vector returns a vector with a move played at x and y of the color given as argument
       [Cvalue (getCvalue x y vect)])
    (if(> (- (current-inexact-milliseconds) init-time) 15000) 1
    (if(< value Cvalue) (begin (2d-vector-set1! vect x y (list color (+ value 1) Cvalue)) 0)
       (begin (if(= value Cvalue)
                 (2d-vector-set1! vect x y (list 'empty 0 Cvalue))
                 (2d-vector-set1! vect x y (list (getcolor x y vect) (- value Cvalue) Cvalue)))
              (let([l (contiguous-blocks x y)])
                (define(f1 l)
                  (if(null? l) 0
                     (begin (change-vector (car (car l)) (cdr (car l)) color vect)
                            (f1 (cdr l)))))
                (f1 l)))))))
    (define(proper-value color1 time-limit)                        ;proper value returns a value proportional to chances of winning
    
      (let([l1 '()]                       ; l1 c <= 1
           [l2 '()]                       ; l2 1 < c <= 3 
           [l3 '()]                       ; l3 3 < c <= 5
           [l4 '()]                       ; l4 5 < c 
           [init-time (current-inexact-milliseconds)]
           [ans (if(equal? color color1) -100000 100000)])                     
      
        (define (f1 x y)
          (define v (make-2d-vector1 m n 0))
          (define(copy1 y-count)                                      ;f1 and f2 combined helps to fill the lists l1 l2 l3 l4
            (define(f2 x-count)
              (if(= x-count n) (copy1 (+ y-count 1))
                 (begin(2d-vector-set1! v x-count y-count (2d-vector-ref1 vect x-count y-count))
                       (f2 (+ x-count 1)))))
            (if(= y-count m) (set! m m)
               (f2 0)))
          (define(copy2 y-count)
            (define(f2 x-count)
              (if(= x-count n) (copy2 (+ y-count 1))
                 (begin(2d-vector-set1! vect x-count y-count (2d-vector-ref1 v x-count y-count))
                       (f2 (+ x-count 1)))))
            (if(= y-count m) (set! m m)
               (f2 0)))
          (if(not (equal? (opp-color color1) (getcolor x y vect)))
             (begin (copy1 0)
                    (change-vector x y color1 vect)
                    (let([c1 (quiescent-state (opp-color color1) vect)]
                         [c (quiescent-state color1 vect)]
                         [b (board-value color vect (if(equal? color color1) 0 1))])
                      (cond [(and (<= c 1) (<= c1 2)) (set! l1 (cons (cons (cons x y) b) l1))]
                            [(<= c 3) (set! l2 (cons (cons (cons x y) b) l2))]
                            [(<= c 5) (set! l3 (cons (cons (cons x y) b) l3))]
                            [else (set! l4 (cons (cons (cons x y) b) l4))]))
                    (copy2 0))
             0))
        (define(f2 y-count)
          (define(f3 x-count)                            ;f2 and f3 functions are made to iterate over whole vector
            (if(= x-count n) (f2 (+ y-count 1))
               (begin
                 (f1 x-count y-count)
                 (f3 (+ x-count 1)))))
          (if(= y-count m) 0
             (f3 0)))
        (define(f4 l)                                   ;f4 runs when time-wasted is greater than new time limit.It returns proper value as the board value. 
          (if(null? l) 0
             (if(equal? color color1)
                (if(< ans (cdr (car l))) (begin (set! ans (cdr (car l)))
                                             (f4 (cdr l)))
                (f4 (cdr l)))
                (if(> ans (cdr (car l))) (begin (set! ans (cdr (car l)))
                                             (f4 (cdr l)))
                (f4 (cdr l))))))
        (define(f5 l new-time-limit)                     ;It runs when there is plenty of time.
          (define v (make-2d-vector1 m n 0))             ;So it calls recursion and maximise or minimise as per condition
          (define(copy1 y-count)
            (define(f2 x-count)
              (if(= x-count n) (copy1 (+ y-count 1))
                 (begin(2d-vector-set1! v x-count y-count (2d-vector-ref1 vect x-count y-count))
                       (f2 (+ x-count 1)))))
            (if(= y-count m) (set! m m)
               (f2 0)))
          (define(copy2 y-count)
            (define(f2 x-count)
              (if(= x-count n) (copy2 (+ y-count 1))
                 (begin(2d-vector-set1! vect x-count y-count (2d-vector-ref1 v x-count y-count))
                       (f2 (+ x-count 1)))))
            (if(= y-count m) (set! m m)
               (f2 0)))
          (if(null? l) 0
             (begin (copy1 0)
                    (if (= (change-vector (car (car (car l))) (cdr (car (car l))) color1 vect) 1) (begin (if(equal? color color1) (set! ans 100000) (set! ans -100000))
                                                                                                         (copy2 0))
                    (let([temp (if(or (and (< (cdr (car l)) 10500) (> (cdr (car l)) 9500))
                                      (and (> (cdr (car l)) -10500) (< (cdr (car l)) -9500))) (cdr (car l)) (proper-value (opp-color color1) new-time-limit))])
                      (copy2 0)
                      (if(equal? color color1) (if(< ans temp) (begin (set! ans temp)
                                             (f5 (cdr l) new-time-limit))
                         (f5 (cdr l) new-time-limit))
                         (if(> ans temp) (begin (set! ans temp)
                                             (f5 (cdr l) new-time-limit))
                         (f5 (cdr l) new-time-limit))))))))
        (f2 0)
        (let ([time-wasted (- (current-inexact-milliseconds) init-time)])     
          (let*([remaining-time (- time-limit time-wasted)]
                [new-time-limit (if(= (length l4) 0) 0 (+ 0.0 (/ (* 1 (/ remaining-time (length l4))) 3)))])   
            (if(> time-wasted new-time-limit) (f4 l4)
               (f5 l4 new-time-limit)))
          (let*([remaining-time (- time-limit (- (current-inexact-milliseconds) init-time))]
                [new-time-limit (if(= (length l3) 0) 0 (+ 0.0 (/ (* 1 (/ remaining-time (length l3))) 3)))])
            (if(> time-wasted new-time-limit) (f4 l3)
               (f5 l3 new-time-limit)))
          (let*([remaining-time (- time-limit (- (current-inexact-milliseconds) init-time))]
                [new-time-limit (if(= (length l2) 0) 0 (+ 0.0 (/ (* 1 (/ remaining-time (length l2))) 3)))])
            (if(> time-wasted new-time-limit) (f4 l2)
               (f5 l2 new-time-limit)))
          (let*([remaining-time (- time-limit (- (current-inexact-milliseconds) init-time))]
                [new-time-limit (if(= (length l1) 0) 0 (+ 0.0 (/ (* 1 (/ remaining-time (length l1))) 3)))])
            (if(> time-wasted new-time-limit) (f4 l1)
               (f5 l1 new-time-limit))))
        ans))
    (let([L1 '()]                    
         [L2 '()]                     
         [L3 '()]                      
         [L4 '()]                       
         [time-limit EXECUTION-TIME])
      (define(f6 y-count)
        (define(f7 x-count)
          (define (F1 x y)
            (define v (make-2d-vector1 m n 0))           ;f6 and f7 functions are made to iterate over whole vector
          (define(copy1 y-count)                         ;F1 and F2 combined helps to fill the lists L1 L2 L3 L4
            (define(f2 x-count)
              (if(= x-count n) (copy1 (+ y-count 1))
                 (begin(2d-vector-set1! v x-count y-count (2d-vector-ref1 vect x-count y-count))
                       (f2 (+ x-count 1)))))
            (if(= y-count m) (set! m m)
               (f2 0)))
          (define(copy2 y-count)
            (define(f2 x-count)
              (if(= x-count n) (copy2 (+ y-count 1))
                 (begin(2d-vector-set1! vect x-count y-count (2d-vector-ref1 v x-count y-count))
                       (f2 (+ x-count 1)))))
            (if(= y-count m) (set! m m)
               (f2 0)))
            (if(not (equal? (opp-color color) (getcolor x y vect)))
               (begin (copy1 0)
                 (change-vector x y color vect)
                      (let([c1 (quiescent-state (opp-color color) vect)]
                           [c (quiescent-state color vect)]
                           [b (board-value color vect 0)])
                        (copy2 0)
                        (cond [(and (<= c 1) (<= c1 2))
                               (set! L1 (cons (cons (cons x y) b) L1))]
                              [(<= c 3) (set! L2 (cons (cons (cons x y) b) L2))]
                              [(<= c 5) (set! L3 (cons (cons (cons x y) b) L3))]
                              [else (set! L4 (cons (cons (cons x y) b) L4))]))
                      )
               0))
          (if(= x-count n) (f6 (+ y-count 1))
             (begin
               (F1 x-count y-count)
               (f7 (+ x-count 1)))))

        (define(F4 l)                                  ;F4 works when there is shortage of time and proper value is simply assigned board value and maximised or minimised 
          (if(null? l) 0                               ;as per condition
             (if(< value (cdr (car l))) (begin
                                          (set! X (car (car (car l))))
                                          (set! Y (cdr (car (car l))))
                                                   
                                          (set! value (cdr (car l)))
                                          (F4 (cdr l)))
                (F4 (cdr l)))))
        (define(F5 l new-time-limit)                  ;It runs when there is sufficient time. 
          (define v (make-2d-vector1 m n 0))          ;It computes proper value of its children and maximise or minimise as per condition
          (define(copy1 y-count)
            (define(f2 x-count)
              (if(= x-count n) (copy1 (+ y-count 1))
                 (begin(2d-vector-set1! v x-count y-count (2d-vector-ref1 vect x-count y-count))
                       (f2 (+ x-count 1)))))
            (if(= y-count m) (set! m m)
               (f2 0)))
          (define(copy2 y-count)
            (define(f2 x-count)
              (if(= x-count n) (copy2 (+ y-count 1))
                 (begin(2d-vector-set1! vect x-count y-count (2d-vector-ref1 v x-count y-count))
                       (f2 (+ x-count 1)))))
            (if(= y-count m) (set! m m)
               (f2 0)))
          (if(null? l) 0
             (begin (copy1 0)
               (if (= (change-vector (car (car (car l))) (cdr (car (car l))) color vect) 1) (begin (set! X (car (car (car l))))
                                                                                                   (set! Y (cdr (car (car l))))
                                                                                                   (set! value 100000))
               (let([temp (if(or (and (< (cdr (car l)) 10500) (> (cdr (car l)) 9500))
                                      (and (> (cdr (car l)) -10500) (< (cdr (car l)) -9500))) (cdr (car l)) (proper-value (opp-color color) new-time-limit))])
                 (copy2 0)
                 (if(< value temp) (begin
                                     (set! X (car (car (car l))))
                                     (set! Y (cdr (car (car l))))
                                                    
                                     (set! value temp)
                                     (F5 (cdr l) new-time-limit))
                    (F5 (cdr l) new-time-limit))
                 )))))
             
             
             
         
        (if(= y-count m)
           (let ([time-wasted (- (current-inexact-milliseconds) init-time)])
             (let*([remaining-time (- time-limit time-wasted)]
                   [new-time-limit (if(= (length L4) 0) 0 (+ 0.0 (/ (* 1 (/ remaining-time (length L4))) 3)))])
               (if(> time-wasted new-time-limit) (F4 L4)
                  (F5 L4 new-time-limit)))
             (let*([remaining-time (- time-limit (- (current-inexact-milliseconds) init-time))]
                   [new-time-limit (if(= (length L3) 0) 0 (+ 0.0 (/ (* 1 (/ remaining-time (length L3))) 3)))])
               (if(> time-wasted new-time-limit) (F4 L3)
                  (F5 L3 new-time-limit)))
             (let*([remaining-time (- time-limit (- (current-inexact-milliseconds) init-time))]
                   [new-time-limit (if(= (length L2) 0) 0 (+ 0.0 (/ (* 1 (/ remaining-time (length L2))) 3)))])
               (if(> time-wasted new-time-limit) (F4 L2)
                  (F5 L2 new-time-limit)))
             (let*([remaining-time (- time-limit (- (current-inexact-milliseconds) init-time))]
                   [new-time-limit (if(= (length L1) 0) 0 (+ 0.0 (/ (* 1 (/ remaining-time (length L1))) 3)))])
               (if(> time-wasted new-time-limit) (F4 L1)
                  (F5 L1 new-time-limit)))
             (cons Y X))
           (f7 0)))
      (f6 0))))
    
      
      
      
      




               

