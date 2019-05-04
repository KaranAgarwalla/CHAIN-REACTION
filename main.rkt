#lang racket
(require 2htdp/universe)
(require 2htdp/image)
(require racket/mpair)
(require "orb-models.rkt")
(require "AI.rkt")
(require "settings.rkt")

(define (p x)
  (inexact->exact (floor x)))

(define (s x)
  (p (* SCALE x)))

(define PLAYER-COUNT 0)
(define COLOR-LST '())
(define COLOR-LISTS '())
(define GRID-WIDTH (* M CELL-SIZE))
(define GRID-HEIGHT (* N CELL-SIZE))
(define WIDTH (* SCALE 1400))
(define HEIGHT (* SCALE 700))
(define MAX-X (- M 1))
(define MAX-Y (- N 1))
(define COUNT 0)
(define TICK 0)
(define X '())
(define tempX '())
(define Y '())
(define tempY '())
(define AI-TURN #f)
(define PROCESS #t)
(define UNIT-CELL (crop 0 0 CELL-SIZE CELL-SIZE (rectangle CELL-SIZE
                    CELL-SIZE "outline" (make-pen GRID-COLOR 2 "solid" "round" "round"))))
(define mainmenu (place-image (scale SCALE (bitmap/file "mymain.png")) (/ WIDTH 2)
                       (/ HEIGHT 2) (rectangle WIDTH HEIGHT 'solid BACK-COLOR)))
(define numberofplayers (place-image (scale SCALE (bitmap/file "NUMBER OF PLAYERS.png")) (/ WIDTH 2)
                       (/ HEIGHT 2) (rectangle WIDTH HEIGHT 'solid BACK-COLOR)))
(define INSTRUCTIONS (place-image (scale SCALE (bitmap/file "INSTRUCTIONS.png")) (/ WIDTH 2)
                       (/ HEIGHT 2) (rectangle WIDTH HEIGHT 'solid BACK-COLOR)))

(define (make-grid m n)
  (apply above (make-list n (apply beside (make-list m UNIT-CELL)))))

(define GRID-OLD (place-image (make-grid (+ MAX-X 1) (+ MAX-Y 1)) (/ GRID-WIDTH 2)
                       (/ GRID-HEIGHT 2) (rectangle WIDTH HEIGHT 'solid BACK-COLOR)))

(define NEW-GRID (place-image (make-grid (+ MAX-X 1) (+ MAX-Y 1)) (/ GRID-WIDTH 2)
                       (/ GRID-HEIGHT 2) (rectangle WIDTH HEIGHT 'solid BACK-COLOR)))

(define GRID (place-image (make-grid (+ MAX-X 1) (+ MAX-Y 1)) (/ GRID-WIDTH 2)
                       (/ GRID-HEIGHT 2) (rectangle WIDTH HEIGHT 'solid BACK-COLOR)))

(define GRIDS '())

(define (make-2d-vector r c initial)
  (build-vector r (lambda (x) (make-vector c initial))))

(define (2d-vector-ref vec x y)
  (vector-ref (vector-ref vec (inexact->exact x)) (inexact->exact y)))

(define (2d-vector-set! vec x y val)
  (let ((v (vector-ref vec (inexact->exact x))))
    (begin (vector-set! v (inexact->exact y) val))))

(define (2d-vector->list v)
  (map vector->list (vector->list v)))

(define (2d-list->vector v)
  (list->vector (map list->vector v)))

(define (all-same? l)
  (cond [(null? l) #t]
        [(equal? (car l) #f) #f]
        [else (andmap (lambda (x) (equal? (car l) x)) l)]))

(define (list-equal? l)
  (if (null? l) #t (if (all-same? l) (car l) #f)))

(define (make-board-vector x y)
  (let ([lst1 (append (list (list 'empty 0 1))
                      (make-list (- y 2) (list 'empty 0 2))
                      (list (list 'empty 0 1)))]
        [lst2 (append (list (list 'empty 0 2))
                      (make-list (- y 2) (list 'empty 0 3))
                      (list (list 'empty 0 2)))])
    (2d-list->vector (append (list lst1) (make-list (- x 2) lst2) (list lst1)))))

(define (mcadr x)
  (mcar (mcdr x)))
(define (mcddr x)
  (mcdr (mcdr x)))

(define EMPTY-VEC (make-board-vector (+ MAX-X 1) (+ MAX-Y 1)))

(define VEC (make-board-vector (+ MAX-X 1) (+ MAX-Y 1)))
(define VECS '())

;;;CODE ADDITION
;;; GRID WITH PLAYER-COLORS
(define (new-grid) (set! NEW-GRID (cond [(= PLAYER-COUNT 0) GRID-OLD]
                       [(= PLAYER-COUNT 1) (place-image (text "PLAYER" (s 40) PLAYER1-COLOR) (s 1300) (s 80)
                                                                                (place-image (text "AI" (s 40) PLAYER2-COLOR) (s 1300) (s 160) GRID-OLD))]
                       [(= PLAYER-COUNT 2) (place-image (text "PLAYER" (s 40) PLAYER1-COLOR) (s 1300) (s 80)
                                                                                (place-image (text "PLAYER" (s 40) PLAYER2-COLOR) (s 1300) (s 160) GRID-OLD))]
                       [(= PLAYER-COUNT 3) (place-image (text "PLAYER" (s 40) PLAYER1-COLOR) (s 1300) (s 80)
                                                                                (place-image (text "PLAYER" (s 40) PLAYER2-COLOR) (s 1300) (s 160)
                                                                                             (place-image (text "PLAYER" (s 40) PLAYER3-COLOR) (s 1300) (s 240) GRID-OLD)))]
                       [(= PLAYER-COUNT 4) (place-image (text "PLAYER" (s 40) PLAYER1-COLOR) (s 1300) (s 80)
                                                                                (place-image (text "PLAYER" (s 40) PLAYER2-COLOR) (s 1300) (s 160)
                                                                                             (place-image (text "PLAYER" (s 40) PLAYER3-COLOR) (s 1300) (s 240)
                                                                                                          (place-image (text "PLAYER" (s 40) PLAYER4-COLOR) (s 1300) (s 320) GRID-OLD))))]))
  (set! GRID (place-image (text "UNDO" (s 40) OVERLAY-COLOR) (s 1300) (s 500)
                               (place-image (text "MAIN MENU" (s 30) OVERLAY-COLOR) (s 1300) (s 600)
                                            (place-image (rectangle (s 180) (s 80) "outline" OVERLAY-COLOR) (s 1300) (s 500)
                                                         (place-image (rectangle (s 180) (s 80) "outline" OVERLAY-COLOR) (s 1300) (s 600) NEW-GRID))))))

;;; GRID WITH UNDO BUTTON LOCATION RECTANGLE [(1210,500),(1390,600)]
;;; WINNING SCREEN FUNCTIONS
(define (win x overlay-grid)
  (win-frame (string-append "Player " (string-upcase (symbol->string x)) " Won") overlay-grid))

(define (win-frame sample-text grid-to)
  (place-image (text "Congratulations" (s 40) OVERLAY-COLOR) (s 510) (s 200)
               (place-image (text sample-text (s 30) OVERLAY-COLOR) (s 493) (s 280)
                            (place-image (text "RETURN TO MENU" (s 25) OVERLAY-COLOR) (s 500) (s 425)
                                         (place-image (text "EXIT" (s 25) OVERLAY-COLOR) (s 750) (s 425)
                                                      (place-image (rectangle (s 600) (s 300) 'solid (color 200 200 200 170)) (s 650) (s 300) grid-to))))))
;;;;
;;;; RESET FUNCTIONS
(define (reset)
  (set! PLAYER-COUNT 0)
  (set! COLOR-LST '())
  (set! COLOR-LISTS '())
  (set! COUNT 0)
  (set! TICK 0)  
  (set! X '())
  (set! tempX '())
  (set! Y '())
  (set! EMPTY-VEC (make-board-vector (+ MAX-X 1) (+ MAX-Y 1)))
  (set! VEC (make-board-vector (+ MAX-X 1) (+ MAX-Y 1)))
  (set! tempY '())
  (set! VECS '())
  (set! GRIDS '())
  (set! AI-TURN #f))
;;;
(define (diff l1 l2)
  (cond [(equal? l1 '()) l2]
        [(equal? l2 '()) l1]
        [(equal? (index-of l1 (car l2)) #f) (cons (car l2) (diff l1 (cdr l2)))]
        [else (diff (remove (car l2) l1) (cdr l2))]))

(define (display-turn)
  (place-image (text "TURN" (s 25) OVERLAY-COLOR) (s 1260) (s 400)
               (place-image (circle (s 30) 'solid (car COLOR-LST)) (s 1350) (s 400)
                            (place-image (rectangle (s 180) (s 80) 'outline OVERLAY-COLOR)
                                         (s 1300) (s 400) GRID))))
(define (draw-world state)
  (cond [(equal? state 'mainmenu) mainmenu]
        [(equal? state 'numberofplayers) numberofplayers]
        [(equal? state 'INSTRUCTIONS) INSTRUCTIONS]
        [(or (equal? state 'grid) (equal? state 'grid-transition))
         (cond [(null? COLOR-LISTS) (display-turn)]
               [(equal? COLOR-LST (mcar COLOR-LISTS)) (display-turn)]
                                    [else (let* ([current-color-list COLOR-LST]
                                                 [total-color-list (last (mlist->list COLOR-LISTS))]
                                                 [difference (diff current-color-list total-color-list)])
                                            (begin (if (member PLAYER1-COLOR difference) (set! GRID (add-line GRID (s 1220) (s 80) (s 1380) (s 80) OVERLAY-COLOR)) GRID)
                                                   (if (member PLAYER2-COLOR difference) (set! GRID (add-line GRID (s 1220) (s 160) (s 1380) (s 160) OVERLAY-COLOR)) GRID)
                                                   (if (member PLAYER3-COLOR difference) (set! GRID (add-line GRID (s 1220) (s 240) (s 1380) (s 240) OVERLAY-COLOR)) GRID)
                                                   (if (member PLAYER4-COLOR difference) (set! GRID (add-line GRID (s 1220) (s 320) (s 1380) (s 320) OVERLAY-COLOR)) GRID)
                                                   (display-turn)))])]
        [(equal? state 'stop) (let* ([total-color-list (last (mlist->list COLOR-LISTS))]
                                     [difference (remove (car COLOR-LST) total-color-list)])
                                            (begin (if (member PLAYER1-COLOR difference) (set! GRID (add-line GRID (s 1220) (s 80) (s 1380) (s 80) OVERLAY-COLOR)) GRID)
                                                   (if (member PLAYER2-COLOR difference) (set! GRID (add-line GRID (s 1220) (s 160) (s 1380) (s 160) OVERLAY-COLOR)) GRID)
                                                   (if (member PLAYER3-COLOR difference) (set! GRID (add-line GRID (s 1220) (s 240) (s 1380) (s 240) OVERLAY-COLOR)) GRID)
                                                   (if (member PLAYER4-COLOR difference) (set! GRID (add-line GRID (s 1220) (s 320) (s 1380) (s 320) OVERLAY-COLOR)) GRID)
                                                   (win (car COLOR-LST) GRID)))]
        [else GRID]))

(define (mouse-handler w x y action)
  (cond [(and (mouse=? action "button-down") (equal? w 'mainmenu))           (cond [(and (>= x (s 440)) (<= x (s 850)) (>= y (s 335)) (<= y (s 462))) 'numberofplayers]
                                                                                   [(and (>= x (s 388)) (<= x (s 912)) (>= y (s 500)) (<= y (s 646))) 'INSTRUCTIONS]
                                                                                   [(and (>= x (s 1225)) (<= x (s 1400)) (>= y (s 615)) (<= y (s 700))) (begin (reset) (stop-with 'mainmenu))]
                                                                                   [else w])]
                       
        [(and (mouse=? action "button-down") (equal? w 'numberofplayers))     (cond [(and (>= y (s 167)) (<= y (s 400)))
                                                                                     (cond [(and (>= x (s 72)) (<= x (s 259)))  (begin (set! PLAYER-COUNT 1)
                                                                                                                                (set! COLOR-LST (list PLAYER1-COLOR PLAYER2-COLOR)) (new-grid) 'grid)]
                                                                                           [(and (>= x (s 297)) (<= x (s 503))) (begin (set! PLAYER-COUNT 2)
                                                                                                                               (set! COLOR-LST (list PLAYER1-COLOR PLAYER2-COLOR)) (new-grid) 'grid)]
                                                                                           [(and (>= x (s 569)) (<= x (s 833))) (begin (set! PLAYER-COUNT 3)
                                                                                                                               (set! COLOR-LST (list PLAYER1-COLOR PLAYER2-COLOR PLAYER3-COLOR)) (new-grid) 'grid)]
                                                                                           [(and (>= x (s 910)) (<= x (s 1260))) (begin (set! PLAYER-COUNT 4)
                                                                                                                                (set! COLOR-LST (list PLAYER1-COLOR PLAYER2-COLOR PLAYER3-COLOR PLAYER4-COLOR)) (new-grid) 'grid)]
                                                                                           [else w])]
                                                                                    [(and (>= y (s 496)) (<= y (s 589)) (>= x (s 534)) (<= x (s 910))) 'mainmenu]
                                                                                    [else w])]
                       
        [(and (mouse=? action "button-down") (equal? w 'INSTRUCTIONS))           (cond [(and (>= y (s 545)) (<= y (s 640)))
                                                                                     (cond [(and (>= x (s 210)) (<= x (s 445))) 'numberofplayers]
                                                                                           [(and (>= x (s 814)) (<= x (s 1215))) 'mainmenu]
                                                                                   [else w])]
                                                                                   [else w])]
;; ADDED STOP
        [(and (mouse=? action "button-down") (equal? w 'stop))  (cond [(and (>= y (s 400)) (<= y (s 450)))
                                                                                     (cond [(and (>= x (s 425)) (<= x (s 675)))  (begin (reset) 'mainmenu)]
                                                                                           [(and (>= x (s 725)) (<= x (s 775))) (begin (reset) (stop-with 'mainmenu))]
                                                                                           [else w])]
                                                                                    [else w])]
;;ADDED STOP
        
        [(and (equal? w 'grid) (< x GRID-WIDTH) (< y GRID-HEIGHT) (equal? action "button-down") PROCESS)

         (cond [(and (equal? PLAYER-COUNT 1) (not AI-TURN))
             (begin0 (begin (set! GRIDS (mcons GRID GRIDS))
                (set! VECS (mcons (2d-vector->list VEC) VECS))
                (set! COLOR-LISTS (mcons COLOR-LST COLOR-LISTS))
                (update w (p (/ x CELL-SIZE)) (p (/ y CELL-SIZE)))) (set! AI-TURN #t))]
             [(equal? PLAYER-COUNT 1) (begin0 (begin (set! GRIDS (mcons GRID GRIDS))
                (set! VECS (mcons (2d-vector->list VEC) VECS))
                (set! COLOR-LISTS (mcons COLOR-LST COLOR-LISTS))
                (update w (p (/ x CELL-SIZE)) (p (/ y CELL-SIZE)))) (set! AI-TURN #f))]
             [else (begin (set! GRIDS (mcons GRID GRIDS))
                (set! VECS (mcons (2d-vector->list VEC) VECS))
                (set! COLOR-LISTS (mcons COLOR-LST COLOR-LISTS))
                (update w (p (/ x CELL-SIZE)) (p (/ y CELL-SIZE))))])
         ]
     
        [(and (equal? w 'grid) (not (null? GRIDS)) (not (null? VECS)) (> x (s 1210)) (< x (s 1390)) (< y (s 540)) (> y (s 460)) (equal? action "button-down")) (if (= PLAYER-COUNT 1)
                                                                                                                                                   (begin (set! VEC (2d-list->vector (mcadr VECS)))
                                                                                                                                                          (set! GRID (mcadr GRIDS))
                                                                                                                                                          (set! GRIDS (mcddr GRIDS))
                                                                                                                                                          (set! VECS (mcddr VECS))
                                                                                                                                                          (set! COUNT (- COUNT 2))
                                                                                                                                                          (set! COLOR-LST (mcadr COLOR-LISTS))
                                                                                                                                                          (set! COLOR-LISTS (mcddr COLOR-LISTS))
                                                                                                                                                          w)
                                                                                                                                                   (begin (set! VEC (2d-list->vector (mcar VECS)))
                                                                                                                                                          (set! GRID (mcar GRIDS))
                                                                                                                                                          (set! GRIDS (mcdr GRIDS))
                                                                                                                                                          (set! VECS (mcdr VECS))
                                                                                                                                                          (set! COUNT (- COUNT 1))
                                                                                                                                                          (set! COLOR-LST (mcar COLOR-LISTS))
                                                                                                                                                          (set! COLOR-LISTS (mcdr COLOR-LISTS))
                                                                                                                                                          w))]
        
        [(and (equal? w 'grid) (> x (s 1210)) (< x (s 1390)) (< y (s 640)) (> y (s 560)) (equal? action "button-down")) (begin (reset) 'mainmenu)]
        [else w]))

(define (invalid vect)
  (let* ([lst (2d-vector->list vect)]
        [ls (map (lambda (x) (map (lambda (y) (> (second y) (third y))) x)) lst)])
    (andmap (lambda (x) (andmap (lambda (y) y) x)) ls)))

(define (x-coordinates x)
  (inexact->exact (floor (- (/ WIDTH 2) (+ (* x CELL-SIZE) (/ CELL-SIZE 2))))))
(define (y-coordinates y)
  (inexact->exact (floor (- (/ HEIGHT 2) (+ (* y CELL-SIZE) (/ CELL-SIZE 2))))))

(define (update w x y)
  (cond [(equal? w 'grid)
         (let ([x-posn (x-coordinates x)]
               [y-posn (y-coordinates y)]
               [old-val (2d-vector-ref VEC x y)]
               [color (car COLOR-LST)])
           (if (or (equal? color (first old-val)) (equal? 'empty (first old-val)))
               (begin (set! COUNT (+ COUNT 1))
                      (set! COLOR-LST (append (cdr COLOR-LST) (list color)))
                      (cond [(< (second old-val) (third old-val)) 
                             (begin (2d-vector-set! VEC x y (list color (+ (second old-val) 1) (third old-val)))
                                    (let ([new-val (2d-vector-ref VEC x y)])
                                      (set! GRID (overlay/align/offset 'center 'center
                                                                       (orb-model (first new-val) (second new-val)) x-posn y-posn GRID))) w)]
                            [else (begin (update-grid x y) (set! X (list x)) (set! Y (list y)) 'grid-transition)
                                  ]))
               w))]))

(define(update-grid x y)
  (let([x-posn (x-coordinates x)]
       [y-posn (y-coordinates y)]
       [old-val (2d-vector-ref VEC x y)])
  (begin (2d-vector-set! VEC x y (list (last COLOR-LST) (+ (second old-val) 1) (third old-val)))
                               (let ([new-val (2d-vector-ref VEC x y)])
                                 (set! GRID (overlay/align/offset 'center 'center
                             (orb-model (first new-val) (second new-val)) x-posn y-posn GRID))))))

(define (all-colors-same)
  (cond [(and (not (equal? VEC EMPTY-VEC)) (> COUNT 2))
         (let* ([lst (2d-vector->list VEC)]
                [color-lst (map (lambda (x) (map car x)) lst)]
                [filtered (map (lambda (x) (remove* (list 'empty) x)) color-lst)]
                [lst (remove* (list #t) (map list-equal? filtered))])
          (if (index-of lst #f) #f (all-same? lst)))]
        [else #f]))

(define (filters)
  (cond [(>= COUNT (max PLAYER-COUNT 2))
         (let ([lst (2d-vector->list VEC)])
           (define (check colour)
             (andmap (lambda (x) (andmap (lambda (y) (not (equal? (car y) colour))) x)) lst))
           (begin (if (check PLAYER1-COLOR) (set! COLOR-LST (remove PLAYER1-COLOR COLOR-LST)) (void))
                  (if (check PLAYER2-COLOR) (set! COLOR-LST (remove PLAYER2-COLOR COLOR-LST)) (void))
                  (if (check PLAYER3-COLOR) (set! COLOR-LST (remove PLAYER3-COLOR COLOR-LST)) (void))
                  (if (check PLAYER4-COLOR) (set! COLOR-LST (remove PLAYER4-COLOR COLOR-LST)) (void))))]))            
                 
(define (tick-handle w)
    (define (helper x y)
      (let ([old-val (2d-vector-ref VEC x y)])
         (begin(if (= (second old-val) (+ 1 (third old-val)))
               (begin(2d-vector-set! VEC x y (list 'empty 0 (third old-val)))
                     (set! GRID (overlay/align/offset 'center 'center
                     EMPTY-CIRCLE (x-coordinates x) (y-coordinates y) GRID)))
               (begin (2d-vector-set! VEC x y (list (first old-val) (- (second old-val) (+ (third old-val) 1)) (third old-val)))
                               (let ([new-val (2d-vector-ref VEC x y)])
                                 (set! GRID (overlay/align/offset 'center 'center
                             (orb-model (first new-val) (second new-val)) (x-coordinates x) (y-coordinates y) GRID)))))
                         
           (cond [(= x 0) (cond [(= y 0) (let([val1 (2d-vector-ref VEC (+ x 1) y)]
                                         
                                              [val3 (2d-vector-ref VEC x (+ y 1))])
                                      
                                    (begin (if(not (= (remainder (+ (second val1) 1) (+ (third val1) 1)) 0)) (update-grid (+ x 1) y) (begin (update-grid (+ x 1) y)
                                                                                                            (set! tempX (cons (+ x 1) tempX))
                                                                                                            (set! tempY (cons y tempY))))
                                           
                                           (if(not (= (remainder (+ (second val3) 1) (+ (third val3) 1)) 0)) (update-grid x (+ y 1)) (begin (update-grid x (+ y 1))
                                                                                                            (set! tempX (cons x tempX))
                                                                                                            (set! tempY (cons (+ y 1) tempY))))
                                           )
                                            )]
                                 [(= y MAX-Y) (let([val1 (2d-vector-ref VEC (+ x 1) y)]
                                         
                                        
                                         [val4 (2d-vector-ref VEC x (- y 1))])
                                      
                                    (begin (if(not (= (remainder (+ (second val1) 1) (+ (third val1) 1)) 0)) (update-grid (+ x 1) y) (begin (update-grid (+ x 1) y)
                                                                                                            (set! tempX (cons (+ x 1) tempX))
                                                                                                            (set! tempY (cons y tempY))))
                                           
                                          
                                           (if(not (= (remainder (+ (second val4) 1) (+ (third val4) 1)) 0)) (update-grid x (- y 1)) (begin (update-grid x (- y 1))
                                                                                                            (set! tempX (cons x tempX))
                                                                                                            (set! tempY (cons (- y 1) tempY)))))
                                            )]
                                 [else (let([val1 (2d-vector-ref VEC (+ x 1) y)]
                                         
                                         [val3 (2d-vector-ref VEC x (+ y 1))]
                                         [val4 (2d-vector-ref VEC x (- y 1))])
                                      
                                    (begin (if(not (= (remainder (+ (second val1) 1) (+ (third val1) 1)) 0)) (update-grid (+ x 1) y) (begin (update-grid (+ x 1) y)
                                                                                                            (set! tempX (cons (+ x 1) tempX))
                                                                                                            (set! tempY (cons y tempY))))
                                           
                                           (if(not (= (remainder (+ (second val3) 1) (+ (third val3) 1)) 0)) (update-grid x (+ y 1)) (begin (update-grid x (+ y 1))
                                                                                                            (set! tempX (cons x tempX))
                                                                                                            (set! tempY (cons (+ y 1) tempY))))
                                           (if(not (= (remainder (+ (second val4) 1) (+ (third val4) 1)) 0)) (update-grid x (- y 1)) (begin (update-grid x (- y 1))
                                                                                                            (set! tempX (cons x tempX))
                                                                                                            (set! tempY (cons (- y 1) tempY)))))
                                            )])]
                  [(= x MAX-X) (cond [(= y 0) (let(
                                         [val2 (2d-vector-ref VEC (- x 1) y)]
                                         
                                         [val4 (2d-vector-ref VEC x (+ y 1))])
                                      
                                    (begin 
                                           (if(not (= (remainder (+ (second val2) 1) (+ (third val2) 1)) 0)) (update-grid (- x 1) y) (begin (update-grid (- x 1) y)
                                                                                                            (set! tempX (cons (- x 1) tempX))
                                                                                                            (set! tempY (cons y tempY))))
                                           
                                           (if(not (= (remainder (+ (second val4) 1) (+ (third val4) 1)) 0)) (update-grid x (+ y 1)) (begin (update-grid x (+ y 1))
                                                                                                            (set! tempX (cons x tempX))
                                                                                                            (set! tempY (cons (+ y 1) tempY)))))
                                            )]
                                     [(= y MAX-Y) (let(
                                         [val2 (2d-vector-ref VEC (- x 1) y)]
                                         
                                         [val4 (2d-vector-ref VEC x (- y 1))])
                                      
                                    (begin 
                                           (if(not (= (remainder (+ (second val2) 1) (+ (third val2) 1)) 0)) (update-grid (- x 1) y) (begin (update-grid (- x 1) y)
                                                                                                            (set! tempX (cons (- x 1) tempX))
                                                                                                            (set! tempY (cons y tempY))))
                                           
                                           (if(not (= (remainder (+ (second val4) 1) (+ (third val4) 1)) 0)) (update-grid x (- y 1)) (begin (update-grid x (- y 1))
                                                                                                            (set! tempX (cons x tempX))
                                                                                                            (set! tempY (cons (- y 1) tempY)))))
                                            )]
                                     [else (let(
                                         [val2 (2d-vector-ref VEC (- x 1) y)]
                                         [val3 (2d-vector-ref VEC x (+ y 1))]
                                         [val4 (2d-vector-ref VEC x (- y 1))])
                                      
                                    (begin 
                                           (if(not (= (remainder (+ (second val2) 1) (+ (third val2) 1)) 0)) (update-grid (- x 1) y) (begin (update-grid (- x 1) y)
                                                                                                            (set! tempX (cons (- x 1) tempX))
                                                                                                            (set! tempY (cons y tempY))))
                                           (if(not (= (remainder (+ (second val3) 1) (+ (third val3) 1)) 0)) (update-grid x (+ y 1)) (begin (update-grid x (+ y 1))
                                                                                                            (set! tempX (cons x tempX))
                                                                                                            (set! tempY (cons (+ y 1) tempY))))
                                           (if(not (= (remainder (+ (second val4) 1) (+ (third val4) 1)) 0)) (update-grid x (- y 1)) (begin (update-grid x (- y 1))
                                                                                                            (set! tempX (cons x tempX))
                                                                                                            (set! tempY (cons (- y 1) tempY)))))
                                            )])]
                  [else (cond [(= y 0) (let([val1 (2d-vector-ref VEC (+ x 1) y)]
                                         [val2 (2d-vector-ref VEC (- x 1) y)]
                                         [val3 (2d-vector-ref VEC x (+ y 1))])
                                         (begin (if(not (= (remainder (+ (second val1) 1) (+ (third val1) 1)) 0)) (update-grid (+ x 1) y) (begin (update-grid (+ x 1) y)
                                                                                                            (set! tempX (cons (+ x 1) tempX))
                                                                                                            (set! tempY (cons y tempY))))
                                           (if(not (= (remainder (+ (second val2) 1) (+ (third val2) 1)) 0)) (update-grid (- x 1) y) (begin (update-grid (- x 1) y)
                                                                                                            (set! tempX (cons (- x 1) tempX))
                                                                                                            (set! tempY (cons y tempY))))
                                           (if(not (= (remainder (+ (second val3) 1) (+ (third val3) 1)) 0)) (update-grid x (+ y 1)) (begin (update-grid x (+ y 1))
                                                                                                            (set! tempX (cons x tempX))
                                                                                                            (set! tempY (cons (+ y 1) tempY))))))]
                              [(= y MAX-Y) (let([val1 (2d-vector-ref VEC (+ x 1) y)]
                                         [val2 (2d-vector-ref VEC (- x 1) y)]
                                         [val4 (2d-vector-ref VEC x (- y 1))])
                                      
                                    (begin (if(not (= (remainder (+ (second val1) 1) (+ (third val1) 1)) 0)) (update-grid (+ x 1) y) (begin (update-grid (+ x 1) y)
                                                                                                            (set! tempX (cons (+ x 1) tempX))
                                                                                                            (set! tempY (cons y tempY))))
                                           (if(not (= (remainder (+ (second val2) 1) (+ (third val2) 1)) 0)) (update-grid (- x 1) y) (begin (update-grid (- x 1) y)
                                                                                                            (set! tempX (cons (- x 1) tempX))
                                                                                                            (set! tempY (cons y tempY))))
                                           (if(not (= (remainder (+ (second val4) 1) (+ (third val4) 1)) 0)) (update-grid x (- y 1)) (begin (update-grid x (- y 1))
                                                                                                            (set! tempX (cons x tempX))
                                                                                                            (set! tempY (cons (- y 1) tempY)))))
                                            )]
                              [else (let([val1 (2d-vector-ref VEC (+ x 1) y)]
                                         [val2 (2d-vector-ref VEC (- x 1) y)]
                                         [val3 (2d-vector-ref VEC x (+ y 1))]
                                         [val4 (2d-vector-ref VEC x (- y 1))])
                                      
                                    (begin (if(not (= (remainder (+ (second val1) 1) (+ (third val1) 1)) 0)) (update-grid (+ x 1) y) (begin (update-grid (+ x 1) y)
                                                                                                            (set! tempX (cons (+ x 1) tempX))
                                                                                                            (set! tempY (cons y tempY))))
                                           (if(not (= (remainder (+ (second val2) 1) (+ (third val2) 1)) 0)) (update-grid (- x 1) y) (begin (update-grid (- x 1) y)
                                                                                                            (set! tempX (cons (- x 1) tempX))
                                                                                                            (set! tempY (cons y tempY))))
                                           (if(not (= (remainder (+ (second val3) 1) (+ (third val3) 1)) 0)) (update-grid x (+ y 1)) (begin (update-grid x (+ y 1))
                                                                                                            (set! tempX (cons x tempX))
                                                                                                            (set! tempY (cons (+ y 1) tempY))))
                                           (if(not (= (remainder (+ (second val4) 1) (+ (third val4) 1)) 0)) (update-grid x (- y 1)) (begin (update-grid x (- y 1)) 
                                                                                                            (set! tempX (cons x tempX))
                                                                                                            (set! tempY (cons (- y 1) tempY)))))
                                            )])]))))
  (begin (if (equal? w 'grid-transition) (set! TICK (+ TICK 1)) 0)
         (cond  [(all-colors-same) (begin (set! COLOR-LST (list (last COLOR-LST))) 'stop)]
                [(and (= TICK 14) (equal? w 'grid-transition))
                (cond [(and (null? X) (null? Y)) (begin (set! TICK 0) 'grid)]
                      [else (begin (map (lambda (x y) (helper x y)) X Y) (set! TICK 0) (set! X tempX) (set! Y tempY) (set! tempX '()) (set! tempY '()) w)])]
        
        [(= (length COLOR-LST) 1) 'stop]
        [(and AI-TURN (equal? w 'grid)) (begin (set! PROCESS #f) (let ([k (decision PLAYER2-COLOR VEC)]) (begin (filters) (set! PROCESS #t) (if (= (length COLOR-LST) 1) 'stop
        (mouse-handler w (+ (* (car k) CELL-SIZE) (/ CELL-SIZE 2)) (+ (* (cdr k) CELL-SIZE) (/ CELL-SIZE 2)) "button-down")))))]
        [(equal? w 'grid) (begin (filters) w)]
        [else w])))

(big-bang 'mainmenu
  (to-draw draw-world)
  (on-mouse mouse-handler)
  (on-tick tick-handle)
  (close-on-stop #t)
  )