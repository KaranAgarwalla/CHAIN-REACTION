#lang racket
(require 2htdp/universe)
(require 2htdp/image)
(require "settings.rkt")
(provide (all-defined-out))

;(define BACK-COLOR 'red)
;(define SCALE 0.69)
(define CELL-SIZE (* SCALE 100))
(define EMPTY-CIRCLE (circle (/ (- CELL-SIZE 4) 2) 'solid BACK-COLOR))
(define RADIUS (* SCALE 25))
(define HALF (/ RADIUS 2))

(define red4 (overlay/offset (overlay/offset
   (overlay/offset (circle RADIUS 'solid (color 255 0 0 255))
                   RADIUS 0
                   (circle RADIUS 'solid (color 255 80 80 255)))
   HALF RADIUS
   (circle RADIUS 'solid (color 255 105 105 255))) (- HALF) HALF
  (circle RADIUS 'solid (color 255 130 130 255))))
(define red3 (overlay/offset
   (overlay/offset (circle RADIUS 'solid (color 255 0 0 255))
                   RADIUS 0
                   (circle RADIUS 'solid (color 255 90 90 255)))
   0 RADIUS
   (circle RADIUS 'solid (color 255 110 110 255))))
(define red2 
   (overlay/offset (circle RADIUS 'solid (color 255 0 0 255))
                   RADIUS 0
                   (circle RADIUS 'solid (color 255 100 100 255))))
(define red1 (circle RADIUS 'solid (color 255 0 0 255)))

(define blue4 (overlay/offset (overlay/offset
   (overlay/offset (circle RADIUS 'solid (color 0 0 255 255))
                   RADIUS 0
                   (circle RADIUS 'solid (color 80 80 255 255)))
   HALF RADIUS
   (circle RADIUS 'solid (color 105 105 255 255))) (- HALF) HALF
  (circle RADIUS 'solid (color 130 130 255 255))))
(define blue3 (overlay/offset
   (overlay/offset
    (circle RADIUS 'solid (color 0 0 255 255))
                   RADIUS 0
                   (circle RADIUS 'solid (color 80 80 255 255)))
   0 RADIUS
   (circle RADIUS 'solid (color 100 100 255 255))))
(define blue2 (overlay/offset (circle RADIUS 'solid (color 0 0 255 255))
                   RADIUS 0
                   (circle RADIUS 'solid (color 80 80 255 255))))
(define blue1 (circle RADIUS 'solid (color 0 0 255 255)))


(define green4 (overlay/offset (overlay/offset
   (overlay/offset (circle RADIUS 'solid (color 0 255 0 255))
                   RADIUS 0
                   (circle RADIUS 'solid (color 100 255 100 255)))
   HALF RADIUS
   (circle RADIUS 'solid (color 135 255 135 255))) (- HALF) HALF
  (circle RADIUS 'solid (color 160 255 160 255))))
(define green3 (overlay/offset
   (overlay/offset (circle RADIUS 'solid 'limegreen)
                   RADIUS 0
                   (circle RADIUS 'solid 'green))
   0 RADIUS
   (circle RADIUS 'solid 'lawngreen)))
(define green2 (overlay/offset (circle RADIUS 'solid 'limegreen)
                   RADIUS 0
                   (circle RADIUS 'solid 'green)))
(define green1 (circle RADIUS 'solid 'limegreen))


(define yellow4 (overlay/offset (overlay/offset
   (overlay/offset (circle RADIUS 'solid 'yellow)
                   RADIUS 0
                   (circle RADIUS 'solid (color 245 245 0 255)))
   HALF RADIUS
   (circle RADIUS 'solid (color 240 240 0 255))) (- HALF) HALF
  (circle RADIUS 'solid (color 230 230 0 255))))
(define yellow3 (overlay/offset
   (overlay/offset (circle RADIUS 'solid 'yellow)
                   RADIUS 0
                   (circle RADIUS 'solid (color 245 245 0 255)))
   0 RADIUS
   (circle RADIUS 'solid (color 230 230 0 255))))
(define yellow2 (overlay/offset (circle RADIUS 'solid 'yellow)
                   RADIUS 0
                   (circle RADIUS 'solid (color 245 245 0 255))))
(define yellow1 (circle RADIUS 'solid 'yellow))
   
(define (orb-model colour x)
      (overlay/align 'center 'center
                     (cond [(equal? colour 'red) (cond [(= x 1) red1]
                                                       [(= x 2) red2]
                                                       [(= x 3) red3]
                                                       [(= x 4) red4]
                                                       [else red4])]
                           [(equal? colour 'blue) (cond [(= x 1) blue1]
                                                        [(= x 2) blue2]
                                                        [(= x 3) blue3]
                                                        [(= x 4) blue4]
                                                        [else blue4])]
                           [(equal? colour 'green) (cond [(= x 1) green1]
                                                         [(= x 2) green2]
                                                         [(= x 3) green3]
                                                         [(= x 4) green4]
                                                         [else green4])]
                           [(equal? colour 'yellow) (cond [(= x 1) yellow1]
                                                          [(= x 2) yellow2]
                                                          [(= x 3) yellow3]
                                                          [(= x 4) yellow4]
                                                          [else yellow4])]
                           [else (rectangle 0 0 'solid 'black)]) EMPTY-CIRCLE))