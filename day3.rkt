#lang racket

(define (layer idx)
  (define root (ceiling (sqrt idx)))
  (inexact->exact
   (floor
    (/ root 2))))

(define (distance idx)
  ;; How many layers away from the centre.
  (let* ([idx-layer (layer idx)]
         ;; The number of elements on one side of this layer.
         [side-length (+ (* 2 idx-layer) 1)]
         ;; The highest value in this ring.
         [layer-max-value (sqr side-length)]
         ;; Calculate our distance from the midpoint of a side of this layer.
         ;; The distance from the max value in this layer.
         [offset-from-max (- layer-max-value idx)]
         ;; The distance to the next corner, which may be further away than the previous corner.
         [offset-from-next-corner (if (zero? (sub1 side-length))
                                      0
                                      (modulo offset-from-max (sub1 side-length)))]
         [offset-from-corner (min
                              (abs (- offset-from-next-corner (sub1 side-length)))
                              offset-from-next-corner)]
         [offset-from-mid (abs (- offset-from-corner idx-layer))])
    (+ idx-layer offset-from-mid)))

(distance 312051)
