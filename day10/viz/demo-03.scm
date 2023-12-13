
;; chickadee play demo-03.scm

(define position (vec2 0.0 240.0))

(define (draw alpha)
  (draw-text "Hello, world!" position))

(define (update dt)
  (update-agenda dt))

(define (update-x x)
  (set-vec2-x! position x))

(let ((start 0.0)
      (end 536.0)
      (duration 4.0))
  (script
   (while #t
    (tween duration start end update-x)
    (tween duration end start update-x))))
