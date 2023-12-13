
;; chickadee play demo-02.scm

(define position (vec2 0.0 240.0))

(define (draw alpha)
  (draw-text "Hello, world!" position))

(define (update dt)
  (set-vec2-x! position (+ (vec2-x position) (* 100.0 dt))))
