(reset)
(echo "trying nx = 12 : ny = 12 ")
(declare-fun width () Int)
(declare-fun height () Int)

(declare-fun x1 () Int)
(declare-fun x2 () Int)
(declare-fun x3 () Int)
(declare-fun x4 () Int)
(declare-fun x5 () Int)
(declare-fun x6 () Int)
(declare-fun x7 () Int)
(declare-fun x8 () Int)
(declare-fun x9 () Int)
(declare-fun x10 () Int)
(declare-fun x11 () Int)
(declare-fun x12 () Int)
(declare-fun y1 () Int)
(declare-fun y2 () Int)
(declare-fun y3 () Int)
(declare-fun y4 () Int)
(declare-fun y5 () Int)
(declare-fun y6 () Int)
(declare-fun y7 () Int)
(declare-fun y8 () Int)
(declare-fun y9 () Int)
(declare-fun y10 () Int)
(declare-fun y11 () Int)
(declare-fun y12 () Int)

(assert (= width 13))
(assert (= height 13))

(assert (or (= x1 -3) (= x1 -2) (= x1 -1) (= x1 3) (= x1 2) (= x1 1)))
(assert (or (= x2 -3) (= x2 -2) (= x2 -1) (= x2 3) (= x2 2) (= x2 1)))
(assert (or (= x3 -3) (= x3 -2) (= x3 -1) (= x3 3) (= x3 2) (= x3 1)))
(assert (or (= x4 -3) (= x4 -2) (= x4 -1) (= x4 3) (= x4 2) (= x4 1)))
(assert (or (= x5 -3) (= x5 -2) (= x5 -1) (= x5 3) (= x5 2) (= x5 1)))
(assert (or (= x6 -3) (= x6 -2) (= x6 -1) (= x6 3) (= x6 2) (= x6 1)))
(assert (or (= x7 -3) (= x7 -2) (= x7 -1) (= x7 3) (= x7 2) (= x7 1)))
(assert (or (= x8 -3) (= x8 -2) (= x8 -1) (= x8 3) (= x8 2) (= x8 1)))
(assert (or (= x9 -3) (= x9 -2) (= x9 -1) (= x9 3) (= x9 2) (= x9 1)))
(assert (or (= x10 -3) (= x10 -2) (= x10 -1) (= x10 3) (= x10 2) (= x10 1)))
(assert (or (= x11 -3) (= x11 -2) (= x11 -1) (= x11 3) (= x11 2) (= x11 1)))
(assert (or (= x12 -3) (= x12 -2) (= x12 -1) (= x12 3) (= x12 2) (= x12 1)))

(assert (or (= y1 -3) (= y1 -2) (= y1 -1) (= y1 3) (= y1 2) (= y1 1)))
(assert (or (= y2 -3) (= y2 -2) (= y2 -1) (= y2 3) (= y2 2) (= y2 1)))
(assert (or (= y3 -3) (= y3 -2) (= y3 -1) (= y3 3) (= y3 2) (= y3 1)))
(assert (or (= y4 -3) (= y4 -2) (= y4 -1) (= y4 3) (= y4 2) (= y4 1)))
(assert (or (= y5 -3) (= y5 -2) (= y5 -1) (= y5 3) (= y5 2) (= y5 1)))
(assert (or (= y6 -3) (= y6 -2) (= y6 -1) (= y6 3) (= y6 2) (= y6 1)))
(assert (or (= y7 -3) (= y7 -2) (= y7 -1) (= y7 3) (= y7 2) (= y7 1)))
(assert (or (= y8 -3) (= y8 -2) (= y8 -1) (= y8 3) (= y8 2) (= y8 1)))
(assert (or (= y9 -3) (= y9 -2) (= y9 -1) (= y9 3) (= y9 2) (= y9 1)))
(assert (or (= y10 -3) (= y10 -2) (= y10 -1) (= y10 3) (= y10 2) (= y10 1)))
(assert (or (= y11 -3) (= y11 -2) (= y11 -1) (= y11 3) (= y11 2) (= y11 1)))
(assert (or (= y12 -3) (= y12 -2) (= y12 -1) (= y12 3) (= y12 2) (= y12 1)))

(assert (> (+ x1) 0))
(assert (> (+ y1) 0))

(assert (> (+ x1 x2) 0))
(assert (> (+ y1 y2) 0))


;; (assert (and (> (+ x1) 0) (< (+ x1) (- width 1))))
;; (assert (and (> (+ x1 x2) 0) (< (+ x1 x2) (- width 1))))
;; (assert (and (> (+ x1 x2 x3) 0) (< (+ x1 x2 x3) (- width 1))))
;; (assert (and (> (+ x1 x2 x3 x4) 0) (< (+ x1 x2 x3 x4) (- width 1))))
;; (assert (and (> (+ x1 x2 x3 x4 x5) 0) (< (+ x1 x2 x3 x4 x5) (- width 1))))
;; (assert
;;   (and (> (+ x1 x2 x3 x4 x5 x6) 0) (< (+ x1 x2 x3 x4 x5 x6) (- width 1))))
;; (assert
;;   (and (> (+ x1 x2 x3 x4 x5 x6 x7) 0)
;;        (< (+ x1 x2 x3 x4 x5 x6 x7) (- width 1))))
;; (assert
;;   (and (> (+ x1 x2 x3 x4 x5 x6 x7 x8) 0)
;;        (< (+ x1 x2 x3 x4 x5 x6 x7 x8) (- width 1))))
;; (assert
;;   (and (> (+ x1 x2 x3 x4 x5 x6 x7 x8 x9) 0)
;;        (< (+ x1 x2 x3 x4 x5 x6 x7 x8 x9) (- width 1))))
;; (assert
;;   (and (> (+ x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) 0)
;;        (< (+ x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) (- width 1))))
;; (assert
;;   (and (> (+ x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) 0)
;;        (< (+ x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) (- width 1))))
;; (assert
;;   (and (> (+ x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12) 0)
;;        (< (+ x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12) (- width 1))))

;; (assert (and (> (+ y1) 0) (< (+ y1) (- height 1))))
;; (assert (and (> (+ y1 y2) 0) (< (+ y1 y2) (- height 1))))
;; (assert (and (> (+ y1 y2 y3) 0) (< (+ y1 y2 y3) (- height 1))))
;; (assert (and (> (+ y1 y2 y3 y4) 0) (< (+ y1 y2 y3 y4) (- height 1))))
;; (assert (and (> (+ y1 y2 y3 y4 y5) 0) (< (+ y1 y2 y3 y4 y5) (- height 1))))
;; (assert
;;   (and (> (+ y1 y2 y3 y4 y5 y6) 0) (< (+ y1 y2 y3 y4 y5 y6) (- height 1))))
;; (assert
;;   (and (> (+ y1 y2 y3 y4 y5 y6 y7) 0)
;;        (< (+ y1 y2 y3 y4 y5 y6 y7) (- height 1))))
;; (assert
;;   (and (> (+ y1 y2 y3 y4 y5 y6 y7 y8) 0)
;;        (< (+ y1 y2 y3 y4 y5 y6 y7 y8) (- height 1))))
;; (assert
;;   (and (> (+ y1 y2 y3 y4 y5 y6 y7 y8 y9) 0)
;;        (< (+ y1 y2 y3 y4 y5 y6 y7 y8 y9) (- height 1))))
;; (assert
;;   (and (> (+ y1 y2 y3 y4 y5 y6 y7 y8 y9 y10) 0)
;;        (< (+ y1 y2 y3 y4 y5 y6 y7 y8 y9 y10) (- height 1))))
;; (assert
;;   (and (> (+ y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11) 0)
;;        (< (+ y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11) (- height 1))))
;; (assert
;;   (and (> (+ y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12) 0)
;;        (< (+ y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12) (- height 1))))
(push)
(echo "can we reach lower right (12,12) with nx = 12: ny = 12 ??")
(assert
  (and (= (+ x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12) 12)
       (= (+ y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12) 12)))
(check-sat)
(pop)


