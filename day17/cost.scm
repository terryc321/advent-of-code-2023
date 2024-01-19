(import scheme)

(import simple-exceptions)

(import (chicken string))
(import (chicken pretty-print))
(define pp pretty-print)

(import (chicken io))
(import (chicken format))
(import (chicken sort))
(import (chicken file))
(import (chicken process-context))
;; (change-directory "day17")
;; (get-current-directory)

(import procedural-macros)
(import regex)

(import simple-md5)
(import simple-loops)

(import srfi-69)
;; hash-table-ref  hash key thunk
;; hash-table-set! hash key val

;; sudo chicken-install srfi-178
(import srfi-178)
;; srfi-178 provides bit-vectors


;; (import-for-syntax
;;   (only checks <<)
;;   (only bindings bind bind-case)
;;   (only procedural-macros macro-rules with-renamed-symbols once-only))

(import sequences)

(import srfi-1)

(import matchable)

;;--------------------------------------
(define *vec*
#(
#(2 4 1 3 4 3 2 3 1 1 3 2 3)
#(3 2 1 5 4 5 3 5 3 5 6 2 3)
#(3 2 5 5 2 4 5 6 5 4 2 5 4)
#(3 4 4 6 5 8 5 8 4 5 4 5 2)
#(4 5 4 6 6 5 7 8 6 7 5 3 6)
#(1 4 3 8 5 9 8 7 9 8 4 5 4)
#(4 4 5 7 8 7 6 9 8 7 7 6 6)
#(3 6 3 7 8 7 7 9 7 9 6 5 3)
#(4 6 5 4 9 6 7 9 8 6 8 8 7)
#(4 5 6 4 6 7 9 9 8 6 4 5 3)
#(1 2 2 4 6 8 6 8 6 5 5 6 3)
#(2 5 4 6 5 4 8 8 8 7 7 3 5)
#(4 3 2 2 6 7 4 6 5 5 5 3 3)
))

;; 0 0 based vector reference
(define (xy x y)
  (vector-ref (vector-ref *vec* y) x))

;; lookup score at x y
(define (score-xy x y)
  (vector-ref (vector-ref *score* y) x))

;; change score at x y
(define (score-xy! x y z)
  (vector-set! (vector-ref *score* y) x z))

(define (vec-height v)
  (vector-length v))

(define (vec-width v)
  (vector-length (vector-ref v 0)))

(define (copy-vec v)
  (let ((height (vec-height v))
	(width (vec-width v)))
    (let ((v2 (make-vector height #f)))
      (letrec ((foo (lambda (n m)
		      (cond
		       ((>= n m) v2)
		       (#t (vector-set! v2 n (make-vector width #f))
			   (foo (+ n 1) m))))))
	(foo 0 height)))))

;; -----------------------------



(define *score* (copy-vec *vec*))
(define *height* (vec-height *vec*))
(define *width* (vec-width *vec*))

;; start in top left
;; minimal path goes anywhere ? ignore constraint of 3 in one direction for now ...
;; breadth first search assigns score to 

(define (onboard? x y)
  (and (>= x 0) (< x *width*)
       (>= y 0) (< y *height*)))

#|
 if off board then redundant
 if has a score higher than known best score then redundant?....
 possibly not , may be several ways to get to square X Y

|#
(define (redundant? state)
  (match state
    ((x y score m1 m2 m3)
     (cond
      ((not (onboard? x y)) #f)
      (#t #t)))))

;; prefer squares closer to bottom right corner
(define (state-size state1 state2)
  (match state1
    ((x1 y1 _ _ _ _)
     (match state2
       ((x2 y2 _ _ _ _)
	(let ((dx (+ (abs (- *width* x1))
		     (abs (- *height* y1))))
	      (dx2 (+ (abs (- *width* x2))
		      (abs (- *height* y2)))))
	  (> dx dx2)))))))
	      	      

;;for each square registered interest find the next states
(define (next-states states)
  ;;(sort (apply append (filter redundant? (map next-state states))) state-size))
  (sort (apply append (map next-state states)) state-size))


(define (next-state state)
  (match state
    ((x y score m1 m2 m3)
     (cond
      ((not (onboard? x y)) #f)
      (#t (next-state2 x y score m1 m2 m3))))))

;; keep ?
(define (keep? x y v)
  (let ((val (score-xy x y)))
    (cond
     ((not val) ;; first integer value to assign
      (score-xy! x y v)
      #t)
     ((< v val) ;; found a lower value
      (score-xy! x y v)
      #t))))

(define (next-state2 x y score m1 m2 m3)
  (cond
   ((eq? m1 'left) (next-state-left x y score m1 m2 m3))
   ((eq? m1 'right) (next-state-right x y score m1 m2 m3))
   ((eq? m1 'up) (next-state-up x y score m1 m2 m3))
   ((eq? m1 'down) (next-state-down x y score m1 m2 m3))
   (#t (error 'next-state2))))

;; next state left : 
;; carry on going left
;; turn right go up
;; turn left go down
;; history of moves 
(define (next-state-left x y score m1 m2 m3)
  ;; left
  (let ((res '()))
    (cond
     ((and (eq? m1 'left) (eq? m2 'left) (eq? m3 'left)) #f)
     (#t
    (let* ((x2 (- x 1))(y2 y))
      (when (onboard? x2 y2)
	(let* ((score2 (+ score (xy x2 y2)))
	       (tmp (list x2 y2 score2 'left m1 m2)))
	  (when (keep? x2 y2 score2)
	    (set! res (cons tmp res))))))))
    ;; down
  (let* ((x2 x)(y2 (+ y 1)))
    ;;(format #t "nsr y2 bug : ~a ~%" y2)
    (when (onboard? x2 y2)
      (let* ((score2 (+ score (xy x2 y2)))
	     (tmp (list x2 y2 score2 'down m1 m2)))
	  (when (keep? x2 y2 score2)
	    (set! res (cons tmp res))))))

    ;; up
  (let* ((x2 x)(y2 (- y 1)))
    ;;(format #t "nsr y2 bug : ~a ~%" y2)
    (when (onboard? x2 y2)
      (let* ((score2 (+ score (xy x2 y2)))
	     (tmp (list x2 y2 score2 'down m1 m2)))
	(when (keep? x2 y2 score2)
	  (set! res (cons tmp res))))))
  res))

;; next state up : 
;; carry on going up
;; turn left go left
;; turn right go right
(define (next-state-up x y score m1 m2 m3)
  (let ((res '()))
    ;; check go up
    (cond
     ((and (eq? m1 'up) (eq? m2 'up) (eq? m3 'up)) #f)
     (#t
      (let* ((x2 x)(y2 (- y 1)))
	(when (onboard? x2 y2)
	  (let* ((score2 (+ score (xy x2 y2)))
		 (tmp (list x2 y2 score2 'up m1 m2)))
	    (when (keep? x2 y2 score2)
	    (set! res (cons tmp res))))))))
	    
   ;; left
    (let* ((x2 (- x 1))(y2 y))
      (when (onboard? x2 y2)
	(let* ((score2 (+ score (xy x2 y2)))
	       (tmp (list x2 y2 score2 'left m1 m2)))
	  (when (keep? x2 y2 score2)
	  (set! res (cons tmp res))))))
    
   ;; right
    (let* ((x2 (+ x 1))(y2 y))
      (when (onboard? x2 y2)
	(let* ((score2 (+ score (xy x2 y2)))
	       (tmp (list x2 y2 score2 'right m1 m2)))
	  (when (keep? x2 y2 score2)
	  (set res (cons tmp res))))))
    res))


;; next state down : 
;; carry on going down
;; turn left go right
;; turn right go left
(define (next-state-down x y score m1 m2 m3)
  (let ((res '()))
    ;; check go down
    (cond
     ((and (eq? m1 'down) (eq? m2 'down) (eq? m3 'down)) #f)
     (#t
      (let* ((x2 x)(y2 (+ y 1)))
	(when (onboard? x2 y2)
	  (let* ((score2 (+ score (xy x2 y2)))
		 (tmp (list x2 y2 score2 'down m1 m2)))
	    (when (keep? x2 y2 score2)
	    (set! res (cons tmp res))))))))
   ;; left
    (let* ((x2 (- x 1))(y2 y))
      (when (onboard? x2 y2)
	(let* ((score2 (+ score (xy x2 y2)))
	       (tmp (list x2 y2 score2 'left m1 m2)))
	  (when (keep? x2 y2 score2)
	  (set! res (cons tmp res))))))
	  ;; right
    (let* ((x2 (+ x 1))(y2 y))
      (when (onboard? x2 y2)
	(let* ((score2 (+ score (xy x2 y2)))
	       (tmp (list x2 y2 score2 'right m1 m2)))
	  (when (keep? x2 y2 score2)
	    (set! res (cons tmp res))))))
    res))



;; next state right : 
;; carry on going right
;; turn right go down
;; turn left go up
(define (next-state-right x y score m1 m2 m3)
  (let ((res '()))
  ;; check go right
  (cond
   ((and (eq? m1 'right) (eq? m2 'right) (eq? m3 'right)) #f)
   (#t
    (let* ((x2 (+ x 1))(y2 y))
      ;;(format #t "nsr y2 bug : ~a ~%" y2)
      (when (onboard? x2 y2)
	(let* ((score2 (+ score (xy x2 y2)))
	       (tmp (list x2 y2 score2 'right m1 m2)))
	  (when (keep? x2 y2 score2)
	  (set! res (cons tmp res))))))))
  ;; down
  (let* ((x2 x)(y2 (+ y 1)))
    ;;(format #t "nsr y2 bug : ~a ~%" y2)
    (when (onboard? x2 y2)
      (let* ((score2 (+ score (xy x2 y2)))
	     (tmp (list x2 y2 score2 'down m1 m2)))
	(when (keep? x2 y2 score2)
	(set! res (cons tmp res))))))
  ;; up
  (let* ((x2 x)(y2 (- y 1)))
    ;;(format #t "nsr y2 bug : ~a ~%" y2)
    (when (onboard? x2 y2)
      (let* ((score2 (+ score (xy x2 y2)))
	     (tmp (list x2 y2 score2 'down m1 m2)))
	(when (keep? x2 y2 score2)
	  (set! res (cons tmp res))))))
  res))


;; while *sources* has keys 
(define (solve)
  (set! *score* (copy-vec *vec*))
  (format #t "score : ")
  (pp *score*)
  (newline)

  ;; initial states
  (let ((init (list (list 1 0 (xy 1 0) 'right #f #f)
		    (list 0 1 (xy 0 1) 'down #f #f))))
    (next-states init)))


#|

buggy as getting multiple answers to same input
(solve)
    (9 12 107 down right down)
    (10 12 112 right down right)
    (11 12 115 right right down)
    (12 12 115 down down down)))
#;4412>
    (9 12 107 down right down)
    (10 12 112 right down right)
    (11 12 113 left down down)
    (12 12 110 down down down)))


|#

  
					 



















