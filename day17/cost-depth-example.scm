#|
introduce pending - where we can suspend 

|#

;; ----------------------------
(import scheme)
(import simple-exceptions)
(import (chicken syntax))
(import (chicken string))
(import (chicken pretty-print))
(import (chicken io))
(import (chicken format))
(import (chicken sort))
(import (chicken file))
(import (chicken process-context))
;; (change-directory "day17")
;; (current-directory)
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
(define pp pretty-print)

;; presumably only - means just macro-rules inside procedural-macros
;;(import-for-syntax (only procedural-macros macro-rules))
(import-for-syntax procedural-macros)
(import-for-syntax srfi-1) ;; first second etc..

;;(change-directory "day17")
;;(current-directory)

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


;; 1 1 based 2d access
(define (xy x y)
  (format #t "accessing square ~a ~a ~%" x y)
  (vector-ref (vector-ref *vec* (- y 1)) (- x 1)))

;; ;; lookup score at x y
;; (define (score-xy x y)
;;   (vector-ref (vector-ref *score* (- y 1)) (- x 1)))

;; ;; change score at x y
;; (define (score-xy! x y z)
;;   (vector-set! (vector-ref *score* (- y 1)) (- x 1) z))

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

(define *id* 0)
(define *output-port* #f)

(define *print-path* #f)

(define *skip* 10000)
(define *skipi* 0)

(define *score* #f)

(define *max-cost* 110)
(define *best* 9999999999999999999999)

(define (keep? x y score) #t)

(define *height* (vec-height *vec*))
(define *width* (vec-width *vec*))

(define (distance-heuristic x y)
  (+ x y))

;; (define (onboard? x y)
;;   (and (>= x 1) (<= x *width*)
;;        (>= y 1) (<= y *height*)))

;; (define (next-states states)
;;   (apply append (map next-state states)))

;; (define (next-state state)
;;   (match state
;;     ((x y score m1 m2 m3)
;;      (cond
;;       ((not (onboard? x y)) #f)
;;       (#t (next-state2 x y score m1 m2 m3))))))

(define-er-macro (macro-feedback)
  %
  `(begin
     ;; skip say 1000 outputs
     ;; (set! *skipi* (+ 1 *skipi*))
     ;; (when *print-path* ;;(> *skipi* *skip*) ;; no skipping of values
     ;;   (format *output-port* "(path ~a id ~a cost ~a)~%" path *id* score)
     ;;   ;; increment id
     ;;   (set! *id* (+ 1 *id*))
     ;;   (set! *skipi* 0))
     ;;(flush-output *output-port*)
     
     ;; (format #t "score ~%")
     ;; (pp *score*)
     ;; (format #t "~%")
     ;; (format #t "position (~a , ~a) ~%~%" x y)
     (when
	 (and (= x *width*) (= y *height*))
       (when (< score *best*)
	 (set! *best* score)
	 ;; ==== record a solution ===
	 (when *print-path* ;;(> *skipi* *skip*) ;; no skipping of values
	   (format *output-port* "(path ~a id ~a cost ~a)~%" path *id* score)
	   (flush-output *output-port*)
	   ;; increment id
	   (set! *id* (+ 1 *id*)))
	 ;; ===== display score in terminal ====
	 (format #t "(~a ~a) : score ~a : best-so-far ~a ~%" x y score *best*)))))


(define (next-state2 x y score m1 m2 m3 path)
  ;; some feedback here ?
  (macro-feedback)
  (cond
   ((> score *best*) #f) ;; end this depth first search if max cost exceeded
   ((eq? m1 'left) (next-state-left x y score m1 m2 m3 path))
   ((eq? m1 'right) (next-state-right x y score m1 m2 m3 path))
   ((eq? m1 'up) (next-state-up x y score m1 m2 m3 path))
   ((eq? m1 'down) (next-state-down x y score m1 m2 m3 path))
   (#t (error 'next-state2))))

;; next state left : 
;; carry on going left
;; turn right go up
;; turn left go down
;; history of moves

#|
;; using a prefix %
;; 
(define-er-macro (macro-xyz)
  %
  `(,%list x y z))

(let ((x 0)(y 1)(z 2))
  (macro-xyz))
|#

;; iterate over path until either square not found or is found
(define (visited? x y path)
  (cond
   ((null? path) #f)
   (#t (let* ((square (car path))
	      (sx (first square))
	      (sy (second square))
	      (sc (third square))
	      )
	 (cond
	  ((and (= x sx) (= y sy)) #t)
	  (#t (visited? x y (cdr path))))))))

#|
(define-er-macro (macro-pending! x y lam)
  %
  `(set! pending (cons (list ,x ,y ,lam) pending)))

explicit-renaming macro
prefix %
,%when ,%list ,%let
|#
(define-syntax macro-go-generic
  (er-macro-transformer
   (lambda (expr rename compare?)
     (let ((dx (second expr))
	   (dy (third expr))
	   (on (fourth expr))
	   (dir (fifth expr)))
       ;;(macro-go-generic dx dy on dir)
       `(let* ((x2 ,dx)(y2 ,dy))
	  ;; rather than onboard? more generic test x against 0 width then y against 0 height
	  ;; specific x2 is zero or more
	  (when ,on 
	    ;; has this square (x2,y2) been visited already - if so cannot be optimal solution
	    (let ((seen (visited? x2 y2 path)))
	      (when (not seen)
		;; not seen - 
		(let* ((new-score (+ score (xy x2 y2)))
		       (new-path (cons (list x2 y2 new-score) path)))
		  ;; record new score
		  ;;(score-xy! x2 y2 new-score)
		  ;; recurse
		  ;;(macro-pending! x2 y2 (lambda () (next-state2 x2 y2 new-score ,dir m1 m2 new-path)))
		  (set! pending (cons (list x2 y2
					       (lambda ()
						 (next-state2 x2 y2 new-score ,dir m1 m2 new-path)))
					 pending))
		  ;;(set! pending (lambda () #f))
		  
		  ;; undo record
		  ;;(score-xy! x2 y2 #f)
		  )))))))))


#|
;;try write an unhygienic macro 

(define-er-macro (macro-foo-1)
  %
  `(set! p (lambda () 3)))

(define-er-macro (macro-foo-2)
  %
  `(,%set! p (,%lambda () 3)))


(define-er-macro (macro-foo-1)
  %
  `(set! p (lambda (z) (+ z z))))

(let ((p 5))
  (macro-foo-1)
  (p 3))

(let ((p 5))
  (macro-foo-2)
  (p 'dont-care))

|#
;;(pp (expand '(macro-go-left) #t))

(define-er-macro (macro-go-left)
  %
  `(macro-go-generic (- x 1) y (>= x2 1) 'left))

(define-er-macro (macro-go-right)
  %
  `(macro-go-generic (+ x 1) y (<= x2 *width*) 'right))

(define-er-macro (macro-go-down)
  %
  `(macro-go-generic x (+ y 1) (<= y2 *height*) 'down))

(define-er-macro (macro-go-up)
  %
  `(macro-go-generic x (- y 1) (>= y2 1) 'up))

;; --------------
;; pending is a list ((x y (lambda () ...))(x y (lambda () ...)))
;;                          ^^ --------------------^^----- can be called no args and fires
;; off the depth first search for that branch
;; sort based on distance

(define pending-sorter
  (lambda (p1 p2)
    (match (list p1 p2)
      (((x1 y1 fn1)(x2 y2 fn2))
       (> (+ x1 y1) (+ x2 y2))))))
   

(define-syntax pending-wrapper
  (er-macro-transformer
   (lambda (expr rename compare?)
     (let ((body (cdr expr)))
       `(let ((pending '()))
	  ,@body
	  (set! pending (sort pending pending-sorter))
	  (do-list (p pending)
		   (let ((fn (third p)))
		     (fn))))))))
			

;; -----------------

(define (next-state-left x y score m1 m2 m3 path)
  (pending-wrapper 
    (cond
     ((and (eq? m1 'left) (eq? m2 'left) (eq? m3 'left)) #f)
     (#t  ;; can go left
      (macro-go-left)))  
    (macro-go-down)
    (macro-go-up)))
  
(define (next-state-up x y score m1 m2 m3 path)
  (pending-wrapper 
  (cond
   ((and (eq? m1 'up) (eq? m2 'up) (eq? m3 'up)) #f)
   (#t
    (macro-go-up)))
  (macro-go-left)
  (macro-go-right)))

(define (next-state-down x y score m1 m2 m3 path)
  (pending-wrapper 
    (cond
     ((and (eq? m1 'down) (eq? m2 'down) (eq? m3 'down)) #f)
     (#t
      (macro-go-down)))
    (macro-go-left)
    (macro-go-right)))

(define (next-state-right x y score m1 m2 m3 path)
  (pending-wrapper 
   (cond
    ((and (eq? m1 'right) (eq? m2 'right) (eq? m3 'right)) #f)
    (#t
     (macro-go-right)))
   (macro-go-up)
   (macro-go-down)))


(define (solve mc)  
  (set! *best* 999999999999999999999)
  (set! *max-cost* mc)
  (set! *score* (copy-vec *vec*))
  ;; go right
  (next-state2 2 1 (xy 2 1) 'right #f #f `((2 1 ,(xy 2 1)) (1 1 0)))
  ;; go down
  (next-state2 1 2 (xy 1 2) 'down #f #f `((1 2 ,(xy 1 2)) (1 1 0)))
  )



(define (go gui)
  ;; solve for a maximum cost
  (cond
   (gui
    (call-with-output-file "solutions-example"
      (lambda (port)
	(set! *output-port* port)
	(set! *id* 0)
	(set! *print-path* #t)
	(solve 999999)
	)))
   (#t
    (set! *output-port* #t)
    (set! *id* 0)
    (set! *print-path* #f)
    (solve 999999))))

;; (format port "it worked!~%")
;; (flush-output port)))

;;(solve 999999999)

(go #t)

















