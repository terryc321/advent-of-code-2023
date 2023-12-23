#|

idea had is
decide if move take me further away from where i want to be ?



|#

(define cost-vector #f)


(define paradoxes '())

(define (add-paradox path)
  (set! paradoxes (cons path paradoxes)))


(add-paradox '((12 10) (12 11) (11 11) (11 10) (10 10) (9 10) (8 10) (8 11) (9 11) (10 11) (10 12) (9 12) (8 12) (7 12) (7 11) (7 10) (6 10) (5 10) (4 10) (4 11) (5 11) (6 11) (6 12) (5 12) (4 12) (3 12) (3 11) (3 10) (3 9) (2 9) (2 8) (2 7) (2 6) (1 6) (1 5) (1 4) (1 3) (0 3) (0 2) (0 1) (0 0)))



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
;; (change-directory "day15")
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

;;--------------------------------------

;; change input file ! 
(define (get-input) (call-with-input-file "input"
		      (lambda (port)
			(read port))))

(define input (get-input))

(define example #f)

;;(set! input example)

(define (1+ x) (+ x 1))
(define (1- x) (- x 1))

(define (10+ x) (+ x 10))
(define (10- x) (- x 10))

(define rest cdr)

;;------------------------------------------



(define (read-all f)
  (call-with-input-file f
    (lambda (port)
      (let ((lines '()))
	(letrec ((foo (lambda ()
			(let ((line (read-line port)))
			  (format #t "~a ~%" line)
			  (cond
			   ((eof-object? line) (reverse lines))
			   (#t (set! lines (cons line lines))
			       (foo)))))))
	  (foo))))))


(define (char->int ch)
  (let ((n (- (char->integer ch) (char->integer #\0))))
    (cond
     ((and (>= n 0) (<= n 9)) #t)
     (#t (set! fails (cons `((char->int ,ch) ,n expected 0 to 9) fails))))
    n))


(define (list->grid v)
  (list->vector (map (lambda (x) (list->vector (map char->int (string->list x))))  v)))

(define input  (list->grid (read-all "input")))
(define cost-input  (list->grid (read-all "input")))


(define example (list->grid (read-all "example")))
(define cost-example (list->grid (read-all "example")))

;;(define example2 (list->grid (read-all "example2")))

(define (get-xy vv x y)
  (vector-ref (vector-ref vv y) x))

(define (grid-width vv)
  (vector-length (vector-ref vv 0)))

(define (grid-height vv)
  (vector-length vv))

(define (set-xy! vv x y z)
  (vector-set! (vector-ref vv y) x z))

  

;;--------------------------------------
#|

solving the example only

cost-example is a grid same size as example
it simply needs filled with '() empty list
as we find a lower cost way to reach a particular square
we label that square with ((cost path-to-square))

each square has a cost and that is accounted for
optimal path does not revisit same square so
we do not concern ourselves with that

state space search 0,0 to 12,12 inclusive
(iota 13)

next-states

bootstrap process
x y cost path

|#

;; flatten cost-example

(define (flatten-cost vv)
  (do-list (i (iota (grid-width vv)))
	   (do-list (j (iota (grid-height vv)))
		    (set-xy! vv j #f))))


;;(define flatten-once #t)
;;(flatten-cost-example)

(define (initial-state)
  '((0 0 0 '())))

;; breadth first search ?
;; may need to apply append to flatten them out
(define (next-states states)
  (map next-state! states))

;; where state is current x ,y  cost and previous squares path to reach 
(define (next-state state)
  #t)


(define (reset)
  (flatten-cost cost-example)
  (flatten-cost cost-input))
  







(define (puzzle vv filename)
  (define id 0)
  (define width (vector-length (vector-ref vv 0)))
  (define height (vector-length vv))
  (define (onboard? x y)
    (and (>= x 0)(< x width)(>= y 0)(< y height)))
  (define (awol? x y)
    (not (onboard? x y)))
  (define (finish-line? x y)
    (and (= x (- width 1))
	 (= y (- height 1))))
  (define (visited? x y path)
    (member `(,x ,y) path))

  (define output-port #f)

  
  (define (solution! path cost step) #f)  
    ;; (cond
    ;;  ((< cost best-cost)
    ;;   (format #t "~%~%***best solution so far *** ~% *** path ~%~a~%***cost ~a~%~%" path cost)
    ;;   (set! best-cost cost)
    ;;   (set! best-solution path)
    ;;   (set! best-step step))))
  
  (define (report x y path cost step) #f)
  
    ;; (format output-port "~a~%" `(path ,path id ,id cost ,cost))
    ;; (set! id (1+ id)))


  
  ;; if path is first ever to reach square then record that as best known
  ;; if path is same as another path , may be equally optimal
  ;; if path is higher cost than previous path , suboptimal , definitely poor path
  ;; return #t means sub optimal - do not continue search
  ;; return #f means first found, equal to best search so far
  (define (poor? x y path cost step)
    (let ((known (get-xy cost-vector x y)))
      (cond
       ((null? known)
	(set-xy! cost-vector x y `(,cost ,path))
	;;(format #t "found first for ~a , ~a  at cost ~a ~%" x y cost)
	#f)
       (#t
	;;(format #t "known = [~a]~%" known)
	(let ((known-cost (car known)))
	  (cond
	   ((< cost known-cost)
	    (set-xy! cost-vector x y `(,cost ,path))
	    (format #t "found new best for for ~a , ~a  at cost ~a ~%" x y cost)	    
	    #f)
	   ((= cost known-cost)
	    ;;(set-xy! cost-vector x y (cons `(,cost ,path) known))
	    #f)
	   (#t #t)))))))


  
       

  
  ;; going to try guess how long optiml path is
  ;; no real idea how to solve this yet ...
  (define (dead-end? p) #f)
;;    (> (length p) 27))
  
  ;; how to prevent a walled off search pattern ??
  
  (define (move-left x y n path cost step)    
    (cond
     ((awol? x y) #f)
     ((dead-end? path) #f)
     (#t
      (let ((cost2 (+ cost (get-xy vv x y))))	
	(cond
	 ((poor? x y path cost2 step) #f)
	 ;; ((finish-line? x y) (solution! (cons `(,x ,y) path) cost2 step))
	 ;; ((visited? x y path) #f)
	 (#t
	  (when (not (= n 0)) (move-left (- x 1) y (- n 1) (cons `(,x ,y) path) cost2 (1+ step)))
	  (move-down x (+ y 1) 2 (cons `(,x ,y) path) cost2 (1+ step))
	  (move-up x (- y 1) 2 (cons `(,x ,y) path) cost2 (1+ step))
	  ))))))
  
  (define (move-right x y n path cost step) 
    (cond
     ((awol? x y) #f)
     ((dead-end? path) #f)
     (#t
      (let ((cost2 (+ cost (get-xy vv x y))))	
	(cond
	 ((poor? x y path cost2 step) #f)
	 ;; ((finish-line? x y) (solution! (cons `(,x ,y) path) cost2 step))
	 ;; ((visited? x y path) #f)
	 (#t
	  (move-down x (+ y 1) 2 (cons `(,x ,y) path) cost2 (1+ step))
	  (when (not (= n 0)) (move-right (+ x 1) y (- n 1) (cons `(,x ,y) path) cost2 (1+ step)))
	  (move-up x (- y 1) 2 (cons `(,x ,y) path) cost2 (1+ step))
	  ))))))
  
  (define (move-up x y n path cost step) 
    (cond
     ((awol? x y) #f)
     ((dead-end? path) #f)
     (#t
      (let ((cost2 (+ cost (get-xy vv x y))))
	(cond
	 ((poor? x y path cost2 step) #f)
	 ;; ((finish-line? x y) (solution! (cons `(,x ,y) path) cost2 step))
	 ;; ((visited? x y path) #f)
	 (#t
	  (move-right (+ x 1) y 2 (cons `(,x ,y) path) cost2 (1+ step))
	  (move-left (- x 1) y 2 (cons `(,x ,y) path) cost2 (1+ step))
	  (when (not (= n 0)) (move-up x (- y 1) (- n 1) (cons `(,x ,y) path) cost2 (1+ step)))))))))
  
  (define (move-down x y n path cost step) 
    (cond
     ((awol? x y) #f)
     ((dead-end? path) #f)
     (#t
      (let ((cost2 (+ cost (get-xy vv x y))))
	(cond
	 ((poor? x y path cost2 step) #f)
	 ;; ((finish-line? x y) (solution! (cons `(,x ,y) path) cost2 step))
	 ;; ((visited? x y path) #f)
	 (#t	
	  (when (not (= n 0)) (move-down x (+ y 1) (- n 1) (cons `(,x ,y) path) cost2 (1+ step)))
	  (move-right (+ x 1) y 2 (cons `(,x ,y) path) cost2 (1+ step))
	  (move-left (- x 1) y 2 (cons `(,x ,y) path) cost2 (1+ step))
	  ))))))

  (define (start)
    (let ((x 0)(y 0)(n 3)(step 0)(path '())(cost 0))
      (move-right x y n path cost step)
      (move-down x y n path cost step)))

  
  #|
  (call-with-output-file filename (lambda (port)
				    (set! output-port port)
  (start)))
  |#
  (start)

  )




;; example1.out is now a fifo
(define (example-1)
  (set! cost-vector cost-example)
  (flatten-cost cost-vector)
  
  (puzzle example "example1.out")
  (pp cost-vector))

;; part1.out is now a fifo
(define (input-1)
  (set! cost-vector cost-input)
  (flatten-cost cost-vector)
  (puzzle input "part1.out")
  (pp cost-vector))

;;(example-1)

;;(input-1)
















    
