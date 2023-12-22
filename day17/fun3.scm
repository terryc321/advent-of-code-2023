#|


|#

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

(define example (list->grid (read-all "example")))

;;(define example2 (list->grid (read-all "example2")))

(define (get-xy vv x y)
  (vector-ref (vector-ref vv y) x))

(define (grid-width vv)
  (vector-length (vector-ref vv 0)))

(define (grid-height vv)
  (vector-length vv))


;; ---------------------------------
;;(char->int #\1)

#|

at most 3 steps in direction before have to turn left or right

         |
X->1->2->3
         |

left or right

right 2
right 1
right 0
when zero can no longer go right , must choose up or down

right -> up / down
up -> left / right
down -> left or right
left -> up / down

start at 0 0
when enter square take its value and add to sum presumably

right x y n
if n = 0 then up x y 2
if n = 0 then down x y 2

at start can take 3 steps either right or down

--------------------------------------------
0,0
how about a random walker
he can go up -Y , right +X , down +Y , left -X
then put constraints on that walk 

vv : problem vector
x coordinate
y
path coordinates passed through

- like to stay on board
x : 0 to width-1
y : 0 to height-1

1) reduce parameters in calls by having them in scope in outer procedure

constraint 1 : on-board
constraint 2 : no walking backwards
initially path is empty
identify walking backwards as if position is opposite direction moving
so need to know direction moving
either explicit or by being executed in a moving procedure
know moving right if in moving-right procedure ...

may miss the final cost of last square , but since that is missing from all paths
the minimum path will still be the minimum path

lack of real time debugging support
lack of gui code
lack of inspector to see what code is doing
lack of visaulisation tools
lack of macro writing support
lack of static checking
lack of theorem proving
lack of reasoning tools
lack of model checking
lack of code writing tools
lack of linter
lack of mathematical rigour
lack of full complete reliable documentation 


if already reached a place we have been to cut the search off

having to check visited? x y on each iteration
can we not produce a way to generate that guarantees we do need to check is somewhere has been visited

incentivise moving down and to right over left and up

curiously it will create a wall around the finish point
then go off searching in the weeds forever never being able to complete task

probbly a good case for logical thinking
how can eliminate search space that will never lead to solutions looking for ?




|#

(define solutions '())
(define best-solution '())
(define best-cost 99999999999999999999)
(define best-step 99999999999999999999)


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

  (define (solution! path cost step)
    (cond
     ((< cost best-cost)
      (format #t "~%~%***best solution so far *** ~% *** path ~%~a~%***cost ~a~%~%" path cost)
      (set! best-cost cost)
      (set! best-solution path)
      (set! best-step step))))

  
  (define (report x y path cost step)
    (format output-port "~a~%" `(path ,path id ,id cost ,cost))
    (set! id (1+ id)))

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
	(report x y path cost step)
	(cond
	 ((finish-line? x y) (solution! (cons `(,x ,y) path) cost2 step))
	 ((visited? x y path) #f)
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
	(report x y path cost step)
	(cond
	 ((finish-line? x y) (solution! (cons `(,x ,y) path) cost2 step))
	 ((visited? x y path) #f)
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
	(report x y path cost step)
	(cond
	 ((finish-line? x y) (solution! (cons `(,x ,y) path) cost2 step))
	 ((visited? x y path) #f)
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
	(report x y path cost step)
	(cond
	 ((finish-line? x y) (solution! (cons `(,x ,y) path) cost2 step))
	 ((visited? x y path) #f)
	 (#t	
	  (when (not (= n 0)) (move-down x (+ y 1) (- n 1) (cons `(,x ,y) path) cost2 (1+ step)))
	  (move-right (+ x 1) y 2 (cons `(,x ,y) path) cost2 (1+ step))
	  (move-left (- x 1) y 2 (cons `(,x ,y) path) cost2 (1+ step))
	  ))))))

  (define (start)
    (let ((x 0)(y 0)(n 3)(step 0)(path '())(cost 0))
      (move-right x y n path cost step)
      (move-down x y n path cost step)))

  (call-with-output-file filename (lambda (port)
				    (set! output-port port)
				    (start)))

  )




;; example1.out is now a fifo
(define (example-1)
  (puzzle example "example1.out"))

;; part1.out is now a fifo
(define (part-1)
  (puzzle input "part1.out"))


;; start with 
(example-1)














    
