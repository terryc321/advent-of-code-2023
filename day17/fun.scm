#|

day16

|#
(use-modules (system base compile))

;;(default-optimization-level 9)
(default-optimization-level 1)


(use-modules (ice-9 textual-ports))
(use-modules (ice-9 format)) ;; format common lisp 
(use-modules (ice-9 pretty-print)) ;; pretty-print
(define pp pretty-print)
;;(use-modules (rnrs)) ;; assert 
(use-modules (srfi srfi-1)) ;; first second third ...
(use-modules (srfi srfi-2)) ;; first second third ...
(use-modules (statprof)) ;; statistical profiler

(use-modules ((rnrs)
	     ;;#:select open-pipe close-pipe)
	      #:renamer (symbol-prefix-proc 'rnrs:)))


;; assert

;; regular expression
(use-modules (ice-9 regex)) 

;; pattern matcher ?
(use-modules (ice-9 match))

;; binary io -- dont know if this helped any as read-u8 is for reading ints no??
(use-modules (ice-9 binary-ports))

;; r7rs 
(use-modules (scheme base))

;; --------------------- macros --------------------------
(define-macro (dolist varls . body)
  (let* ((fn (gensym "fn"))
	 (xs (gensym "xs"))
	 (var (car varls))
	 (ls  (car (cdr varls))))
    `(letrec ((,fn (lambda (,xs)
		     (cond
		      ((null? ,xs) #f)
		      (#t (let ((,var (car ,xs)))
			    ,@body
			    (,fn (cdr ,xs))))))))
       (,fn ,ls))))

;; --------------------- while ----------------------------

(defmacro while (condition . body)
  (let ((lup (gensym "loop")))
    `(letrec ((,lup (lambda ()
		      (when ,condition
			,@body
			(,lup)))))
       (,lup))))

;; (let ((i 0))
;;   (while (< i 10)
;;     (format #t " i = ~a ~%" i )
;;     (set!  i (+ i 1))))

;; --------------------- macros --------------------------

;; (chdir "day16")
;; (getcwd)

#|
read lines of input
|#

(define fails 0)

(define (read-all f)
  (call-with-input-file f
    (lambda (port)
      (let ((lines '()))
	(letrec ((foo (lambda ()
			(let ((line (get-line port)))
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


|#

(define (puzzle vv filename)
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

  (define output-port (open-output-file filename))
  
  (define (report x y path cost step)
    (format output-port "~a~%" `(path ,path cost ,cost)))
  
  (define (move-left x y n path cost step)    
    (cond
     ((awol? x y) #f)
     (#t
      (let ((cost2 (+ cost (get-xy vv x y))))
	(report x y path cost step)
	(cond
	 ((finish-line? x y) (format #t "path ~a : cost ~a ~%" path cost))
	 ((visited? x y path) #f)
	 (#t
	  (when (not (= n 0)) (move-left (- x 1) y (- n 1) (cons `(,x ,y) path) cost2 (1+ step)))
	  (when (= n 0) (move-down x (+ y 1) 2 (cons `(,x ,y) path) cost2 (1+ step)))
	  (when (= n 0) (move-up x (- y 1) 2 (cons `(,x ,y) path) cost2 (1+ step)))
	  ))))))
  
  (define (move-right x y n path cost step) 
    (cond
     ((awol? x y) #f)
     (#t
      (let ((cost2 (+ cost (get-xy vv x y))))	
	(report x y path cost step)
	(cond
	 ((finish-line? x y) (format #t "path ~a : cost ~a ~%" path cost))
	 ((visited? x y path) #f)
	 (#t
	  (when (= n 0) (move-down x (+ y 1) 2 (cons `(,x ,y) path) cost2 (1+ step)))
	  (when (not (= n 0)) (move-right (+ x 1) y (- n 1) (cons `(,x ,y) path) cost2 (1+ step)))
	  (when (= n 0) (move-up x (- y 1) 2 (cons `(,x ,y) path) cost2 (1+ step)))
	  ))))))
  
  (define (move-up x y n path cost step) 
    (cond
     ((awol? x y) #f)
     (#t
      (let ((cost2 (+ cost (get-xy vv x y))))
	(report x y path cost step)
	(cond
	 ((finish-line? x y) (format #t "path ~a : cost ~a ~%" path cost))
	 ((visited? x y path) #f)
	 (#t
	  (when (= n 0) (move-left (- x 1) y 2 (cons `(,x ,y) path) cost2 (1+ step)))
	  (when (= n 0) (move-right (+ x 1) y 2 (cons `(,x ,y) path) cost2 (1+ step)))
	  (when (not (= n 0)) (move-up x (- y 1) (- n 1) (cons `(,x ,y) path) cost2 (1+ step)))))))))
  
  (define (move-down x y n path cost step) 
    (cond
     ((awol? x y) #f)
     (#t
      (let ((cost2 (+ cost (get-xy vv x y))))
	(report x y path cost step)
	(cond
	 ((finish-line? x y) (format #t "path ~a : cost ~a ~%" path cost))
	 ((visited? x y path) #f)
	 (#t	
	  (when (not (= n 0)) (move-down x (+ y 1) (- n 1) (cons `(,x ,y) path) cost2 (1+ step)))
	  (when (= n 0) (move-right (+ x 1) y 2 (cons `(,x ,y) path) cost2 (1+ step)))
	  (when (= n 0) (move-left (- x 1) y 2 (cons `(,x ,y) path) cost2 (1+ step)))
	  ))))))

  (define (start)
    (let ((x 0)(y 0)(n 3)(step 0)(path '())(cost 0))
      (move-right x y n path cost step)
      (move-down x y n path cost step)))

  (start))




(define (example-1 f)
  (puzzle example f))

(define (part-1 f)
  (puzzle input f))
















    
