#|

day16

|#

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

(define (list->grid v)
  (list->vector (map (lambda (x) (list->vector (string->list x)))  v)))

(define input  (list->grid (read-all "input")))

(define example (list->grid (read-all "example")))

(define example2 (list->grid (read-all "example2")))


(define (get-xy vv x y)
  (vector-ref (vector-ref vv y) x))

(define (grid-width vv)
  (vector-length (vector-ref vv 0)))

(define (grid-height vv)
  (vector-length vv))

#|

if we can have beam at top left corner 0 , 0
up is -Y
down +Y
left -X
right +X
(x , y) coord
get-xy x y => returns whats at that coordinate grid square

beam x y and direction left right up down 
returns squares that are hot

if beams are off grid

. <.............
- <.............
| <.............
/ <.............
\ <.............

beam at  (0,0) moving right
foo will  look at 0 0 say ok right square (1,0) if mirror at 1,0 then direction change
 beam now at (1,0) direction appropriate with 0,0 *hot*

|#

(define (find-energised g)
  (let ((hot '((0 0))))	
    (letrec ((foo (lambda (beams)
		    (cond
		     ((null? beams) #f)
		     (#t
		      (let ((new-beams '()))
		      (do-list (b beams)
			       (match b
				 ((x y dir) 
				  (cond  ;; record where beams go
				   ((member (list x y) hot equal?) #f)
				   (#t (set! hot (cons (list x y) hot))))
				  (cond
				   ((eq? dir 'left ) (let* ((x2 (- x 1))
							    (y2 y)
							    (e (get-xy g x2 y2)))
						       (cond
							((char=? e #\.) "empty square"
							 (set! new-beams (cons `(,x2 ,y2 left)
									       new-beams)))
							((char=? e #\-) "pass through -"
							 (set! new-beams (cons `(,x2 ,y2 left)
									       new-beams)))
							((char=? e #\|) "splitter"
							 (set! new-beams (cons `(,x2 ,y2 up)
									       (cons `(,x2 ,y2 down)
										     new-beams))))
							((char=? e #\/) "mirror"
							 (set! new-beams (cons `(,x2 ,y2 down)
									       new-beams)))
							((char=? e #\\) "mirror"
							 (set! new-beams (cons `(,x2 ,y2 up)
									       new-beams)))
							(#t (error "char-mirror-1")))))

				   ((eq? dir 'right ) (let* ((x2 (+ x 1))
							    (y2 y)
							    (e (get-xy g x2 y2)))
						       (cond
							((char=? e #\.) "empty square"
							 (set! new-beams (cons `(,x2 ,y2 right)
									       new-beams)))
							((char=? e #\-) "pass through -"
							 (set! new-beams (cons `(,x2 ,y2 right)
									       new-beams)))
							((char=? e #\|) "splitter"
							 (set! new-beams (cons `(,x2 ,y2 up)
									       (cons `(,x2 ,y2 down)
										     new-beams))))
							((char=? e #\/) "mirror"
							 (set! new-beams (cons `(,x2 ,y2 up)
									       new-beams)))
							((char=? e #\\) "mirror"
							 (set! new-beams (cons `(,x2 ,y2 down)
									       new-beams)))
							(#t (error "char-mirror-2")))))

				   ((eq? dir 'up ) (let* ((y2 (- y 1))
							    (x2 x)
							    (e (get-xy g x2 y2)))
						       (cond
							((char=? e #\.) "empty square"
							 (set! new-beams (cons `(,x2 ,y2 up)
									       new-beams)))
							((char=? e #\-) "splitter"
							 (set! new-beams (cons `(,x2 ,y2 left)
									       (cons `(,x2 ,y2 right)
										     new-beams))))
							 ((char=? e #\|) "pass"
							 (set! new-beams (cons `(,x2 ,y2 up)
										     new-beams)))
							((char=? e #\/) "mirror"
							 (set! new-beams (cons `(,x2 ,y2 right)
									       new-beams)))
							((char=? e #\\) "mirror"
							 (set! new-beams (cons `(,x2 ,y2 left)
									       new-beams)))
							(#t (error "char-mirror-3")))))

				   ((eq? dir 'down ) (let* ((y2 (+ y 1))
							    (x2 x)
							    (e (get-xy g x2 y2)))
						       (cond
							((char=? e #\.) "empty square"
							 (set! new-beams (cons `(,x2 ,y2 down)
									       new-beams)))
							((char=? e #\-) "splitter"
							 (set! new-beams (cons `(,x2 ,y2 left)
									       (cons `(,x2 ,y2 right)
										     new-beams))))
							 ((char=? e #\|) "pass"
							 (set! new-beams (cons `(,x2 ,y2 down)
										     new-beams)))
							((char=? e #\/) "mirror"
							 (set! new-beams (cons `(,x2 ,y2 left)
									       new-beams)))
							((char=? e #\\) "mirror"
							 (set! new-beams (cons `(,x2 ,y2 right)
									       new-beams)))
							(#t (error "char-mirror-4")))))
				   (#t (error "no direction "))))
				 (_ (error "no match"))))
		      (foo)))))))))
      (foo '((0 0 right)))
      hot))








    
