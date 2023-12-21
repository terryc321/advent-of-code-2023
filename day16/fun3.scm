
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
(use-modules ((scheme base)
	      #:renamer (symbol-prefix-proc 'base:)))



;; --------------------- macros --------------------------
(define-macro (do-list varls . body)
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


;; ---------------------------------------------------------------------------------
#|
laser at x , y with direction d

BUG (char=? #\.)
char=? does not pick up only one argument provided


|#

(define hot '())

(define (hit x y)
  (let ((s (list x y)))
    (cond
     ((member s hot equal?) #f)
     (#t (set! hot (cons s hot))))))

(define (laser-entry vv x y d)
  (let ((width (grid-width vv))
	(height (grid-height vv)))
    (define (laser x y d)
      (cond
       ;; off board
       ((or (< x 0)(>= x width)(< y 0)(>= y height)) #f)
       (#t
	(let ((c (get-xy vv x y)))

	  ;; laser is at x y
	  (hit x y)
	  (format #t "hot .length = ~a ~%" (length hot))
	  (cond
	  ;; right
	  ((and (char=? c #\.) (eq? d 'R)) (laser (1+ x) y 'R))
	  ((and (char=? c #\/) (eq? d 'R)) (laser x (1- y) 'U))
	  ((and (char=? c #\\) (eq? d 'R)) (laser x (1+ y) 'D))
	  ((and (char=? c #\-) (eq? d 'R)) (laser (1+ x) y 'R))
	  ((and (char=? c #\|) (eq? d 'R)) (laser x (1- y) 'U)
           (laser x (1+ y) 'D))
	  ((eq? d 'R) (error "no match R"))
	  ;; left
	  ((and (char=? c #\.) (eq? d 'L)) (laser (1- x) y 'L))
	  ((and (char=? c #\/) (eq? d 'L)) (laser x (1+ y) 'D))
	  ((and (char=? c #\\) (eq? d 'L)) (laser x (1- y) 'U))
	  ((and (char=? c #\-) (eq? d 'L)) (laser (1- x) y 'L))
	  ((and (char=? c #\|) (eq? d 'L)) (laser x (1- y) 'U)
           (laser x (1+ y) 'D))
	  ((eq? d 'L) (error "no match L"))
	  
	  ;; up
	  ((and (char=? c #\.) (eq? d 'U)) (laser x (1- y) 'U))
	  ((and (char=? c #\/) (eq? d 'U)) (laser (1+ x) y 'R))
	  ((and (char=? c #\\) (eq? d 'U)) (laser (1- x) y 'L))
	  ((and (char=? c #\-) (eq? d 'U)) (laser (1- x) y 'L)
           (laser (1+ x) y 'R))
	  ((and (char=? c #\|) (eq? d 'U)) (laser x (1- y) 'U))
	  ((eq? d 'U) (error "no match U"))

	  ;; down
	  ((and (char=? c #\.) (eq? d 'D)) (laser x (1+ y) 'D))
	  ((and (char=? c #\/) (eq? d 'D)) (laser (1- x) y 'L))
	  ((and (char=? c #\\) (eq? d 'D)) (laser (1+ x) y 'R))
	  ((and (char=? c #\-) (eq? d 'D)) (laser (1- x) y 'L)
           (laser (1+ x) y 'R))
	  ((and (char=? c #\|) (eq? d 'D)) (laser x (1+ y) 'D))
	  ((eq? d 'D) (error "no match D"))
	  (#t (error "laser" (list vv x y d width height c))))))))
    (laser x y d)))



(define (test)
  (set! hot '())
  (laser-entry example 0 0 'R))
;; hot .length = 23 


(define (part-1)
  (set! hot '())
  (laser-entry input 0 0 'R))
;; hot.length 1834


     
     
     
#|







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



;; --------------- (new-beams (x2 y2 left) --------------------
(defmacro new-beams! ( . args)
  (cond
   ((null? (cdr args))
    (let ((v1 (car args)))
      `(let ((s ,(list 'list (first v1) (second v1) `',(third v1))))
	 (cond
	  ((member s history) #f)
	  (#t (set! history (cons s history))
	      (set! new-beams (cons s  new-beams)))))))
   (#t
    (let ((v1 (car args))
	  (v2 (cadr args)))
      `(let ((s1 ,(list 'list (first v1) (second v1) `',(third v1)))
	     (s2 ,(list 'list (first v2) (second v2) `',(third v2))))
	 (cond
	  ((member s1 history) #f)
	  (#t
	   (set! history (cons s1 history))
	   (set! new-beams (cons s1 new-beams))))
	 (cond
	  ((member s2 history) #f)
	  (#t
	   (set! history (cons s2 history))
	   (set! new-beams (cons s2 new-beams)))))))))


(let ((new-beams '()))
  (new-beams! (1 2 left) (3 4 right))
  new-beams)

(let ((new-beams '()))
  (new-beams! (1 2 left))
  new-beams)


(let ((new-beams '()))
  (let ((x2 3)
	(y2 4))
    (new-beams! (x2 y2 left) (x2 y2 right))
    new-beams))

;; ------------------------------------------

(defmacro empty-e? ()  `(char=? e #\.))

(defmacro horz? ()  `(char=? e #\-))

(defmacro vert? ()  `(char=? e #\|))

;; \ north west
(defmacro nw? ()  `(char=? e #\\))

;; / north east
(defmacro ne? ()  `(char=? e #\/))


(let ((e #\/))
  (list (empty-e?) (horz?) (vert?) (nw?) (ne?)))

(let ((e #\\))
  (list (empty-e?) (horz?) (vert?) (nw?) (ne?)))

(let ((e #\-))
  (list (empty-e?) (horz?) (vert?) (nw?) (ne?)))

(let ((e #\|))
  (list (empty-e?) (horz?) (vert?) (nw?) (ne?)))

(let ((e #\.))
  (list (empty-e?) (horz?) (vert?) (nw?) (ne?)))

;; --------------------------------------------------------
(defmacro off-grid? ()
  `(or (< x2 0)(>= x2 width)(< y2 0)(>= y2 height)))

(defmacro get-xy! (x y)
  `(cond
    ((or (< ,x 0)(>= ,x width)(< ,y 0)(>= ,y height)) #\?)
    (#t (vector-ref (vector-ref g ,y) ,x))))

;;(let ((width 10)(height 12)(x 5)(y 4))  (get-xy! 3 4))


;; -----------------------------------------------------

(define (find-energised g)
  (let ((hot '((0 0)))
	(history '())
	(width (grid-width g))
	(height (grid-height g)))
    ;;(format #t "width = ~a : height = ~a ~%" width height)
    (letrec ((foo (lambda (beams)
		    ;;(format #t "beams = ~a ~%" beams)
		    (cond
		     ((null? beams) #f)
		     (#t
		      (let ((new-beams '()))
			(format #t "beams length ~a ~%" (length beams))
		      (do-list (b beams)
			       (match b
				 ((x y dir)
				  ;;(format #t "x = ~a : y = ~a : dir = ~a ~%" x y dir)
				  (cond  ;; record where beams go
				   ((member (list x y) hot equal?) #f)
				   (#t (set! hot (cons (list x y) hot))))
				  (cond
				   ((or (< x 0)(>= x width)(< y 0)(>= y height)) #f)				   
				   ((eq? dir 'L ) (let* ((x2 (- x 1)) (y2 y)(e (get-xy! x2 y2)))
						    (cond
						     ((off-grid?) #f)
							((empty-e?) (new-beams! (x2 y2 L)))
							((horz?) (new-beams! (x2 y2 L)))
							((vert?) (new-beams! (x2 y2 U) (x2 y2 D)))
							((nw?) (new-beams! (x2 y2 U)))
							((ne?) (new-beams! (x2 y2 D)))
							(#t (error "char-mirror-1")))))
				   ((eq? dir 'R ) (let* ((x2 (+ x 1)) (y2 y)(e (get-xy! x2 y2)))
						       (cond
						     ((off-grid?) #f)
							((empty-e?) (new-beams! (x2 y2 R)))
							((horz?) (new-beams! (x2 y2 R)))
							((vert?) (new-beams! (x2 y2 U) (x2 y2 D)))
							((nw?) (new-beams! (x2 y2 D)))
							((ne?) (new-beams! (x2 y2 U)))
							(#t (error "char-mirror-2")))))
				   ;;
				   ((eq? dir 'U ) (let* ((x2 x)(y2 (- y 1))(e (get-xy! x2 y2)))
						       (cond
						     ((off-grid?) #f)
							((empty-e?) (new-beams! (x2 y2 U)))
							((horz?) (new-beams! (x2 y2 L)(x2 y2 R)))
							((vert?) (new-beams! (x2 y2 U)))
							((nw?) (new-beams! (x2 y2 L)))
							((ne?) (new-beams! (x2 y2 R)))
							(#t (error "char-mirror-3")))))
				   ((eq? dir 'D ) (let* ((x2 x)(y2 (+ y 1))(e (get-xy! x2 y2)))
						       (cond
						     ((off-grid?) #f)
							((empty-e?) (new-beams! (x2 y2 D)))
							((horz?) (new-beams! (x2 y2 L)(x2 y2 R) ))
							((vert?) (new-beams! (x2 y2 D)))
							((nw?) (new-beams! (x2 y2 R)))
							((ne?) (new-beams! (x2 y2 L)))
							(#t (error "char-mirror-4")))))
				   (#t (error "no direction " (list dir)))))
				 (_ (error "no match"))))
		      (foo new-beams)))))))
      (foo '((0 0 R)))
      hot)))





;;(trace foo)


;;(find-energised example)
;;(off-grid?)


|#






    
