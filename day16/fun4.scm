
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

record when laser hit x y at time t

so we can visualise by sorting output based on time t
replay using viz.scm



|#

(define hot '())

;; compare only x-y-direction , ignore time stamp
(define (compare-only-xyd? r s)
  (and (= (first r)(first s))
       (= (second r)(second s))
       (eq? (fourth r)(fourth s))))



(define (hit x y d t)
  (let ((s (list x y t d)))
    (cond
     ;;((member s hot equal?) #f)
     ((member s hot compare-only-xyd?) #f)
     (#t (set! hot (cons s hot))))))

(define (laser-entry vv x y d t)
  (let ((width (grid-width vv))
	(height (grid-height vv)))
    (define (laser x y d t)
      (cond
       ;; off board
       ((or (< x 0)(>= x width)(< y 0)(>= y height)) #f)
       (#t
	(let ((c (get-xy vv x y)))

	  ;; laser is at x y
	  (hit x y d t)
	  (format #t "hot .length = ~a ~%" (length hot))
	  (cond
	  ;; right
	  ((and (char=? c #\.) (eq? d 'R)) (laser (1+ x) y 'R (1+ t)))
	  ((and (char=? c #\/) (eq? d 'R)) (laser x (1- y) 'U (1+ t)))
	  ((and (char=? c #\\) (eq? d 'R)) (laser x (1+ y) 'D (1+ t)))
	  ((and (char=? c #\-) (eq? d 'R)) (laser (1+ x) y 'R (1+ t)))
	  ((and (char=? c #\|) (eq? d 'R))
	   (laser x (1- y) 'U (1+ t))
           (laser x (1+ y) 'D (1+ t)))
	  ((eq? d 'R) (error "no match R"))
	  ;; left
	  ((and (char=? c #\.) (eq? d 'L)) (laser (1- x) y 'L (1+ t)))
	  ((and (char=? c #\/) (eq? d 'L)) (laser x (1+ y) 'D (1+ t)))
	  ((and (char=? c #\\) (eq? d 'L)) (laser x (1- y) 'U (1+ t)))
	  ((and (char=? c #\-) (eq? d 'L)) (laser (1- x) y 'L (1+ t)))
	  ((and (char=? c #\|) (eq? d 'L))
	   (laser x (1- y) 'U (1+ t))
           (laser x (1+ y) 'D (1+ t)))
	  ((eq? d 'L) (error "no match L"))
	  
	  ;; up
	  ((and (char=? c #\.) (eq? d 'U)) (laser x (1- y) 'U (1+ t)))
	  ((and (char=? c #\/) (eq? d 'U)) (laser (1+ x) y 'R (1+ t)))
	  ((and (char=? c #\\) (eq? d 'U)) (laser (1- x) y 'L (1+ t)))
	  ((and (char=? c #\-) (eq? d 'U))
	   (laser (1- x) y 'L (1+ t))
           (laser (1+ x) y 'R (1+ t)))
	  ((and (char=? c #\|) (eq? d 'U)) (laser x (1- y) 'U (1+ t)))
	  ((eq? d 'U) (error "no match U"))

	  ;; down
	  ((and (char=? c #\.) (eq? d 'D)) (laser x (1+ y) 'D (1+ t)))
	  ((and (char=? c #\/) (eq? d 'D)) (laser (1- x) y 'L (1+ t)))
	  ((and (char=? c #\\) (eq? d 'D)) (laser (1+ x) y 'R (1+ t)))
	  ((and (char=? c #\-) (eq? d 'D))
	   (cond
	    ((= t 674) (format #t "step 674 : c = ~a : d = ~a ***************************~%" c d))
	    ((= t 206) (format #t "step 206 : c = ~a : d = ~a *************************** ~%" c d))
	    )
	   (laser (1- x) y 'L (1+ t))
           (laser (1+ x) y 'R (1+ t)))
	  ((and (char=? c #\|) (eq? d 'D)) (laser x (1+ y) 'D (1+ t)))
	  ((eq? d 'D) (error "no match D"))
	  (#t (error "laser" (list vv x y d width height c))))))))
    (laser x y d t)))



(define (results)
  (sort hot (lambda (x y) (< (third x)(third y)))))


;; x y direction time-t
(define (test)
  (set! hot '())
  (let ((x 0)(y 0)(dir 'R)(t 0))
    (laser-entry example x y dir t)))
;; hot .length = 23 


(define (part-1)
  (set! hot '())
  (let ((x 0)(y 0)(dir 'R)(t 0))
    (laser-entry input 0 0 'R t)))

#|
;; hot.length 1834
;; hot .length = 1903 

now saying hot length 1903

if laser at x y in direction d that is unique
then record that as time stamped
x y time direction

sorting based on third entry time






(part-1)
(length (results)) => 1834

(results)
to see results see at end
....
3 1608) (49 24 1609) (49 26 1611) (49 27 1612) (49 28 1613) (49 29
1614) (49 30 1615) (49 31 1616) (49 33 1618) (49 34 1619) (49 35 1620)
(49 36 1621)
(49 37 1622) (49 39 1624) (49 40 1625) (48 40 1626) (47 40 1627))

last line ends on time step 1627
even though 1834 squares are occupied , it took only 1627 to cover those squares

this is because lasers split up and cover more ground than single laser being
rerflected , more than one single laser could





|#

