
#|

day16

5th version , still trying to work out how to see where lasers go

rather than depth first
if do breadth first

from given beams active at time step T => give next beams active at time step T + 1





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
(define hot-count 0)
(define hot-vector #f)

(define width 0)
(define height 0)
(define vv #f)

(define nloop 0)

;; compare only x-y-direction , ignore time stamp
(define (compare-only-xyd? r s)
  (and (= (first r)(first s))
       (= (second r)(second s))
       (eq? (fourth r)(fourth s))))



(define (hot-xy? x y)
  (vector-ref (vector-ref hot-vector y) x))

(define (set-hotxy x y n)
  (vector-set! (vector-ref hot-vector y) x n))


(define (hit g)
;;  (format #t "check g : hit : ~a ~%" g)
  (match g
    ((x y d t) 
     (cond
      ((hot-xy? x y) #f)
      (#t (set-hotxy x y t)
	  ;;(format #t "HO T   HO T        T O H  T  O  H ~%")
	  (set! hot-count (+ 1 hot-count))
	  (format #t "hit [~a , ~a] at time [~a]  : hot count [~a] ~%" x y t hot-count)
	  (cond
	   ((= t (hot-xy? x y)) 'ok)
	   (#t (error "hit function did not record my hit !!~%"))))))))




     ;; ;;((member s hot equal?) #f)
     ;; ((member s hot compare-only-xyd?) #f)
     ;; (#t (set! hot (cons s hot))))))


(define (laser-right state c)
  (match state
    ((x y d t)
     (cond
      ((char=? c #\.)  (list (list (1+ x) y 'R (1+ t))))
      ((char=? c #\/)  (list (list x (1- y) 'U (1+ t))))
      ((char=? c #\\)  (list (list x (1+ y) 'D (1+ t))))
      ((char=? c #\-)  (list (list (1+ x) y 'R (1+ t))))
      ((char=? c #\|)  (list (list x (1- y) 'U (1+ t)) (list x (1+ y) 'D (1+ t))))
      (#t (error "bad-dir right"))))))


(define (laser-left state c)
  (match state
    ((x y d t)
     (cond
      ((char=? c #\.)  (list (list (1- x) y 'L (1+ t))))
      ((char=? c #\/)  (list (list x (1+ y) 'D (1+ t))))
      ((char=? c #\\)  (list (list x (1- y) 'U (1+ t))))
      ((char=? c #\-)  (list (list (1- x) y 'L (1+ t))))
      ((char=? c #\|)  (list (list x (1- y) 'U (1+ t)) (list x (1+ y) 'D (1+ t))))
      (#t (error "bad-dir left"))))))


(define (laser-up state c)
  (match state
    ((x y d t)
     (cond
      ((char=? c #\.)  (list (list x (1- y) 'U (1+ t))))
      ((char=? c #\/)  (list (list (1+ x) y 'R (1+ t))))
      ((char=? c #\\)  (list (list (1- x) y 'L (1+ t))))
      ((char=? c #\-) 
       (list (list (1- x) y 'L (1+ t))
	     (list (1+ x) y 'R (1+ t))))
      ((char=? c #\|)  (list (list x (1- y) 'U (1+ t))))
      (#t (error "bad-dir up"))))))


(define (laser-down state c)
  (match state
    ((x y d t)
     (cond
      ((char=? c #\.)  (list (list x (1+ y) 'D (1+ t))))
      ((char=? c #\/)  (list (list (1- x) y 'L (1+ t))))
      ((char=? c #\\)  (list (list (1+ x) y 'R (1+ t))))
      ((char=? c #\-) 
       (list (list (1- x) y 'L (1+ t)) (list (1+ x) y 'R (1+ t))))
      ((char=? c #\|) (list (list x (1+ y) 'D (1+ t))))
      (#t (error "bad-dir down"))))))


(define (laser state) ;; x y d t
  (match state
    ((x y d t)
     (cond
      ;; off board
      ((or (< x 0)(>= x width)(< y 0)(>= y height)) #f)
      (#t
       (let ((c (get-xy vv x y)))
	 ;; laser is at x y
	 ;;(hit x y d t)
	 ;;(format #t "hot .length = ~a ~%" (length hot))
	 (cond
	  ((eq? d 'R) (laser-right state c))
	  ((eq? d 'L) (laser-left state c))
	  ((eq? d 'U) (laser-up state c))
	  ((eq? d 'D) (laser-down state c))
	  (#t (error "laser bad direction general")))))))))

(define (laser2 state)
  (let ((r (laser state)))
    ;;(format #t "R = ~a : incoming ~a ~%" r state)
    r))

(define (live-onboard? s)
  (if s
      (match s
	((x y d t) (and (>= x 0)(< x width)(>= y 0)(< y height))))
      #f))



(define (laser-loop-helper states)
  (format #t "loop ~a : hot-count ~a ~%" nloop hot-count)
  (set! nloop (1+ nloop))
  (do-list (g states)
	   ;;(format #t "~a~%" g)
	   (hit g))
  (let ((next-gen (filter live-onboard?
			  (apply append
				 (filter (lambda (x)(if x x #f))
					 (map laser2 states))))))
    ;;(format #t "next-gen ~a ~%" next-gen)
    (laser-loop-helper next-gen)))






(define (laser-loop x y d t)
  (let ((states (list (list x y d t))))
    (laser-loop-helper states)))


(define (laser-entry vv2 x y d t)  
  (set! vv vv2)
  (set! width (grid-width vv))
  (set! height (grid-height vv))
  (set! nloop 0)
  (set! hot-vector (make-vector height))
  (do-list (i (iota height))
	   (vector-set! hot-vector i (make-vector width #f)))
  (set! hot-count 0)
  (laser-loop x y d t))


(define (results)
  (let ((wl (iota width))
	(hl (iota height))
	(tot 0))
    (do-list (y hl)
	    (do-list (x wl)
		    (let ((t (vector-ref (vector-ref hot-vector y) x)))
		      (cond
		       ((and t (integer? t)) (set! tot (+ 1 tot)))))))
    tot))


(define (results2)
  (let ((wl (iota width))
	(hl (iota height))
	(xs '()))
    (do-list (y hl)
	    (do-list (x wl)
		    (let ((t (vector-ref (vector-ref hot-vector y) x)))
		      (cond
		       ((and t (integer? t)) (set! xs (cons (list x y 'U t) xs)))))))
    (sort xs (lambda (x y)(< (fourth x)(fourth y))))))




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

check 2 --- 7392 --- iterating over vector 2d for integers

after some ruminating ...... visualiser ...... 7392 ... seems to be latest hot-count


loop 624 : hot-count 7390 
loop 625 : hot-count 7390 
hit [0 , 33] at time [625]  : hot count [7391] 
hit [39 , 30] at time [625]  : hot count [7392] 
loop 626 : hot-count 7392 
loop 627 : hot-count 7392 



;; hot.length 1834
;; hot .length = 1903 

now saying hot length 1903

if laser at x y in direction d that is unique
then record that as time stamped
x y time direction

sorting based on third entry time

part-1)
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

