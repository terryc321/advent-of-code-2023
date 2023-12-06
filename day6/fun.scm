#|
(getcwd)
(chdir "day6")
|#

(use-modules (ice-9 format)) ;; format common lisp 
(use-modules (ice-9 pretty-print)) ;; pretty-print
(define pp pretty-print)
;;(use-modules (rnrs)) ;; assert 
(use-modules (srfi srfi-1)) ;; first second third ...
(use-modules (srfi srfi-2)) ;; first second third ...


(use-modules (rnrs)) ;; assert

;; regular expression
(use-modules (ice-9 regex)) 

;; pattern matcher ?
(use-modules (ice-9 match))

;; binary io -- dont know if this helped any as read-u8 is for reading ints no??
(use-modules (ice-9 binary-ports))

;; r7rs 
(use-modules (scheme base))

#|
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

(defmacro while (condition . body)
  (let ((lup (gensym "loop")))
    `(letrec ((,lup (lambda ()
		      (when ,condition
			,@body
			(,lup)))))
       (,lup))))

|#

;; (let ((i 0))
;;   (while (< i 10)
;;     (format #t " i = ~a ~%" i )
;;     (set!  i (+ i 1))))

;; --------------------- macros --------------------------

#|
puzzle

|#


(define *debug* #f)

(define input #f)
(define input2 #f)

(define (get-input filename)  
  (call-with-port (open-input-file filename)
    (lambda (port)
      (read port))))

(set! input (get-input "input"))
(set! input (map list (first input) (second input)))




(define example
  (let ((xs '((  7  15   30)
	      (  9  40  200))))
    (map list (first xs) (second xs))))



;;---------------------------
;; iterative version
;; suffers from having to manually execute each state...
;; -----------------------------
(define (distance s d t tlim)
  (cond
   ((> t tlim) d)
   (#t (distance s (+ d s) (+ t 1) tlim))))

(define (hold-for2 n tlim)
  (distance n 0 (+ n 1) tlim))
;;------------------------------
;; can we write a better more efficient hold-for ?
#|

hold button down for say 6 seconds of 7 second race , only got 1 second to travel at 6 millimetres/second

|#
(define (hold-for3 n tlim)
  (let ((time-remain (- tlim n))
	(speed n))
    (* time-remain speed)))


(define (hold-for n tlim)
  (hold-for3 n tlim))

  #|
  (let ((r1 (hold-for2 n tlim))
	(r2 (hold-for3 n tlim)))
    ;;(format #t "hold-for ~a with time limit ~a : " n tlim)
    ;;(format #t "r1 ~a ; r2 ~a ~%" r1 r2)
    (when (not (equal? r1 r2))
      (error "not-equal" (list r1 r2 n tlim)))
    r1))
  |#
  

;; -----------------------------------

(define (search tlim)
  (map (lambda (n) (hold-for n tlim)) (iota (+ tlim 1))))

;; time lim and distance limit
(define (search2 tlim dlim)
  (length
   (filter (lambda (x) (> x dlim))
	   (map (lambda (n) (hold-for n tlim)) (iota (+ tlim 1))))))

(define (search3 tlim dlim)
  (let ((wins 0))
    (letrec ((foo (lambda (n)
		    ;; (when (zero? (mod n 1000))
		    ;;   (format #t "n : ~a  ..... wins ~a ~%" n wins))
		    (cond
		     ((>= n tlim) #f)
		     (#t
		      (let ((h (hold-for n tlim)))
			(cond
			 ((> h dlim)
			  (set! wins (+ wins 1)))))
		      (foo (+ n 1))))))
	     )
      (foo 0)
      wins)))



(define (process xs)
  (let ((ys (map (lambda (x)
		   (match x
		     ((tlim dlim)
		      (search2 tlim dlim))))
		 xs)))
    (values ys (apply * ys))))




;; --------- test cases ----??
(define (check e)
  (cond
   (e e)
   (#t (format #t "check failed on example ~a : expected ~a : given ~a ~%" e e e))))


(define (check-example)
  (let ((res (search 7))
	(expected '(0 6 10 12 12 10 6 0)))
    (check (equal? res expected))))

;; ----------------------------------


(define (part-1)
  (process input))

(define (example-1)
  (process example))


#|
scheme@(guile-user)> (process example)
$34 = (4 8 9)
$35 = 288
scheme@(guile-user)> (process input)
$36 = (25 51 51 51)
$37 = 3316275

3316275    ................     

|#


(define (example-2)
  (let ((tlim 71530)
	(dlim 940200))
    (search3 tlim dlim)))


;;(example-2)

(define (part-2)
  (let* ((s1 (apply string-append (map (lambda (x) (format #f "~a" (car x))) input)))
	 (s2 (apply string-append (map (lambda (x) (format #f "~a" (cadr x))) input)))
	 (tcat (string->number s1))
	 (dcat (string->number s2)))
    (format #t "part2 ~a ~a : ~a ~a ~%" s1 tcat s2 dcat)
    (search3 tcat dcat)))


#|

scheme@(guile-user)> ,t (part-2)
part2 40828492 40828492 : 233101111101487 233101111101487 
$10 = 27102791

;; 1.021801s real time, 1.021315s run time.  0.000000s spent in GC.
scheme@(guile-user)> input
$11 = ((40 233) (82 1011) (84 1110) (92 1487))

27102791
accepted

How can i have multiple implementations of a particular problem
running in parallel
available to be run in parallel if desired
or how change "now i want to run with algroithm 2 and leave everything else as it is"
should be a one line change at most

|#
  
  
  
   
		       
  








