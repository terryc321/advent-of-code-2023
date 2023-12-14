#|

day14

part-2 - disagreement

1 verified input correct and matches original input
through io to seperate file input2
diff input input2

2  know number of O s on board is constant
so after any move of an O , can always verify still constant number of Os

3 number of cube shaped rocks # is also constant

4 size of grid is constant

5 after tilt north
should not have empty space above any O
.
O

similar for tilt-east
O . should not find

similr for tilt west
. O should not find

similar for tilt south
O
. should not find

6 when tilt north or south number of O 's in column is constant

7 when tilt east or tilt west number of O's in row is constant



|#


(use-modules (ice-9 textual-ports))
(use-modules (ice-9 format)) ;; format common lisp 
(use-modules (ice-9 pretty-print)) ;; pretty-print
(define pp pretty-print)
;;(use-modules (rnrs)) ;; assert 
(use-modules (srfi srfi-1)) ;; first second third ...
(use-modules (srfi srfi-2)) ;; first second third ...
(use-modules (statprof)) ;; statistical profiler

(use-modules (rnrs)) ;; assert

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

;;(load "../guile-imports.scm")

;;(load "chicken-imports.scm")
;;(define get-char read-byte)

(define (get-input filename)
  (define in (open-input-file filename))

  (define (read-line)
    (let ((res '()))
      (define (iter)
	(let ((ch (get-char in)))
	  (cond
	   ((eof-object? ch) (reverse res))
	   ((char=? ch #\newline) (reverse res))
	   (#t (set! res (cons ch res))
	       (iter)))))
      (iter)))

  (define (read-object2)
    (let ((res '()))
      (define (keep-reading3)
	(let ((line (read-line)))
	  (cond
	   ((eof-object? line) (reverse res))
	   ((null? line) (reverse res))
	   (#t (set! res (cons line res))
	       (keep-reading3)))))
      (keep-reading3)))

  (define (forever fn n limit)
    (cond
     ((> n limit) #t)
     (#t (fn)
	 (forever fn (+ n 1) limit))))

  (define (read-all-objects)
    (let ((res '()))
      (define (keep-reading2)
	(let ((obj (read-object2)))
	  (cond
	   ((eof-object? obj)
	    (close-input-port in) ;; port?? 
	    (reverse res))
	   ((null? obj) (reverse res))	 
	   (#t (set! res (cons obj res))
	       (keep-reading2)))))
      (keep-reading2)))

  (define (my-read)
    (read-all-objects))

  (my-read))


(define input (first (get-input "input")))

;; input 100x100

(define input2 (first (get-input "input")))


(define example
  (map string->list '(
		      "OOOO.#.O.." 
		      "OO..#....#" 
		      "OO..O##..O" 
		      "O..#.OO..." 
		      "........#." 
		      "..#....#.#" 
		      "..O..#.O.O" 
		      "..O......." 
		      "#....###.." 
		      "#....#....")))




;; tilt is destructive on the vector ?
(define (tilt-north v)
  (let ((width (length (list-ref v 0)))
	(height (length v))
	(moved #f))
    (dolist (h (iota height)) ;; 0 to height-1 inclusive
	    (dolist (w (iota width))
		    (let ((ch (list-ref (list-ref v h) w)))
		      (cond
		       ((char=? ch #\O) (cond
					 ((and (> h 0) (char=? (list-ref (list-ref v (- h 1)) w) #\. ))
					  (list-set! (list-ref v (- h 1)) w #\O)
					  (list-set! (list-ref v h) w #\.)
					  (set! moved #t))))))))
    (cond
     (moved (tilt-north v))
     (#t v))))


(define (tilt-south v)
  (let ((width (length (list-ref v 0)))
	(height (length v))
	(moved #f))
    (dolist (h (iota height)) ;; 0 to height-1 inclusive
	    (dolist (w (iota width))
		    (let ((ch (list-ref (list-ref v h) w)))
		      (cond
		       ((char=? ch #\O) (cond
					 ((and (< h (1- height)) (char=? (list-ref (list-ref v (+ h 1)) w) #\. ))
					  (list-set! (list-ref v (+ h 1)) w #\O)
					  (list-set! (list-ref v h) w #\.)
					  (set! moved #t))))))))
    (cond
     (moved (tilt-south v))
     (#t v))))


(define (tilt-east v)
  (let ((width (length (list-ref v 0)))
	(height (length v))
	(moved #f))
    (dolist (h (iota height)) ;; 0 to height-1 inclusive
	    (dolist (w (iota width))
		    (let ((ch (list-ref (list-ref v h) w)))
		      (cond
		       ((char=? ch #\O) (cond
					 ((and (< w (1- width)) (char=? (list-ref (list-ref v h) (+ w 1)) #\. ))
					  (list-set! (list-ref v h) (+ w 1) #\O)
					  (list-set! (list-ref v h) w #\.)
					  (set! moved #t))))))))
    (cond
     (moved (tilt-east v))
     (#t v))))


(define (tilt-west v)
  (let ((width (length (list-ref v 0)))
	(height (length v))
	(moved #f))
    (dolist (h (iota height)) ;; 0 to height-1 inclusive
	    (dolist (w (iota width))
		    (let ((ch (list-ref (list-ref v h) w)))
		      (cond
		       ((char=? ch #\O) (cond
					 ((and (> w 0) (char=? (list-ref (list-ref v h) (- w 1)) #\. ))
					  (list-set! (list-ref v h) (- w 1) #\O)
					  (list-set! (list-ref v h) w #\.)
					  (set! moved #t))))))))
    (cond
     (moved (tilt-west v))
     (#t v))))


(define (cycle v)
  (format #t "~%~%INPUT TO CYCLE : ~%")
  (show-tilt v)
  (tilt-north v)
  (format #t "~%~%AFTER TILT NORTH~%")
  (show-tilt v)
  (tilt-west v)
  (format #t "~%~%AFTER TILT WEST~%")
  (show-tilt v)
  (tilt-south v)
  (format #t "~%~%AFTER TILT SOUTH~%")
  (show-tilt v)
  (tilt-east v)
  (format #t "~%~%AFTER TILT EAST~%")
  (show-tilt v)
)



(define (show-tilt v)
  (let ((width (length (list-ref v 0)))
	(height (length v))
	(moved #f))
    (dolist (h (iota height)) ;; 0 to height-1 inclusive
	    (dolist (w (iota width))
		    (let ((ch (list-ref (list-ref v h) w)))
		      (format #t "~a" ch)))
	    (format #t "~%"))))
  

(define (find-load v)
  (let ((width (length (list-ref v 0)))
	(height (length v))
	(tot 0))
    (dolist (h (iota height))
	    (dolist (w (iota width))
		    (let ((ch (list-ref (list-ref v h) w)))
		      (cond
		       ((char=? ch #\O) (let ((load (- height h)))
					  ;;(format #t "boulder at ~a,~a : load of ~a ~%" w h load)
					  (set! tot (+ tot load))))))))
    tot))


#|
(define (part-1)
  ;; tilt the table ...
  (tilt-north input2)
  (find-load input2))
|#



;; 137  102657
(define (predict n)
  (let ((index (modulo (- n 137) 21)))
    ;;(format #t "index = ~a ~%" index)
    (list-ref '(102657 102657 102655 102648 102651 102642 102643 102656 102660 102653
		       102647 102654 102640 102642 102659 102658 102652 102650 102652 102639
		       102645 102657 102657 102655 102648 102651 102642 102643)
	      index)))


(define (part-2)
  (let ((n 0))
    (while (< n 1000000000)
      (cycle input2)
      (let ((computed-load (find-load input2))
	    (predicted-load (predict n)))			     
	(format #t "cycle ~a : load ~a : predict ~a : match ~a ~%" n computed-load predicted-load
		(= computed-load predicted-load))
	(set! n (+ n 1))))))


(define (predict-example n)
  (let ((index (modulo (- n 12) 7)))
    ;;(format #t "index = ~a ~%" index)
    (list-ref '(65 64 65 63 68 69 69 65)
	      index)))

 

(define (work-example)
  (format #t "working example .......~%")
  (show-tilt example)
  ;; reload this file !!
  (let ((n 1))
    (while #t ;(< n 100)
      (cycle example)
      (let* ((computed-load (find-load example))
	     (predicted-load (predict-example n))
	     (same (= computed-load predicted-load)))
      (format #t "cycle ~a : load ~a : predicted ~a : same ~a ~%" n computed-load predicted-load same)
      ;;(show-tilt example)
      (set! n (1+ n))))))



(predict-example 1000000000)
;; 64

(predict 1000000000)
#|
;; 102655

answer rejected ?
is there something incorrect with our load computation ?
something wrong with predictor ?
something wrong with cycle?


|#


    

					  
    
