#|

changed original input symbol from hash # to letter P

only two symbols on
P and .

think about a piece of tracing paper 
only need to record where the P #s are on the sheet of paper 

  123
1 P..
2 ..P

(1 1)(3 1)

P|..
.|.P

place vertical mirror between column 1 and 2
anything less than 2 is reflected

1 2          1 2
P|.  becomes .|P

(1 ?) mirror between 1 and 2 =>   (2 ?)  :  hi-mirror - x + lo-mirror
hi-mirror 2
lo-mirror 1

if reflected point goes off image - considered valid reflection
if reflected onto point not on original image ,  inside bounds original image , invalid reflection



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

(use-modules (ice-9 textual-ports))
;;(load "chicken-imports.scm")
;;(define get-char read-byte)

(define in (open-input-file "input"))

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
	 ((eof-object? obj) (reverse res))
	 ((null? obj) (reverse res))	 
	 (#t (set! res (cons obj res))
	     (keep-reading2)))))
    (keep-reading2)))

(define (my-read)
  (read-all-objects))

(define p (my-read))

;; check output to test file
(define (spew)
  (define (print-line x)
    (dolist (c x)
	    (cond
	     ((char=? c #\P) (format #t "#"))
	     ((char=? c #\.) (format #t "."))))
    (format #t "~%"))
  (define (print-object x)
    (dolist (xs x)
	    (print-line xs)
	    )
    (format #t "~%"))
  (dolist (q p) (print-object q)))


(define (test)
    (with-output-to-file "test"
      (lambda ()
	(spew))))


(define (obj->points obj)
  (let ((x 0)
	(y 0)
	(rs '()))
    (dolist (row obj)
	    (set! y (+ y 1))
	    (set! x 1)
	    (dolist (c row)
		    (cond
		     ((or (char=? c #\#) (char=? c #\P)) (set! rs (cons (list x y #\P) rs)))
		     ((char=? c #\.) (set! rs (cons (list x y #\.) rs))))		     
		    (set! x (+ x 1))))
    rs))


;; points only collect Ps or #s so orignal object may be bigger...
;; input data has # on every edge far right and lower left
(define (bounds-of pts)
  (list 
   (apply min (map first pts))
   (apply max (map first pts))
   (apply min (map second pts))
   (apply max (map second pts))))

#|

1 2            1 2
.|P      ->    P|.

 max-x is inclusive , if reflection lands there , should be a P or # there
|#
(define (reflect-vert pts lo-mirror hi-mirror max-x max-y)
  (cond
   ((>= lo-mirror max-x) #f)
   (#t 
    (call/cc (lambda (escape)
	       (dolist (p pts)
		       (let ((x (first p))
			     (y (second p))
			     (ch (third p)))
			 (let ((refy y)
			       (refx (cond
				      ((< x hi-mirror) (+ lo-mirror (- hi-mirror x)))
				      (#t (- hi-mirror (- x lo-mirror))))))
			   (cond
			    ((and (>= refx 1) (<= refx max-x))
			     (cond
			      ((member (list refx refy ch) pts equal?) #t)
			      (#t (escape #f))))
			    (#t #t)))))
	       #t)))))


#|
no idea if this is correct
just carbon copied vertical reflection and replaced x with y , refx with refy
|#
(define (reflect-horz pts lo-mirror hi-mirror max-x max-y)
  (cond
   ((>= lo-mirror max-y) #f)
   (#t 
    (call/cc (lambda (escape)
	       (dolist (p pts)
		       (let ((x (first p))
			     (y (second p))
			     (ch (third p)))
			 (let ((refx x)
			       (refy (cond
				      ((< y hi-mirror) (+ lo-mirror (- hi-mirror y)))
				      (#t (- hi-mirror (- y lo-mirror))))))
			   (cond
			    ((and (>= refy 1) (<= refy max-y))
			     (cond
			      ((member (list refx refy ch) pts equal?) #t)
			      (#t (escape #f))))
			    (#t #t)))))
	       #t)))))



(define (find-reflect obj)
  (let ((max-y (length obj))
	(max-x (length (first obj)))
	(pts (obj->points obj))
	(res '())
	(ok #f))
    ;;(format #t "max-x ~a : max-y ~a ~%" max-x max-y)
    (dolist (p (cdr (iota (+ 1 max-x))))
	    ;;(format #t "trying reflection at ~a~%" p)
	    (let ((r (reflect-vert pts p (+ p 1) max-x max-y)))
	      (when r
		(when (eq? ok 'good) (format #t "DUPLiCATED ??~%")
		      (set! ok 'bad))
		(format #t "vertical mirror : with ~a : ~a => ~a ~%" p (+ p 1) r )
		(set! ok 'good)
		(set! res `(vert ,p ,(+ p 1)))
		)
	      ))
    (dolist (p (cdr (iota (+ 1 max-y))))
	    ;;(format #t "trying reflection at ~a~%" p)
	    (let ((r (reflect-horz pts p (+ p 1) max-x max-y)))
	      (when r
		(when (eq? ok 'good) (format #t "DUPLiCATED ??~%")
		      (set! ok 'bad))		
		(format #t "horiz mirror : with ~a : ~a => ~a ~%" p (+ p 1) r )
		(set! ok 'good)		
		(set! res `(horz ,p ,(+ p 1)))
		)
	      ))
    (cond
     ((eq? ok 'good) res)
     (#t 'bad))))


;; BUG !!! got sh and sv mixed up giving wrong answer
(define (part-1)
  (let ((rs (map find-reflect p)))
    (let ((horz (filter (lambda (x) (eq? (car x) 'horz)) rs))
	  (vert (filter (lambda (x) (eq? (car x) 'vert)) rs)))
      (format #t "horz = ~a ~%" horz)
      (format #t "vert = ~a ~%" vert)
      (let ((sh (apply + (map second horz)))
	    (sv (apply + (map second vert))))
	(+ sh (* 100 sv)))))) ;; should be (+ sv (* 100 sh))



(define horz! #f)
(define vert! #f)
(define neither! #f)

(define (retry2)
  (let ((rs (map find-reflect p)))
    (let ((horz (filter (lambda (x) (eq? (car x) 'horz)) rs))
	  (vert (filter (lambda (x) (eq? (car x) 'vert)) rs))
	  (neither (filter (lambda (x) (not (or (eq? (car x) 'horz)
						(eq? (car x) 'vert))))
			   rs)))
      (format #t "horz = ~a : ~a ~%" horz (length horz))
      (format #t "vert = ~a : ~a ~%" vert (length vert))
      (format #t "neither! = ~a : ~a ~%" neither (length neither))
      
      (set! neither! neither)
      (set! horz! horz)
      (set! vert! vert)

      (+ (apply + (map second vert))
	 (* 100 (apply + (map second horz)))))))


#|

retry 2

34918
......... acceepted answer




|#

#|

buggy as more vertical solutions than horizontal line solutions so cant just iterate down them
stop when no more horz or whatever ... snot synced up..

(define (part-1-retry)
  (let ((rs (map find-reflect p)))
    (let ((horz (filter (lambda (x) (eq? (car x) 'horz)) rs))
	  (vert (filter (lambda (x) (eq? (car x) 'vert)) rs))
	  (neither (filter (lambda (x) (not (or (eq? (car x) 'horz)
						(eq? (car x) 'vert))))
			   rs)))
      (format #t "horz = ~a : ~a ~%" horz (length horz))
      (format #t "vert = ~a : ~a ~%" vert (length vert))
      (format #t "neither! = ~a : ~a ~%" neither (length neither))
      
      (set! neither! neither)
      (set! horz! horz)
      (set! vert! vert)
      (let ((hd horz)
	    (tot 0)
	    (hd2 vert))
	(letrec ((loop (lambda ()
			 (cond
			  ((null? hd) tot)
			  (#t (let ((h (second (car hd)))
				    (v (second (car hd2))))
				(format #t "h = ~a : v = ~a ~%" h v )
				(set! tot (+ tot (+ h (* 100 v))))
				(set! hd (cdr hd))
				(set! hd2 (cdr hd2))))))))
	  (loop)
	  tot)))))
|#





#|
horz = ((horz 4 5) (horz 1 2) (horz 3 4) (horz 6 7) (horz 3 4) (horz 9 10) (horz 3 4) (horz 6 7) (horz 15 16) (horz 10 11) (horz 16 17) (horz 3 4) (horz 2 3) (horz 6 7) (horz 6 7) (horz 9 10) (horz 1 2) (horz 2 3) (horz 5 6) (horz 13 14) (horz 4 5) (horz 14 15) (horz 4 5) (horz 15 16) (horz 9 10) (horz 5 6) (horz 13 14) (horz 4 5) (horz 5 6) (horz 1 2) (horz 7 8) (horz 1 2) (horz 14 15) (horz 7 8) (horz 14 15) (horz 15 16) (horz 9 10) (horz 1 2) (horz 3 4) (horz 14 15) (horz 1 2) (horz 2 3) (horz 3 4) (horz 8 9) (horz 12 13) (horz 7 8) (horz 12 13) (horz 3 4) (horz 16 17)) 
vert = ((vert 13 14) (vert 1 2) (vert 11 12) (vert 10 11) (vert 12 13) (vert 3 4) (vert 9 10) (vert 1 2) (vert 1 2) (vert 1 2) (vert 9 10) (vert 1 2) (vert 2 3) (vert 10 11) (vert 1 2) (vert 1 2) (vert 1 2) (vert 10 11) (vert 13 14) (vert 5 6) (vert 10 11) (vert 10 11) (vert 4 5) (vert 1 2) (vert 4 5) (vert 12 13) (vert 7 8) (vert 16 17) (vert 10 11) (vert 12 13) (vert 3 4) (vert 6 7) (vert 3 4) (vert 3 4) (vert 10 11) (vert 14 15) (vert 2 3) (vert 1 2) (vert 1 2) (vert 3 4) (vert 3 4) (vert 8 9) (vert 1 2) (vert 3 4) (vert 16 17) (vert 14 15) (vert 1 2) (vert 3 4) (vert 15 16) (vert 1 2) (vert 6 7)) 

$34 = 32146

nope ... okay....


|#

(define (example)
  (map string->list
       '(
	 "#...##..#"
	 "#....#..#"
	 "..##..###"
	 "#####.##."
	 "#####.##."
	 "..##..###"
	 "#....#..#")))


(define (example-2)
  (map string->list
       '(
	 "#.##..##."
	 "..#.##.#."
	 "##......#"
	 "##......#"
	 "..#.##.#."
	 "..##..##."
	 "#.#.##.#."
	 )))

(find-reflect (example))

(find-reflect (example-2))




    













  
  
  
  
  














		     
		     
		     










  
