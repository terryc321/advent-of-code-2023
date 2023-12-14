#|

day14


|#


(use-modules (ice-9 textual-ports))
(use-modules (ice-9 format)) ;; format common lisp 
(use-modules (ice-9 pretty-print)) ;; pretty-print
(define pp pretty-print)
;;(use-modules (rnrs)) ;; assert 
(use-modules (srfi srfi-1)) ;; first second third ...
(use-modules (srfi srfi-2)) ;; first second third ...
(use-modules stat-prof) ;; statistical profiler

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
(define (tilt v)
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
     (moved (tilt v))
     (#t v))))


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
					  (format #t "boulder at ~a,~a : load of ~a ~%" w h load)
					  (set! tot (+ tot load))))))))
    tot))


(define (find-load2 v)
  (let ((width (length (list-ref v 0)))
	(height (length v))
	(tot 0))
    (dolist (h (iota height))
	    (dolist (w (iota width))
		    (let ((ch (list-ref (list-ref v h) w)))
		      (cond
		       ((char=? ch #\O) (let ((load (- height h)))
					  (format #t "boulder at ~a,~a : load of ~a ~%" w h load)
					  (set! tot (+ tot load))))))))
    tot))



(define (part-1)
  ;; tilt the table ...
  (tilt input2)
  (find-load input2))



#|

......
.........
.......
boulder at 14,99 : load of 1 
boulder at 27,99 : load of 1 

$59 = 109638


|#


					  
    
