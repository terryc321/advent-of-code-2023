
(import scheme)

(import simple-exceptions)

(import (chicken string))
(import (chicken pretty-print))
(define pp pretty-print)

(import (chicken format))
(import (chicken sort))
(import (chicken file))
(import (chicken process-context))
;; (change-directory "day11")
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


;;------------------------- code -----------------------------------

;; change input file ! 
(define (get-input) (call-with-input-file "input"
		      (lambda (port)
			(read port))))

(define input (get-input))

(define example
  "...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....")

(set! input example)

(define (1+ x) (+ x 1))
(define (1- x) (- x 1))

(define (10+ x) (+ x 10))
(define (10- x) (- x 10))

(define rest cdr)

;;--------------------------------------------------------------------
#|

read coordinates of galaxies marked #
top left 0 0


|#

(define *expansion-rate* (make-parameter (- 2 1)))
(define coords '())
(define nulls-x '())
(define nulls-y '())

(define (reset)
  (set! coords '())
  (set! nulls-x '())
  (set! nulls-y '())
  )

(define (found-galaxy x y)
  (format #t "found galaxy at ~a , ~a ~%" x y)
  (set! coords (cons (list x y) coords)))

(define (parse-horz-xy str x y slen)
  (cond
   ((>= x slen) #f)
   (#t (let ((ch (string-ref str x)))
	 (cond
	  ((char=? ch #\#) (found-galaxy x y))
	  ((char=? ch #\.) #f)
	  (#t (error "parse-horz-xy" (list 'unrecognised 'char))))
	 (parse-horz-xy str (+ x 1) y slen)))))

(define (parse-horizontal str y)
  (let ((slen (string-length str)))
    (parse-horz-xy str 0 y slen)))

(define (parse-lines lines n)
  (cond
   ((null? lines) #f)
   (#t
    (parse-horizontal (car lines) n)
    (parse-lines (cdr lines) (+ n 1)))))

(define (parse dat)
  (let* ((in dat)
	 (lines (string-split in "\n")))
    (parse-lines lines 0)))


;; find empty rows columns
(define (find-nulls)
  (let ((min-x (apply min (map car coords)))
	(max-x (apply max (map car coords)))
	(min-y (apply min (map cadr coords)))
	(max-y (apply max (map cadr coords))))
    (list min-x max-x min-y max-y)
    (find-nulls-x min-x max-x coords)
    (find-nulls-y min-y max-y coords)
    (set! nulls-x (reverse nulls-x))
    (set! nulls-y (reverse nulls-y))
    (format #t "nulls-x ~a ~%" nulls-x)
    (format #t "nulls-y ~a ~%" nulls-y)    
    ))

(define (find-nulls-x x mx coordinates)
  (cond
   ((> x mx) #f)
   (#t
    (call/cc (lambda (escape)
	       (do-list (c coordinates)
			(let ((cx (car c)))
			  (when (= cx x)
			    (escape #f))))
	       ;; otherwise add x to nulls-x
	       (set! nulls-x (cons x nulls-x))))    
    (find-nulls-x (+ x 1) mx coordinates))))


(define (find-nulls-y y my coordinates)
  (cond
   ((> y my) #f)
   (#t
    (call/cc (lambda (escape)
	       (do-list (c coordinates)
			(let ((cy (cadr c)))
			  (when (= cy y)
			    (escape #f))))
	       ;; otherwise add y to nulls y
	       (set! nulls-y (cons y nulls-y))))    
    (find-nulls-y (+ y 1) my coordinates))))


(define (expand-universe)
  (expand-universe-x)
  (expand-universe-y))

#|

buggy

(define (expand-universe-x)
  (do-list (nx nulls-x)
	   (set! coords (map (lambda (cord)
			       (let ((x (car cord))
				     (y (cadr cord)))
				 (cond
				  ((> x nx) (list (+ x 1) y))
				  (#t (list x y)))))
                                coords))))
|#

;;redefinition
(define (expand-universe-x)
  (set! coords (map (lambda (cord)
		      (let ((x (car cord))
			    (y (cadr cord))
			    (n 0))
			(do-list (nx nulls-x)				 
				 (cond
				  ((> x nx) (set! n (+ n (*expansion-rate*))))))
			(list (+ n x) y)))
		    coords)))

#|
buggy
(define (expand-universe-y)
  (do-list (ny nulls-y)
	   (set! coords (map (lambda (cord)
			       (let ((x (car cord))
				     (y (cadr cord)))
				 (cond
				  ((> y ny) (list x (+ 1 y)))
				  (#t (list x y)))))
			     coords))))
|#

(define (expand-universe-y)
  (set! coords (map (lambda (cord)
		      (let ((x (car cord))
			    (y (cadr cord))
			    (n 0))
			(do-list (ny nulls-y)				 
				 (cond
				  ((> y ny) (set! n (+ n (*expansion-rate*))))))
			(list x (+ n y))))
		    coords)))



(define (shortest-path co1 co2)
  (let ((x1 (car co1))
	(y1 (cadr co1))
	(x2 (car co2))
	(y2 (cadr co2)))
    (+ (abs (- x2 x1))
       (abs (- y2 y1)))))


(define (short-iter xs)
  (cond
   ((null? xs) 0)
   ((null? (cdr xs)) 0)
   (#t (let ((c1 (car xs))
	     (dist 0))
	 (do-list (c2 (cdr xs))
		  (set! dist (+ dist (shortest-path c1 c2))))
	 (+ dist (short-iter (cdr xs)))))))


(define (shortest-paths)
  (short-iter coords))


(define (arrange-top-bottom-left-right)
  (let ((min-x (apply min (map car coords)))
	(max-x (apply max (map car coords)))
	(min-y (apply min (map cadr coords)))
	(max-y (apply max (map cadr coords))))
    (let ((x min-x)
	  (y min-y)
	  (rs '()))
      (format #t "min-y max-y ~a ~a ~%" min-y max-y)
      (do-while (<= y max-y)
		(set! x min-x)
		(do-while (<= x max-x)
			  ;;(format #t "x,y check ~a,~a ~%" x y)
			  (let ((co (list x y)))
			    (cond
			     ((member co coords equal?)
			      (set! rs (cons co rs)))))
			  (set! x (+ x 1)))
		(set! y (+ y 1)))
      (set! coords (reverse rs))
      coords)))


(define (show-coords)
  (let ((min-x (apply min (map car coords)))
	(max-x (apply max (map car coords)))
	(min-y (apply min (map cadr coords)))
	(max-y (apply max (map cadr coords))))
    (let ((x min-x)
	  (y min-y))
      ;;(format #t "min-y max-y ~a ~a ~%" min-y max-y)
      (format #t "~%~%")
      (do-while (<= y max-y)
		(set! x min-x)
		(format #t "~%")
		(do-while (<= x max-x)
			  ;;(format #t "x,y check ~a,~a ~%" x y)
			  (let ((co (list x y)))
			    (cond
			     ((member co coords equal?)
			      (format #t "#")
			      )
			     (#t (format #t "."))))
			  (set! x (+ x 1)))
		(set! y (+ y 1)))
      (format #t "~%~%"))))

(define (between a b)
  (shortest-path (list-ref coords (- a 1))
		 (list-ref coords (- b 1))))


#|

|#

(define (part-1)
  (reset)
  (let ((larger 2))
    (*expansion-rate* (- larger 1))
    (parse input) ;; sets up coords
    ;; (format #t "directly after parse - before exapnsions~%")
    ;; (show-coords)
    (find-nulls)
    (expand-universe)
    (arrange-top-bottom-left-right)
    ;; (format #t "-----after exapnsions------~%")
    ;; (show-coords)
    (shortest-paths)  
    ))





#|

n (n + 1) / 2
where n is 9 - 1

8 x 9 / 2
4 x 9
36

first item with rest of list
next iteration cdr list

   1 2 3  4  5  6  7  8  9 
1    1 2  3  4  5  6  7  8
2      9 10 11 12 13 14 15
3        16 17 18 19 20 21
4           22 23 24 25 26
5              27 28 29 30
6                 31 32 33
7                    34 35
8                       36
9

n (n + 1) / 2
input data n is 447 - 1
446 x 447 / 2
223 x 447
(* 446 447 1/2)
99681

(* 8 9 1/2)
36

(part-1)

10165598

|#







