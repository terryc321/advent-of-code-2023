
#|

pipe connection puzzle

S is pipe but do not know where it goes

| is a vertical pipe connecting north and south.

- is a horizontal pipe connecting east and west.

L is a 90-degree bend connecting north and east.

J is a 90-degree bend connecting north and west.

7 is a 90-degree bend connecting south and west.

F is a 90-degree bend connecting south and east.

. is ground; there is no pipe in this tile.

S is the starting position of the animal; there is a pipe on this tile, but your sketch doesn't show what shape the pipe has.


1 2 3
4 S 6
7 8 9

look at 6 to right of S and no pipe connecting west - reached dead end


1 2 3
4 S 6
7 8 9

  2
4 S 6
  8

1,3,7,9 do not look at no diagonal pipe connections


S is at (24 76)

(arr-xy 24 75)  J
(arr-xy 23 76)  F
(arr-xy 24 77)  L
(arr-xy 25 76)  L

          J
        F S L
          L


|#



(import scheme)

(import simple-exceptions)

(import (chicken string))
(import (chicken pretty-print))
(define pp pretty-print)

(import (chicken format))
(import (chicken sort))
(import (chicken file))
(import (chicken process-context))
;; (change-directory "day10")
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

(define example #f)

;;(set! input example)

(define (1+ x) (+ x 1))
(define (1- x) (- x 1))

(define (10+ x) (+ x 10))
(define (10- x) (- x 10))

(define rest cdr)

;;--------------------------------------------------------------------


(define (parse dat)
  (let* ((in dat)
	 (lines (string-split in "\n"))
	 (ylen (length lines))
	 (xlen (string-length (car lines)))
	 (x 0)
	 (y 0)
	 (vec (make-vector ylen 0)))
    (do-list (str lines)
	     (format #t "~%")
	     (assert (= xlen (string-length str)))
	     (let ((tvec (make-vector xlen #\.)))
	     (do-for n (0 xlen 1)
		     (set! x n)
		     (vector-set! tvec n (string-ref str n))
		     (format #t "~a" (string-ref str n)))
	     (vector-set! vec y tvec)
	     (set! y (+ y 1))))
    (format #t "~%~%")
    vec))

(define arr #f)
(set! arr (parse input))

(define (arr-xy x y)
  (vector-ref (vector-ref arr y) x))

(define (arr-width)
  (vector-length (vector-ref arr 0)))

(define (arr-height)
  (vector-length arr))

(define (find-s)
  (call/cc (lambda (escape)
	     (do-for x (0 (arr-width) 1)
		     (do-for y (0 (arr-height) 1)
			     (let ((ch (arr-xy x y)))
			       (cond
				((char=? ch #\S)
				 (escape (list x y))))))))))

(define (on-board x y)
  (and (>= x 0) (< x (arr-width))
       (>= y 0) (< y (arr-height))))


#|
north - Y
south + Y

west - X
east + X

| : north - south
- : west - east
7 : south + west
J : north + west
F : south + east
L : north + east



|#


(define (reach x y dir path)
  (when (on-board x y)
    ;;(format #t "reach ; at ~a , ~a ~%" x y)
    (let ((ch (arr-xy x y)))
      (cond
       ((eq? dir 'east)
	;;(format #t "east is east.~%")
	(cond
	 ((char=? ch #\-) (reach (+ x 1) y 'east (cons (list x y) path)))
	 ((char=? ch #\J) (reach x (- y 1) 'north (cons (list x y) path)))
	 ((char=? ch #\7) (reach x (+ y 1) 'south (cons (list x y) path)))
	 ((char=? ch #\S)
	  ;;(format #t "~%stopped at S~%~a~%" path)
	  path)
	 (#t '())
	 ))       
       ((eq? dir 'west) ;; 
	(cond
	 ((char=? ch #\-) (reach (- x 1) y 'west (cons (list x y) path)))
	 ((char=? ch #\L) (reach x (- y 1) 'north (cons (list x y) path)))
	 ((char=? ch #\F) (reach x (+ y 1) 'south (cons (list x y) path)))
	 ((char=? ch #\S)
	  ;;(format #t "~%stopped at S~%~a~%" path)
	  path)
	 (#t '())
	 ))       
       ((eq? dir 'south) ;;
	(cond 
	 ((char=? ch #\|) (reach x (+ y 1) 'south (cons (list x y) path)))
	 ((char=? ch #\J) (reach (- x 1) y 'west (cons (list x y) path)))
	 ((char=? ch #\L) (reach (+ x 1) y 'east (cons (list x y) path)))
	 ((char=? ch #\S)
	  ;;(format #t "~%stopped at S~%~a~%" path)
	  path)
	 (#t '())
	 ))       
       ((eq? dir 'north) ;;
	(cond
	 ((char=? ch #\|) (reach x (- y 1) 'north (cons (list x y) path)))
	 ((char=? ch #\F) (reach (+ x 1) y 'east (cons (list x y) path)))
	 ((char=? ch #\7) (reach (- x 1) y 'west (cons (list x y) path)))
	 ((char=? ch #\S)
	  ;;(format #t "~%stopped at S~%~a~%" path)
	  path)
	 (#t '())
	 ))       
       (#t (error "reach dir" (list dir 'unknown 'direction)))))))



(define (part-1)
  (let ((s-loc (find-s)))
    (let ((x (car s-loc))
	  (y (cadr s-loc)))
      (list
       (reach (+ x 1) y 'east '())
       (reach (- x 1) y 'west '()) ;; ***
       (reach x (- y 1) 'north '())
       (reach x (+ y 1) 'south '()) ;;****
       ))))



(define (hack-step step xs ys)
  (cond
   ((null? xs) #f)
   ((null? ys) #f)
   ((equal? (car xs) (car ys)) step)
   (#t (hack-step (+ step 1) (cdr xs) (cdr ys)))))

(define (hack)
  (let* ((b (part-1))
	 (p1 (second b))
	 (p2 (fourth b)))
    (hack-step 1 p1 p2)))



(define (define-matrix)
  (format #t "~%~%")
  (do-list (x (iota 140))
	   (do-list (y (iota 140))
		    (format #t "matrix[~a][~a]='~a';~%" x y (arr-xy x y))
		    )))

(define (define-matrix2)
  (format #t "~%~%")
  (do-list (x (iota 140))
	   (do-list (y (iota 140))
		    (format #t "matrix2[~a][~a]=~a;~%" x y 0)
		    ))
  ;; start at 24 76
  (format #t "matrix2[~a][~a]=~a;~%" 24 76 1)
  (let ((n 2 ))
    (do-list (c (second (part-1)))
	     (let ((x (car c))
		   (y (cadr c)))
	       (format #t "matrix2[~a][~a]=~a;~%" x y n)
	       (set! n (+ n 1))))))



	     







#|
#;2258> (hack)
6842

6842 steps to reach furthest point ?

suggests S is a 7 piece of pipe connecting west and south


|#


    



#|
buggy
did not terminate
simply repeated same position hundred or so times ...

(define (cycle x y path)
  (when (and (on-board x y) (not (member (list x y) path equal?)))
    (format #t "path ~a ~%" path)
    
    (let ((x2 (+ x 1)) ;; east or move right ?
	  (y2 y))
      (when (on-board x2 y2)
	(let ((ch (arr-xy x2 y2)))
	  (cond
	   ((char=? ch #\-) (cycle (+ x2 1) y2 (cons (list x2 y2) path))) ;; go east
	   ((char=? ch #\J) (cycle x2 (- y2 1) (cons (list x2 y2) path))) ;; go north
	   ((char=? ch #\7) (cycle x2 (+ y2 1) (cons (list x2 y2) path))) ;; go south
	   ))))
    (let ((x2 (- x 1)) ;; west or move left ?
	  (y2 y))
      (when (on-board x2 y2)
	(let ((ch (arr-xy x2 y2)))
	  (cond
	   ((char=? ch #\-) (cycle (- x2 1) y2 (cons (list x2 y2) path))) ;; go west
	   ((char=? ch #\L) (cycle x2 (- y2 1) (cons (list x2 y2) path))) ;; go north
	   ((char=? ch #\F) (cycle x2 (+ y2 1) (cons (list x2 y2) path))) ;; go south
	   ))))
    (let ((x2 x) ;; north or move up ?
	  (y2 (- y 1)))
      (when (on-board x2 y2)
	(let ((ch (arr-xy x2 y2)))
	  (cond
	   ((char=? ch #\|) (cycle x2 (- y2 1) (cons (list x2 y2) path))) ;; go north
	   ((char=? ch #\7) (cycle (- x2 1) y2 (cons (list x2 y2) path))) ;; go west
	   ((char=? ch #\F) (cycle (+ 1 x2) y2 (cons (list x2 y2) path))) ;; go east
	   )))) 
    (let ((x2 x) ;; south or move down ?
	  (y2 (+ y 1)))
      (when (on-board x2 y2)
	(let ((ch (arr-xy x2 y2)))
	  (cond
	   ((char=? ch #\|) (cycle x2 (+ y2 1) (cons (list x2 y2) path))) ;; go south
	   ((char=? ch #\L) (cycle (+ x2 1) y2 (cons (list x2 y2) path))) ;; go east
	   ((char=? ch #\J) (cycle (- x2 1) y2 (cons (list x2 y2) path))) ;; go west
	   ))))
    ))

|#




  
       
       
       




		     
	     
	    


