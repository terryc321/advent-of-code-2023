
(import procedural-macros)

(import (chicken format))
(import (chicken pretty-print))

;; (import scheme)
;; (import expand-full)
;; (import simple-exceptions)
;; (import (chicken repl))
;; (import (chicken string))
;; (import (chicken io))
;; (import (chicken sort))
;; (import (chicken file))
;; (import (chicken process-context))
;; ;; (change-directory "day17")
;; ;; (get-current-directory)
;; (import regex)
;; (import simple-md5)
;; (import simple-loops)
;; ;; hash-table-ref  hash key thunk
;; ;; hash-table-set! hash key val
;; ;; sudo chicken-install srfi-178
;; (import srfi-178)
;; ;; srfi-178 provides bit-vectors
;; ;; (import-for-syntax
;; ;;   (only checks <<)
;; ;;   (only bindings bind bind-case)
;; ;;   (only procedural-macros macro-rules with-renamed-symbols once-only))
;; (import sequences)
;; (import srfi-1)
;; (import matchable)
;; ;;(define pp pretty-print)
;; (import srfi-69) ;; hash tables

;; ------------ macros ---------------------------------------
;; dolist
(define-macro (dolist varlist . body)
  (let ((var (car varlist))
	(ls (cadr varlist))
	(fn (gensym "fn")))	
    `(begin
       (letrec
	   ((,fn (lambda (xs)
		   (cond
		    ((null? xs) #f)
		    (#t (let ((,var (car xs)))
			  ,@body
			  (,fn (cdr xs))))))))
	 (,fn ,ls)))))

;; dofor
;; cannot handle decreasing steps ?
(define-macro (for v . body)
  (let ((var (car v))
	(init (cadr v))
	(lim (caddr v))
	(step (cadddr v))	      
	(foo (gensym "foo"))
	(v-i (gensym "i"))
	(v-step (gensym "step"))
	(v-lim (gensym "lim")))
    `(begin
       (letrec ;; want to capture var
	   ((,foo (lambda (,var ,v-step ,v-lim)
		    (cond
		     ((> ,var ,v-lim) #f)
		     (#t
		      ,@body
		      (,foo (+ ,var ,v-step) ,v-step ,v-lim))))))
	 (,foo ,init ,step ,lim)))))

;;(pp (expand* '(for (i 1 10 1) (format #t "i = ~A ~%" i))))
;; (for (i 1 10 1) (format #t "i = ~A ~%" i))
;; (for (i 10 1 -1) (format #t "i = ~A ~%" i))

;;this is test.scm
;; (+ 1 2 )

;; ---------------------------------------------------------------
;; make a grid from text file
;; how define a grid abstraction - without falling into object closures
;;

(define make-grid
  (lambda (v w h)
    v))

(define grid?
  (lambda (x)
    #t))

(define grid-copy
  (lambda (g)
    (let* ((width (grid-width g))
	   (height (grid-height g))
	   (vec (make-vector height)))     
      (for (y 0 (- height 1) 1)
	   (let ((horz (make-vector width)))
	     (vector-set! vec y horz)
	     (for (x 0 (- width 1) 1)	      
		  (let ((val (grid-get g x y)))
		    (vector-set! horz x val)))))
      (make-grid vec width height))))


;; copy stones X literally
;; replaces all other squares with _ empties
(define grid-copy-stones
  (lambda (g)
    (let* ((width (grid-width g))
	   (height (grid-height g))
	   (vec (make-vector height)))     
      (for (y 0 (- height 1) 1)
	   (let ((horz (make-vector width)))
	     (vector-set! vec y horz)
	     (for (x 0 (- width 1) 1)	      
		  (let ((val (grid-get g x y)))
		    (cond
		     ((eq? val 'X)
		      (vector-set! horz x val))
		     (#t
		      (vector-set! horz x '_)))))))		     
      (make-grid vec width height))))



(define grid-width
  (lambda (g)
    (vector-length (vector-ref g 0))))

(define grid-height
  (lambda (g)
    (vector-length g)))

(define grid-get
  (lambda (g x y)
    (cond
     ((grid-valid? g x y)     
      (vector-ref (vector-ref g y) x))
     (#t (error "grid-get")))))

(define grid-set!
  (lambda (g x y z)
    (cond
     ((grid-valid? g x y)
      (vector-set! (vector-ref g y) x z))
     (#t (error "grid set!")))))

(define grid-valid?
  (lambda (g x y)
    (and (>= x 0) (< x (grid-width g))
	 (>= y 0) (< y (grid-height g)))))

(define grid-show
  (lambda (g)
    (let ((width (grid-width g))
	  (height (grid-width g)))
    (format #t "~%")
    (for (y 0 (- height 1) 1)
	 (format #t "~%")
	 (for (x 0 (- width 1) 1)
	      (let ((sq (grid-get g x y)))
		(format #t "~a " sq))))
    (format #t "~%"))))


(define grid-replace-start!
  (lambda (g)
    (call/cc (lambda (found) 
	       (let ((width (grid-width g))
		     (height (grid-width g)))
		 (for (y 0 (- height 1) 1)
		      (for (x 0 (- width 1) 1)
			   (let ((sq (grid-get g x y)))
			     (cond
			      ((eq? sq 'S) (format #t "found START at ~a ~a ~%" x y)
			       (grid-set! g x y 0)))))))))))


(define grid-on-edge?
  (lambda (g x y)
    (let ((width (grid-width g))
	  (height (grid-width g)))    
      (or (= x 0)
	  (= x (- width 1))
	  (= y 0)
	  (= y (- height 1))))))

(define grid-on?
  (lambda (g x y)
    (let ((width (grid-width g))
	  (height (grid-width g)))    
      (and (>= x 0)
	   (<= x (- width 1))
	   (>= y 0)
	   (<= y (- height 1))))))


;; when we check if square is a garden could be
;; X occupied by stone -> not garden
;; INT already written to -> garden
;; _ empty -> garden
;;
;; we are allowed to walk the perimeter also , i thought perimeter not allowed ...
(define garden?
  (lambda (g x y)
    (and ;;(not (grid-on-edge? g x y))
	 (grid-on? g x y)
	 (let* ((val (grid-get g x y)))	
	   (cond
	    ((eq? val 'X) #f)
	    (#t #t))))))


(define north
  (lambda (g x y i)
    (let ((x2 x)
	  (y2 (- y 1)))
      (cond
       ((garden? g x2 y2)
	(grid-set! g x2 y2 i))))))

(define south
  (lambda (g x y i)
    (let ((x2 x)
	  (y2 (+ y 1)))
      (cond
       ((garden? g x2 y2)
	(grid-set! g x2 y2 i))))))

(define east
  (lambda (g x y i)
    (let ((x2 (+ x 1))
	  (y2 y))
      (cond
       ((garden? g x2 y2)
	(grid-set! g x2 y2 i))))))

(define west
  (lambda (g x y i)
    (let ((x2 (- x 1))
	  (y2 y))
      (cond
       ((garden? g x2 y2)
	(grid-set! g x2 y2 i))))))




(define grid-reach
  (lambda (g i)
    (let ((width (grid-width g))
	  (height (grid-width g))
	  (copy (grid-copy-stones g)))
      (for (y 0 (- height 1) 1)
	   (for (x 0 (- width 1) 1)
		(let* ((a (grid-get g x y)))
		  (cond
		   ((and (integer? a) (= a i))
		    (north copy x y (+ i 1))
		    (south copy x y (+ i 1))
		    (west copy x y (+ i 1))
		    (east copy x y (+ i 1)))))))
      copy)))

;; if grid has an integer means we reached that square
;; simply add up number squares with ints
;; rather than symbol 'X stone occupied
;; rather than symbol '_ empty square
(define grid-count
  (lambda (g)
    (let ((width (grid-width g))
	  (height (grid-width g))
	  (n 0))	  
      (for (y 0 (- height 1) 1)
	   (for (x 0 (- width 1) 1)
		(let* ((a (grid-get g x y)))
		  (cond
		   ((integer? a)
		    (set! n (+  n 1)))))))
      n)))


(define run
  (let ((n 0)
	(result 0))
    (lambda (g lim)
      (letrec ((foo (lambda ()
		      (cond
		       ((>= n lim)
			(set! result (grid-count g))
			'done)
		       (#t
			(set! g (grid-reach g n))
			(set! n (+ n 1))
			;;(grid-show g)
			(foo)
			)))))
	(foo)
	(grid-show g)
	(format #t "there are ~a gardens reached~%" result)
	result))))


      
;; ------- inputs 

(define example (with-input-from-file "example" (lambda () (read))))
(define input (with-input-from-file "input" (lambda ()  (read))))

(define puzzle (grid-copy example))
(define puzzle2 (grid-copy puzzle))

(grid-set! puzzle 0 0 "00")
(grid-set! puzzle 1 1 "11")
(grid-set! puzzle 2 2 "22")

(grid-show puzzle)
(grid-show puzzle2)
(set! puzzle (grid-copy example))
(grid-replace-start! puzzle)

(grid-replace-start! input)

;;(run puzzle 6)
(run input 64)





















