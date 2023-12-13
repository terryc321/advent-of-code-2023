
;; try put tour3.scm guile code inside a c application SDL graphics


;;#lang racket
;; some scheme systems define while loop like guile
;; if we want our own what else can we do besides rename it while2
;; without knowing a priori what scheme language will be running the code

;(define (guile)
;; (use-modules (ice-9 pretty-print))
;; (use-modules (ice-9 format))
;; format - print string - not portable code

(define *debug* #f)

(define-syntax inc
  (syntax-rules ()
    ((inc n) (set! n (+ n 1)))))

(define-syntax dec
  (syntax-rules ()
    ((dec n) (set! n (- n 1)))))

(define-syntax while2
  (syntax-rules ()
    ((while2 con body ...)
     (letrec ((lup (lambda()
		     (if con
			 (begin
			   body
			   ...
			   (lup))
			 #f))))
       (lup)))))

;; macro vary x from 1 to 8 , y from 1 to 8
;; pass those values to procedure proc
(define-syntax bar
  (syntax-rules (x y)
    ((bar proc)
     (let ((x 1)
	   (y 1))
       (while2 (<= y 8)
	 (set! x 1)
	 (while2 (<= x 8)
	   (proc x y)
	   (inc x))
	 (inc y))
       #t))))

;; was called linear
;; x y to single value
(define (xy->n x y)
  (+ x (* 8 (- y 1))))

;; initialise when called first time
;; after that - simply a vector lookup
(define n->xy
  (let ((outer-vec #f))
    (let ((compute
	   (lambda ()
	     ;;(display "computing vector ...")
	     (let ((vec (make-vector 70 #f)))
	       (bar (lambda (x y)
		      (let ((n (xy->n x y)))
			(vector-set! vec n (list x y)))))
	       (set! outer-vec vec)
	       outer-vec))))
      (lambda (n)
	(if outer-vec
	    (vector-ref outer-vec n)
	    (begin
	      (compute)
	      (vector-ref outer-vec n)))))))

;; -----------------------------------------------------------------------------

(define (make-vec-board)
  (vector (vector 0 0 0 0 0 0 0 0 0 0 0)
	  (vector 0 0 0 0 0 0 0 0 0 0 0)
	  (vector 0 0 0 0 0 0 0 0 0 0 0)
	  (vector 0 0 0 0 0 0 0 0 0 0 0)
	  (vector 0 0 0 0 0 0 0 0 0 0 0)
	  (vector 0 0 0 0 0 0 0 0 0 0 0)
	  (vector 0 0 0 0 0 0 0 0 0 0 0)
	  (vector 0 0 0 0 0 0 0 0 0 0 0)
	  (vector 0 0 0 0 0 0 0 0 0 0 0)
	  (vector 0 0 0 0 0 0 0 0 0 0 0)
	  (vector 0 0 0 0 0 0 0 0 0 0 0)
	  ))

(define (get-xy board x y)
  (let ((yboard (vector-ref board y)))
    (vector-ref yboard x)))

(define (set-xy! board x y q)
  (let ((yboard (vector-ref board y)))
    (vector-set! yboard x q)
    board))

(define (clear-board board)
  (bar (lambda (x y)
	 (set-xy! board x y 0)))
  board)


(define (make-board)
  (let ((board (make-vec-board)))
    (clear-board board)
    board))

(define (show-board board)
  (let ((x 0)
	(y 9))
    (while2 (>= y 0)
      (set! x 0)
      (newline)
      (while2 (< x 10)
	(let ((piece (get-xy board x y)))
	  (if piece
	      (begin (display " ")(display piece)(display " "))
	      (display " . ")
	      ))
	(inc x))
      (dec y))
    (newline)
    board))


(define (on-board x y)
  (and (>= x 1) (<= x 8)
       (>= y 1) (<= y 8)))


(define (copy-board board)
  (let ((new-board (make-board)))
    (bar (lambda (x y)
	   (set-xy! new-board x y (get-xy board x y))))
    new-board))

;; -----------------------------------------------------------------------------
;; generate all moves possible from a certain square and store those in a vector
;; since knight can only go maximum of 8 squares
;; vector >= 64 long 
;; vector >= 8  wide


;; array has to be 70 long by 10 wide
(define (make-70x10)
  (let ((array (make-vector 70 #f))
	(i 0))
    (while2 (< i 70)
	    (vector-set! array i (make-vector 10 0))
	    (inc i))
    array))


(define-syntax foo
  (syntax-rules ()
    ((foo a b x y board n array)
     (begin
       (let ((p (+ x a))
	     (q (+ y b)))
	 (if (not (on-board p q))
	     #f
	     (let* ((from (xy->n x y))
		    (to (xy->n p q)))
	       (set-xy! board p q n)
	       ;; (display n) (display "::")	     
	       ;; (display x) (display ",") (display y) (display "::") (display from)
	       ;; (display "::")
	       ;; (display p) (display ",") (display q) (display "::") (display to)
	       ;; (newline)
	       (vector-set! (vector-ref array from) n to)
	       (set! n (+ n 1)))))))))


;; same trick
;; first time called compute
;; after that just return it
(define choices
  (let ((array #f))    
    (lambda ()
      (letrec ((init (lambda () (set! array (make-70x10))))
	       (n 0))	       
	(if array ;; init array 
	    array
	    (begin
	      (init)
	      (bar (lambda (x y)
		     (let ((board (make-board)))
		       (set-xy! board x y '@)
		       (set! n 1)
		       (foo 1 2 x y board n array)
		       (foo 1 -2 x y board n array)
		       (foo -1 2 x y board n array)
		       (foo -1 -2 x y board n array)
		       (foo 2 1 x y board n array)
		       (foo 2 -1 x y board n array)
		       (foo -2 1 x y board n array)
		       (foo -2 -1 x y board n array)
		       ;; (display "record : ")
		       ;; (display (vector-ref array (xy->n x y)))
		       ;; (show-board board)
		       ;; (newline)
		       ;; (newline)
		       )))
	      array))))))


(choices)

;; ------------------------------------------------------------------------------
;; knights tour
;;
;; is the square occupied already ?
;; ------------------------------------------------------------------------------

;; any empty square should be zero
(define (occupied? board x y)
  (cond
   ((not (on-board x y))
    #t)
   (#t 
    ;;(display "occupied? ")
    ;;(show-board board)
    ;;(display "x , y = ") (display x)(display ",")(display y)(display " :=> ")
    (let ((p (get-xy board x y)))
      (if (and (integer? p)(zero? p))
	  (begin ;(display "not occupied . ")
	    #f)
	  (begin ;(display "yes - occupied. ")
	    #t))))))




(define-syntax when2
  (syntax-rules ()
    ((when2 con bod ...)
     (if (not con) #f (begin bod ...)))))


(define tour
  (let ((longest-path '())
	(longest-step 0)
	(longest-board #f))
    (lambda (board x y step path port)

      (when2 (> step longest-step)
	     (set! longest-path path)
	     (set! longest-step step)
	     (set! longest-board board)
	     
	     (display "longest - so - far : ") (display step) (newline)
	     (display "longest - path : ") (display path) (newline)	     
	     (show-board board)
	     (newline)
	     )

      
  ;; (display "tour : ")
  ;; (show-board board)
  ;; (display "x , y = ") (display x)(display ",")(display y)(display " :=> ")(display step)
  (when2 (= step 64)
	 (newline)
	 (display "********** TOUR &&&&&&&&&&&&&")
	 (newline)
	 (show-board board)
	 (newline))
  
  (cond
   ((occupied? board x y)
    ;;(display "!")
    #f)
   (#t
    ;(display "tour : ")
    ;(show-board board)
    
    (let ((new-board (copy-board board)))
       ;;make it occupied
      (set-xy! new-board x y step)

      ;; generate output to file provided by run
      (display step port)
      (display " " port)
      (display x port)
      (display " " port)
      (display y port)
      (display "\n" port)
      
      ;; (newline)
      ;; (display "step : ")(display step)(display " at (")
      ;; (display x)(display " ") (display y)
      ;; (display ")")
      ;; (newline)
      ;; (pretty-print path)
      ;; (newline)
      ;; (show-board board)
      
      ;; for each position reachable from this position    
      (let ((options (vector-ref (choices) (xy->n x y))))
	(let ((i 1))
	  (while2 (< i 9)
	    ;; {0 p1 p2 p3 ... p8 0}
	    ;; maximum of 8 choices - encoded as single number
	    ;; transfer back to x y from n
		  (let ((pick (vector-ref options i)))
		    ;;(display "pick =") (display pick) (newline)
		    (when2 (and (integer? pick) (and (> pick 0)(< pick 65)))
			   (let* ((xy (n->xy pick))
				  (new-x (car xy))
				  (new-y (car (cdr xy))))
			     (tour new-board
				   new-x
				   new-y
				   (+ step 1)
				   (cons (list new-x new-y) path)
				   port)
			     ))
		    (inc i)))))))))))


;; start position 1
;; this is the first step = 10 , just make all steps 2 digits wide.
  ;; start with empty board 
(define (run filename)
  (let* ((step 1)
	 (path '())
	 (x 1)
	 (y 1)
	 (board (make-board)))
    (call-with-output-file filename
      (lambda (port)
	(display "sanity checking...line 336 tour3.scm tue 24th aug 14:13 ")
	(newline)
	(tour board x y step (cons '(1 1) path) port)))))




;; step computation
;; so we can move knight on screen
;; then proceed
;; ;;

;; (define (hefty-computation do-other-stuff) 
;;   (let loop ((n 5)) 
;;     (display "Hefty computation: ") 
;;     (display n) 
;;     (newline) 
;;     (set! do-other-stuff (call/cc do-other-stuff)) 
;;     (display "Hefty computation (b)")  
;;     (newline) 
;;     (set! do-other-stuff (call/cc do-other-stuff)) 
;;     (display "Hefty computation (c)") 
;;     (newline) 
;;     (set! do-other-stuff (call/cc do-other-stuff)) 
;;     (if (> n 0) 
;;         (loop (- n 1)))))

;; ;; notionally displays a clock 
;; (define (superfluous-computation do-other-stuff) 
;;   (let loop () 
;;     (for-each (lambda (graphic) 
;;                 (display graphic) 
;;                 (newline) 
;;                 (set! do-other-stuff (call/cc do-other-stuff))) 
;;               '("Straight up." "Quarter after." "Half past."  "Quarter til.")) 
;;     (loop))) 

;; (hefty-computation superfluous-computation)



;; ;; no idea whats going on here ...
;; (define (f1 f2 f3)
;;   (while2 #t
;; 	  (format #t "inside f1~%")
;; 	  (set! f2 (call/cc f2))
;; 	  (set! f1 (call/cc f1))
;; 	  (set! f3 (call/cc f3))
;; 	  ))

;; (define (f2 f1 f3)
;;   (while2 #t
;; 	  (format #t "inside f2~%")
;; 	  (set! f1 (call/cc f1))
;; 	  (set! f3 (call/cc f3))	  
;; 	  ))

;; (define (f3 f1 f2)
;;   (while2 #t
;; 	  (format #t "inside f3~%")
;; 	  (set! f2 (call/cc f2))
;; 	  (set! f1 (call/cc f1))
;; 	  ))

;; (f1 f2 f3)


