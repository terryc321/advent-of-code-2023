;; -----------------------------------------------------------------------------------
;; ;; find all the reachable connections
;; ;; initial start symbol s
;; (define reach
;;   (let ((seen '()))
;;     (lambda (init connections)
;;       (letrec ((foo (lambda (s)
;; 		      (dolist (v connections)
;; 			      (let ((a (car v))
;; 				    (b (cadr v)))
;; 				(cond
;; 				 ((member a seen) #f)
;; 				 (#t (set! seen (cons a seen))
;; 				     (foo a)))
;; 				(cond
;; 				 ((member b seen) #f)
;; 				 (#t (set! seen (cons b seen))
;; 				     (foo b))))))))
;; 	(foo init)
;; 	(sort seen symbol<? )))))
;; 
;; (define find-groups
;;   (let ((groups '()))
;;     (lambda (conns)
;;       (dolist (v *all-symbols*)
;; 	      (let ((group (reach v conns)))
;; 		(cond
;; 		 ((member group groups) #f)
;; 		 (#t (set! groups (cons group groups))))))
;;       groups)))
;; 
;; ;; remove 3 connections from conns
;; (define remove3
;;   (lambda (conns f)
;;     (let* ((len (length conns))
;; 	   (is (iota len))
;; 	   (i 0)(j 0)(k 0))
;;       (dolist (i is)
;; 	       (dolist (j is)
;; 		       (dolist (k is)
;; 			       (cond
;; 				((and (not (= i j))
;; 				      (not (= i k))
;; 				      (not (= j k)))
;; 				 (let ((v  (list-ref conns i))
;; 				       (v2 (list-ref conns j))
;; 				       (v3 (list-ref conns k)))
;; 				 (f (remove (lambda (x)
;; 					   (or (equal? x v)
;; 					       (equal? x v2)
;; 					       (equal? x v3)))
;; 					    conns)))))))))))
;; 
;; 
;; (define chunk
;;   (let ((c 0))
;;     (lambda ()
;;       (remove3 *all* (lambda (x)
;; 		       ;;(format #t "~a : x = ~a ~%" c x)
;; 		       (set! c (+ c 1))))
;;       (format #t "there were ~a connections with 3 removed  ~%" c))))


;;(chunk)







;; 
;; ;; (find-group (caar *all*) *all*)
;; (define find-group (lambda (s g)
;; 		     (format #t " g = ~a ~%" g )
;; 		     (apply append 
;; 		     (map (lambda (x)
;; 			    ;;(format #t "x = ~a~%" x ) 
;; 			    (let ((a (car x))
;; 				  (b (car (cdr x))))
;; 			      (format #t "x = ~a : a = ~a : b = ~a : s = ~a ~%" x a b s)
;; 			      (cond
;; 			       ((eq? a s) (list b))
;; 			       ((eq? b s) (list a))
;; 			       (#t (list)))))
;; 			  g))))
;; 
;; 
;; (define find-all
;;   (lambda (s g)
;;     (let ((reach '()))
;;       (letrec ((foo (lambda (s)
;; 		      (let ((r (find-group s g)))
;; 			(dolist (v r)
;; 				(cond
;; 				 ((member? v 
;; -----------------------------------------------------------------------------------

(import (chicken format))
(import (chicken pretty-print))
(import (chicken sort))
(import procedural-macros)

(import srfi-1)


;; (import scheme)
;; (import expand-full)
;; (import simple-exceptions)
;; (import (chicken repl))
;; (import (chicken string))
;; (import (chicken pretty-print))
;; (import (chicken io))
;; (import (chicken format))
;; (import (chicken sort))
;; (import (chicken file))
;; (import (chicken process-context))
;; ;; (change-directory "day17")
;; ;; (get-current-directory)
;; (import procedural-macros)
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
;; (define pp pretty-print)
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


;; ----------------------------------------------------------------------------





;;
;; hfx/pzl 
;; bvb/cmg
;; nvd/jqt
;;
;; In this example, if you disconnect the wire between
;; components into two separate, disconnected groups:
;; 
;;     9 components: cmg, frs, lhk, lsr, nvd, pzl, qnr, rsh, and rzs.
;;     6 components: bvb, hfx, jqt, ntq, rhn, and xhk.
;; 
;; Multiplying the sizes of these groups together produces 54.
;;
;; flatten input to give a list of symbols = list of all components
;; make a list of pairs where components connected together
;; (jqt rhn)
;; (jqt xhk)
;; (jqt nvd)
;; ...
;; 
;; if it breaks into 2 groups only
;; find all connected components from some random symbol
;; then any symbol in all symbols not in this group , must make another group
;; find all those symbols , are we done ?
;; count number symbols in each group
;; since only want 2 groups any more not of interest

;; (define *example* (with-input-from-file "example.scm"  (lambda () (read))))
;; (define *input* (with-input-from-file "input.scm"  (lambda () (read))))
;; input-changed -- removed 3 links
;; removed 1 sss-pzr
;; removed 2 njx-pbx
;; removed 3 zvk-svx
(define *input* (with-input-from-file "input-changed.scm"  (lambda () (read))))

;; choose one of these to switch between problem sets
;;(define *connections* *example*)
 (define *connections* *input*)


;; (define big-group (lambda (g) (apply append g)))
;; ;; (big-group example)
;; 
(define connect (lambda (xs)
		  (let ((a (car xs)))
		    (map (lambda (x) (list a x)) (cdr xs)))))
			
(define make-connections (lambda (g) (apply append (map connect g))))

;; *all* are list of pairs - (jqt rhn) ... (frs lsr)
(define *all* (make-connections *connections*))
;; all the connections 
;; *all*
;; ((jqt rhn) (jqt xhk) (jqt nvd) (rsh frs) (rsh pzl) (rsh lsr) (xhk hfx) (cmg qnr) (cmg nvd)
;;  (cmg lhk) (cmg bvb) (rhn xhk) (rhn bvb) (rhn hfx) (bvb xhk) (bvb hfx) (pzl lsr) (pzl hfx)
;;  (pzl nvd) (qnr nvd) (ntq jqt) (ntq hfx) (ntq bvb) (ntq xhk) (nvd lhk) (lsr lhk) (rzs qnr)
;;  (rzs cmg) (rzs lsr) (rzs rsh) (frs qnr) (frs lhk) (frs lsr))

(define symbol<? (lambda (x y)
		   (string<=? (symbol->string x)
			      (symbol->string y))))


(define remove-duplicates
  (lambda (xs)
    (define remove-duplicates2
      (lambda (xs ys)
	(cond
	 ((null? xs) ys)
	 (#t (let ((x (car xs)))
	       (cond
		((member x ys) (remove-duplicates2 (cdr xs) ys))
		(#t (remove-duplicates2 (cdr xs) (cons x ys)))))))))
    (remove-duplicates2 xs '())))


(define *all-symbols* (sort (remove-duplicates (apply append *all*))
			    symbol<?))

#|
efficiency ?
((jqt rhn) ...
 (jqt xhk) ...
 (jqt nvd) ...

translate each symbol jqt to integer
*all-symbols*
(bvb cmg frs hfx jqt lhk lsr ntq nvd pzl qnr rhn rsh rzs xhk)


|#
(define *int-symbols*
  (let ((c 0))
    (map (lambda (x) (let ((r c)) (set! c (+ c 1)) (list x r)))
	 *all-symbols*)))

(define *vec-int-conns*
  (list->vector
    (map (lambda (pr)
	   (let ((a (car pr))
		 (b (cadr pr)))
	     (list->vector
	      (list (cadr (assoc a *int-symbols*))
		    (cadr (assoc b *int-symbols*))))))
	 *all*)))

(define *syms-len* (length *all-symbols*))

(define blank-vec
  (lambda () (make-vector *syms-len* #f)))

(define reachable
  (lambda ()
    (let ((vec (blank-vec))
	  (comms-last (- (vector-length *vec-int-conns*) 1))
	  (count 0)
	  (tot *syms-len*))
      (letrec ((foo (lambda (n)
		      (let ((val (vector-ref vec n)))
			(cond
			 ((not val) ;; unset
			  (vector-set! vec n #t)
			  (set! count (+ count 1))
			  ;; find all reachable from here
			  (for (i 0 comms-last 1)
			       (let* ((vpair (vector-ref *vec-int-conns* i))
				      (va (vector-ref vpair 0))
				      (vb (vector-ref vpair 1)))
				 (cond
				  ((= n va) (foo vb))
				  ((= n vb) (foo va)))))))))))
	(foo 0)
	(let* ((not-reached (- tot count))
	       (reached count)
	       (product (* not-reached reached)))
	  (format #t "reached ~a : not reached ~a : total ~a : product ~a ~%"
		  reached
		  not-reached
		  tot
		  product)
	  product)))))

;; reachable

#|

#;1085> ,t (reachable)
reached 782 : not reached 745 : total 1527 : product 582590 
0.877s CPU time, 0.012s GC time (major), 375230/61763 mutations (total/tracked), 12/24591 GCs (major/minor), maximum live heap: 1.81 MiB
582590

582590   ........... ACCEPTED 


|#



;; a 0 to len-1
;; b a+1 to len-1
;; c b + 1 to len-1
;; ---- but b and c still need to fit on end 

;; a 0 to len-1-2
;; b a+1 to len-1-1
;; c b + 1 to len-1
;; ------------ tidy up


;; a : 0 to len-3
;; b : a+1 to len-2
;; ;; c : b + 1 to len-1
;; (define three
;;   (lambda (f)
;;     (let ((last #f))
;;       (letrec ((fa (lambda (from to)
;; 		     (cond
;; 		      ((> from to) #f)
;; 		      (#t (let ((a from))
;; 			    (fb (+ from 1) to a)
;; 			    (fa (+ from 1) to))))))
;; 	       (fb (lambda (from to a)
;; 		     (cond
;; 		      ((> from to) #f)
;; 		      (#t (let ((b from))
;; 			    (fc (+ from 1) to a b)
;; 			    (fb (+ from 1) to a))))))
;; 	       (fc (lambda (from to a b)
;; 		     (cond
;; 		      ((> from to) #f)
;; 		      (#t (let ((c from))
;; 			    (set! last (f a b c))
;; 			    (fc (+ from 1) to a b)))))))
;; 	(fa 0 (- *syms-len* 1))
;; 	last))))
;; 
;; 
;; 
;; (define f3
;;   (let ((n 0))
;;     (lambda (a b c)
;;       ;;(format #t "~a : a = ~a : b = ~a : c = ~a ~%" n a b c)
;;       (set! n (+ n 1))
;;       n)))


;;(format #t "there were ~a combinations ~%" (three f3))
#|
csc fun.scm
time ./fun

there were 592,259,675 combinations thats 592 million + combinations 
          ^^^^^^^^^^^^
real	0m10.580s
user	0m10.524s
sys	0m0.056s

|#



;; ;; 3600 seconds in an hour
;; (define (hour)
;;   (let ((sec 60)
;; 	(min 60))
;;     (* sec min)))
;; 
;; (define (estimate seconds)
;;   (let ((each seconds)
;; 	(array 592259675))
;;     (+ 0.0
;;      (/ (* each array)
;; 	(hour)))))

#|
(estimate 1)
164516.576388889

if find all reachable nodes from removing 3 links ,
given that task took 1 second
overall take approx 164516 hours of computation time to figure out

|#

(define make-dot
  (lambda ()
    (with-output-to-file "input.dot"
      (lambda ()
	(let ((pairs *all*))
	  (format #t "graph {~%")
	  (letrec ((foo
		    (lambda (xs)
		      (cond
		       ((null? xs) #f)
		       (#t (let ((pr (car xs)))
			     (let ((a (car pr))
				   (b (cadr pr)))
			       (format #t "~A -- ~A~%" a b)
			       (foo (cdr xs)))))))))
	    (foo  pairs))
	  (format #t "~%}~%"))))))



#|
shell commands
dot -Tsvg example.dot > example.svg
dot -Tsvg input.dot > input.svg


using firefox look at input.svg we see 3 lines cross two groups

sss-pzr
njx-pbx
zvk-sxx

|#




