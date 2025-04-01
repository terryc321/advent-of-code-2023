
#|


call a day on this as puzzle is so badly explained




|#

(import scheme)
(import expand-full)
(import simple-exceptions)
(import (chicken repl))
(import (chicken string))
(import (chicken pretty-print))
(import (chicken io))
(import (chicken format))
(import (chicken sort))
(import (chicken file))
(import (chicken process-context))
;; (change-directory "day17")
;; (get-current-directory)
(import procedural-macros)
(import regex)
(import simple-md5)
(import simple-loops)
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
(define pp pretty-print)
(import srfi-69) ;; hash tables

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

;; ------------ macros ---------------------------------------


#|


|#

(define example-1
  '(    
( broadcaster  a  b  c)
( flipflop a  b) ;; fa
( flipflop b  c) ;; fb
( flipflop c  inv) ;; fc
( conjunction inv  a) ;; inv is conjunction
    ))




;; broadcaster sends low pulse
;; how does a flip flop work ?
;; after pressing button 1000 times, 8000 low pulses and 4000 high pulses are sent. 
(define (run-example-1)
  (let ((a 0)
	(b 0)
	(c 0)
	(inv 0)
	(low-pulse 0)
	(high-pulse 0)
	(tasks '()))
    (letrec ((hold (lambda (f) (set! tasks (cons f tasks))))
	     (lo (lambda () (set! low-pulse (+ 1 low-pulse)) 0))
             (hi (lambda () (set! high-pulse (+ 1 high-pulse)) 1))
             (sig (lambda (o)
		  (cond
		   ((= o 1) (hi))
		   (#t (lo)))))
             (broadcaster       (lambda ()
				  (fa (lo))
				  (fb (lo))
				  (fc (lo))))
             (fa                (lambda (in) (when (= in 0) (set! a (- 1 a))
						   (hold (lambda () (fb (sig a)))))))
             (fb (lambda (in) (when (= in 0) (set! b (- 1 b))
				    (hold (lambda () (fc (sig b)))))))
             (fc (lambda (in) (when (= in 0) (set! c (- 1 c))
				    (hold (lambda () (finv (sig c)))))))
             (finv (lambda (in) (cond
				 ((= inv in 1) (set! inv 1)
				  (hold (lambda () (fa (sig 0)))))
				 (#t (set! inv in)
				   (hold (lambda () (fa (sig 1))))))))
	     (do-tasks (lambda ()
			 (cond
			  ((null? tasks) #t)
			  (#t (let ((t (car tasks)))
				(set! tasks (cdr tasks))
				(t)
				(do-tasks))))))
	     (foo (lambda (n)
		    (broadcaster)
		    ;;(do-tasks)
		    (format #t "~a : [a ~a] [b ~a] [c ~a] [inv ~a] : low ~a : high ~a ~%" n a b c inv low-pulse high-pulse)
		    (cond
		     ((< n 1000) (foo (+ n 1)))
		     (#t 'done)))))
      (foo 1))))




(define example-2
  '(    
( broadcaster  a)
( flipflop a  inv  con)
( conjunction inv  b)
( flipflop b  con)
( conjunction con  output)
))


(define puzzle
  '(
( flipflop jb  ps)
( flipflop cm  ps  tm)
( flipflop sl  ml  cp)
( flipflop qr  ml)
( flipflop hf  kh  jg)
( flipflop jg  kk)
( flipflop jt  pq)
( flipflop qv  kv)
( flipflop rj  mm  kh)
( flipflop kf  xt)
( flipflop kx  vk  mk)
( flipflop dq  qn)
( conjunction ps  xc  mq  jt  zs  sr  nt  pq)
( flipflop jk  hh  ps)
( flipflop rr  mk  nh)
( flipflop hs  kh  mb)
( flipflop mg  mk  kf)
( flipflop xt  dq  mk)
( conjunction xc  zh)
( flipflop mq  nt)
( flipflop nh  bm)
( conjunction ml  bp  gd  qv  kq)
( flipflop md  hs)
( flipflop vk  mk  vl)
( flipflop mm  kh)
( conjunction th  zh)
( conjunction zh  rx)
( flipflop kc  ps  jk)
( flipflop kk  dm)
( flipflop jn  ll  ml)
( conjunction pd  zh)
( conjunction kh  jg  qx  md  th  hf  dm  kk)
( flipflop pp  kh  md)
( flipflop zf  ml  bd)
( flipflop qx  pp)
( conjunction mk  kf  qn  nh  pd  dq  mg  bm)
( flipflop qn  rr)
( flipflop mb  qb  kh)
( flipflop nt  jt)
( flipflop vl  zk  mk)
( flipflop gd  ml  rm)
( flipflop hh  ps  jb)
( flipflop tm  ps  mq)
( flipflop kv  jn  ml)
( flipflop zs  kc)
( flipflop ll  ml  kq)
( flipflop cp  qv  ml)
( flipflop rm  sl  ml)
( flipflop bd  qr  ml)
( flipflop dm  qx)
( flipflop qb  rj  kh)
( flipflop pq  zs)
( flipflop bm  kx)
( flipflop sr  cm  ps)
( flipflop zk  mk)
( broadcaster -> sr  gd  mg  hf)
( flipflop kq  zf)
( conjunction bp  zh)    
    ))























