#lang racket

;; guile
;;(getcwd)
;;(chdir "day20/guile")
;; (import (ice-9 format))


#|

not entirely clear on inputs for a conjunction module until scan though all flip flops
and conjunctions to see
also if broadcaster were to pulse a conjunction what does that mean ? what is the
value of the broadcaster ?

conjunction has to know all its inputs before hand so it can assign a lo state to them
for example a b c wre inputs to conjunction a=lo b=lo c=lo

on trigger a conjunction it also has to know where did pulse come from

|#

(define example1 '(machine (broadcaster (a  b  c))
			   (flipflops 
			    (a   b)
			    (b   c)
			    (c  inv))
			   (conjunctions (inv   a))))


;; defmacro
;; (define-macro (save . body)
;;   `(set! todo-stack (append todo-stack (list (lambda () ,@body)))))
(define-syntax save
  (syntax-rules ()
    ((_ stk body ...) (set! stk (append stk (list (lambda () body ...)))))))

(define-syntax format2
  (syntax-rules ()
    ((_ out body ...) (format body ...))))


    


;; (let ((stack '()))
;;   (save (format #t "hello world!"))
;;   ((car stack)))


;;solve for given machine - just example1 
(define (run)
  ;;(define (make-flip-flop) #t)
  ;; broadcast do ?
  ;; a b c 
  ;; flip flops all initially
  ;; conjunctions
  ;; inv from c input only , output to a
  ;;
  (define todo-stack '())
  (define working-stack '())
      
  ;; what does broadcaster do exactly - sends lo pulse to all modules  
  (define (broadcast)
    (save todo-stack
     (format2 #t "broadcast low -> a~%")
     (fa 'lo))

    (save todo-stack
     (format2 #t "broadcast low -> b~%")
     (fb 'lo))

    (save todo-stack
     (format2 #t "broadcast low -> c~%")
     (fc 'lo)))
  
  (define (flip-state state)
    (cond
     ((eq? state 'on) 'off)
     ((eq? state 'off) 'on)))
  ;; flip flop a , states on or off , pulses lo or hi

  ;; fa -> fb
  (define fa (let ((state 'off))
	       (lambda (pulse)
		 (cond
		  ((eq? pulse 'hi) #t)		  
		  ((eq? pulse 'lo) (cond
				    ((eq? state 'off)
				     (save todo-stack
				      (set! state (flip-state state))
				      (format2 #t "a hi -> b~%")
				      (fb 'hi))
				     ((eq? state 'on)
				      (save todo-stack 
				       (set! state (flip-state state))
				       (format2 #t "a lo -> b~%")
				       (fb 'lo))))))))))
  
  ;; fb -> fc 
  (define fb (let ((state 'off))
	       (lambda (pulse)
		 (cond
		  ((eq? pulse 'hi) #t)		  
		  ((eq? pulse 'lo) (cond
				    ((eq? state 'off)
				     (save todo-stack
				      (set! state (flip-state state))
				      (format2 #t "b hi -> c~%")
				      (fc 'hi)))
				    ((eq? state 'on)
				     (save todo-stack
				      (set! state (flip-state state))
				      (format2 #t "b lo -> c~%")
				      (fc 'lo)))
				    ))))))
  
  ;; fc -> finv
  (define fc (let ((state 'off))
	       (lambda (pulse)
		 (cond
		  ((eq? pulse 'hi) #t)		  
		  ((eq? pulse 'lo) (cond
				    ((eq? state 'off)
				     (save todo-stack
				      (set! state (flip-state state))
				      (format2 #t "c hi -> inv from c~%")
				      (finv 'hi 'c)))
				    ((eq? state 'on)
				     (save todo-stack
				      (set! state (flip-state state))
				      (format2 #t "c lo -> inv from c~%")
				      (finv 'lo 'c))
				     )))))))
  

  
  ;; finv -> fa  ;; conjunction module
  (define finv (let ((memory-c 'lo))
		 (lambda (pulse from)
		   (cond
		    ((eq? from 'c) (set! memory-c pulse)))
		   ;; all hi ?
		   (cond
		    ((eq? memory-c 'hi) ;; send low pulse
		     ;;(send-lo-pulse fa)
		     (save todo-stack
		      (format2 #t "inv lo -> a~%")
		      (fa 'lo)))
		    (#t ;; send hi pulse
		     ;;(send-hi-pulse fa)
		     (save todo-stack
		      (format2 #t "inv hi -> a~%")
		      (fa 'hi)))))))
  

  (define (run-loop)
    (let loop ()
      (cond
       ((null? working-stack)
	(set! working-stack todo-stack)
	(cond
	 ((null? working-stack) #f)
	 (#t (run-loop))))
       (#t (let ((fn (car working-stack)))
	     (set! working-stack (cdr working-stack))
	     (fn)
	     (loop))))))
  
  ;; start 
    (broadcast)
    (run-loop))

  


(define puzzle '(machine (broadcaster ( sr  gd  mg  hf))
			 (conjunctions (ps   xc  mq  jt  zs  sr  nt  pq)
					(xc   zh)
					(ml   bp  gd  qv  kq)
					(th   zh)
					(zh   rx)
					(pd   zh)
					(kh   jg  qx  md  th  hf  dm  kk)
					(mk   kf  qn  nh  pd  dq  mg  bm)
					(bp   zh))
			 (flipflops  (jb   ps)
				     (cm   ps  tm)
				     (sl   ml  cp)
				     (qr   ml)
				     (hf   kh  jg)
				     (jg   kk)
				     (jt   pq)
				     (qv   kv)
				     (rj   mm  kh)
				     (kf   xt)
				     (kx   vk  mk)
				     (dq   qn)
				     (jk   hh  ps)
				     (rr   mk  nh)
				     (hs   kh  mb)
				     (mg   mk  kf)
				     (xt   dq  mk)
				     (mq   nt)
				     (nh   bm)
				     (md   hs)
				     (vk   mk  vl)
				     (mm   kh)
				     (kc   ps  jk)
				     (kk   dm)
				     (jn   ll  ml)
				     (pp   kh  md)
				     (zf   ml  bd)
				     (qx   pp)
				     (qn   rr)
				     (mb   qb  kh)
				     (nt   jt)
				     (vl   zk  mk)
				     (gd   ml  rm)
				     (hh   ps  jb)
				     (tm   ps  mq)
				     (kv   jn  ml)
				     (zs   kc)
				     (ll   ml  kq)
				     (cp   qv  ml)
				     (rm   sl  ml)
				     (bd   qr  ml)
				     (dm   qx)
				     (qb   rj  kh)
				     (pq   zs)
				     (bm   kx)
				     (sr   cm  ps)
				     (zk   mk)
				     (kq   zf))))


;; each symbol either conjunction OR flipflop not both , can be in broadcaster also
;; how many symbols are there ?

;; (pulse SYM lo) low pulse
;; (pulse SYM hi) hi pulse 

;; conjunction module takes inputs from outputs of flip flops

