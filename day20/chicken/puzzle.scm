
(import (chicken format))
(import (chicken pretty-print))
(import (srfi-1))
(import (srfi-69)) ;; hash


(define puzzle '(
		 (broadcast sr gd mg hf)
		 (flipflops
		  ( jb   ps)
		  ( cm   ps  tm)
		  ( sl   ml  cp)
		  ( qr   ml)
		  ( hf   kh  jg)
		  ( jg   kk)
		  ( jt   pq)
		  ( qv   kv)
		  ( rj   mm  kh)
		  ( kf   xt)
		  ( kx   vk  mk)
		  ( dq   qn)
		  ( jk   hh  ps)
		  ( rr   mk  nh)
		  ( hs   kh  mb)
		  ( mg   mk  kf)
		  ( xt   dq  mk)
		  ( mq   nt)
		  ( nh   bm)
		  ( md   hs)
		  ( vk   mk  vl)
		  ( mm   kh)
		  ( kc   ps  jk)
		  ( kk   dm)
		  ( jn   ll  ml)
		  ( pp   kh  md)
		  ( zf   ml  bd)
		  ( qx   pp)
		  ( qn   rr)
		  ( mb   qb  kh)
		  ( nt   jt)
		  ( vl   zk  mk)
		  ( gd   ml  rm)
		  ( hh   ps  jb)
		  ( tm   ps  mq)
		  ( kv   jn  ml)
		  ( zs   kc)
		  ( ll   ml  kq)
		  ( cp   qv  ml)
		  ( rm   sl  ml)
		  ( bd   qr  ml)
		  ( dm   qx)
		  ( qb   rj  kh)
		  ( pq   zs)
		  ( bm   kx)
		  ( sr   cm  ps)
		  ( zk   mk)
		  ( kq   zf))

		 (conjunctions
		  ( ps   xc  mq  jt  zs  sr  nt  pq)
		  ( xc   zh)
		  ( ml   bp  gd  qv  kq)
		  ( th   zh)
		  ( zh   rx)
		  ( pd   zh)
		  ( kh   jg  qx  md  th  hf  dm  kk)
		  ( mk   kf  qn  nh  pd  dq  mg  bm)
		  ( bp   zh))
		 )
  )


;; flip flops ok since only one internal state
;; conjunctions need to know who the inputs for each conjunction is
;; also note a conjunction cannot feed a conjunction - atleast i dont think it can

;; (define all-symbols
;;   (let ((
    

(define all-broadcast (cdr (assoc 'broadcast puzzle)))

(define all-flipflops (map car (cdr (assoc 'flipflops puzzle))))

(define all-conjunctions (map car (cdr (assoc 'conjunctions puzzle))))

(define all-symbols (append all-flipflops all-conjunctions))


;; no flipflop is conjunction and visa versa
(define any-conjunctions-flipflops
  (append (list 'flipflop-is-conjunction)
	  (filter (lambda (x)(not (eq? x #f)))
		  (map (lambda (s) (member s all-conjunctions)) all-flipflops))
	  (list 'conjunction-is-flipflop)
	  (filter (lambda (x)(not (eq? x #f)))
		  (map (lambda (s) (member s all-flipflops)) all-conjunctions))))


;; no broadcast can be a conjunction
(define any-conjunctions-broadcast
  (append (list 'conjunction-is-broadcast)
	  (filter (lambda (x)(not (eq? x #f)))
		  (map (lambda (s) (member s all-conjunctions)) all-flipflops))
	  (list 'conjunction-is-flipflop)
	  (filter (lambda (x)(not (eq? x #f)))
		  (map (lambda (s) (member s all-flipflops)) all-conjunctions))))


(define flipflop?
  (lambda (s)
    (if (member s all-flipflops) #t #f)))

(define conjunction?
  (lambda (s)
    (if (member s all-conjunctions) #t #f)))


(define flipflop-outputs
  (lambda (s)
    (cond
     ((not (flipflop? s)) '())
     (#t (cdr (assoc s (cdr (assoc 'flipflops puzzle))))))))



(define conjunction-outputs
  (lambda (s)
    (cond
     ((not (conjunction? s)) '())
     (#t (cdr (assoc s (cdr (assoc 'conjunctions puzzle))))))))
    

(define conjunction-inputs
  (lambda (s)
    (cond
     ((not (conjunction? s)) '())
     (#t 
      (let ((inputs '()))
	(let loop ((fs (cdr (assoc 'flipflops puzzle))))
	  (cond
	   ((null? fs) inputs)
	   (#t (let ((def (car fs)))
		 (when (member s (cdr def))
		   (when (not (member s inputs))
		     (set! inputs (cons (car def) inputs))))
		 (loop (cdr fs))))))
	(let loop ((fs (cdr (assoc 'conjunctions puzzle))))
	  (cond
	   ((null? fs) inputs)
	   (#t (let ((def (car fs)))
		 (when (member s (cdr def))
		   (when (not (member s inputs))
		     (set! inputs (cons (car def) inputs))))
		 (loop (cdr fs))))))
	inputs)))))


(define type?
  (lambda (s)
    (cond
     ((flipflop? s) 'flip)
     ((conjunction? s) 'conj)
     (#t 'unknown))))



;; th gets its input from kh
(define (program)
  (pp (map
       (lambda (x) ;; x is a flipflop symbol
	 (list 'make-flipflop x
	       'outputs-> (flipflop-outputs x)))
       all-flipflops))
  (pp (map
       (lambda (x) ;; x is a conjunction symbol
	 (list 'make-conjunction x
	       'inputs-> (conjunction-inputs x)
	       'outputs-> (conjunction-outputs x)))
       all-conjunctions))
  )

(define symbol-vector #f)
(define symbol-count 0)

(define lookup-symbol
  (let ((hash (make-hash-table))
	(counter 0))
    (let loop ((as all-symbols))
      (cond
       ((null? as) #t)
       (#t (let ((sym (car as)))
	     (hash-table-set! hash sym counter)
	     (set! counter (+ 1 counter))
	     (loop (cdr as))))))
    ;; hash has list of symbols -> values which use to make a vector 
    (set! symbol-count counter)
    (set! symbol-vector (make-vector symbol-count))
    ;; iterate over symbols
    (set! counter 0)
    (let loop ((as all-symbols))
      (cond
       ((null? as) #t)
       (#t (let ((sym (car as)))
	     (vector-set! symbol-vector counter sym)	     
	     (set! counter (+ 1 counter))
	     (loop (cdr as))))))
    ;; lookup a symbol 
    (lambda (s)
      (hash-table-ref hash s))))





    




	  
	  




