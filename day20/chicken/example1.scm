
(import (procedural-macros))


;; -------------------------------------------------
(define-macro (incf x)
  `(set! ,x (+ ,x 1)))

;; -------------------------------------------------
;; (delay! x) we add expressions to the todo-group
(define todo-group '())
(define-macro (delay! . body)
  `(begin
     (set! todo-group (cons (lambda () ,@body) todo-group))))

(define working-group '())

;;(delay! (format #t "alpha") (format #t "beta!~%"))





;; ---------------------------------------------
;; from puzzle.scm

(import (chicken format))
(import (chicken pretty-print))
(import (srfi-1))
(import (srfi-69)) ;; hash
(define ppp pp)

(define puzzle '(
		 (broadcast a b c)
		 (flipflops
		  ( a b)
		  ( b c)
		  ( c inv)
		  )
		 (conjunctions
		  ( inv a)
		  )))


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

;; ---------------------------------------------
;; from flipflop.scm

(define high-pulse-sent-count 0)
(define low-pulse-sent-count 0)

;; -------------------------------------------------

(define (flip-flop-initial-state) 'off)

(define (on) 'on)
(define (off) 'off)
(define (on? s) (eq? s 'on))
(define (off? s) (eq? s 'off))

(define (low) 'low)
(define (high) 'high)
(define (low? s) (eq? s 'low))
(define (high? s) (eq? s 'high))

(define (flip-state s)
  (cond
   ((on? s) (off))
   ((off? s) (on))
   (#t (error (format #f "flip-state : bad state ~a!~%" s)))))

;; a module has to take
;; 1) pulse either low or high
;; 2) symbol id on who sent the message

(define (make-flip-flop)
  (let ((state (flip-flop-initial-state))
	(id (gensym "flipflop"))
	(outputs '()))
    (lambda (op . args)
      (cond
       ((eq? op 'get-type) 'flipflop)
       ((eq? op 'get-state) state)
       ((eq? op 'get-id) id)
       ((eq? op 'set-id) (let ((new-id (car args)))
			   (cond
			    ((symbol? new-id) (set! id new-id))
			    (#t (error (format #f "flipflop : set-id : bad id not a symbol ~a" new-id))))))       
       ((eq? op 'add-outputs)
	(format #t "flipflop : add-outputs : args given = (~a)~%" (car args))
	(set! outputs (append outputs (car args)))
	(format #t "flipflop : outputs now : (~a)~%" outputs))
       ((eq? op 'pulse)
	(let ((input (car args))
	      (from (if (null? (cdr args)) 'anon (car (cdr args)))))
	  (format #t "flipflop (~a) : received pulse(~a) from ~a ~%" id input from)
	  (delay! ;; DELAY MACRO !!
	  (cond
	   ((high? input) #f)
	   ((low?  input)
	    (let ((old-state state))
	      (set! state (flip-state state))
	      (cond
	       ((off? old-state)
		(map (lambda (output)
		       (let ((factual (eval output)))
			 (incf high-pulse-sent-count)
			 (factual 'pulse (high) id))) outputs))
	       ((on? old-state)
		(map (lambda (output)
		       (let ((factual (eval output)))
			 (incf low-pulse-sent-count)
			 (factual 'pulse (low) id))) outputs))
	       (#t (error (format #f "flipflop : flip : bad state ~a" old-state))))))
	   (#t (error (format #f "flipflop : flip : bad input ~a" input)))))))
       (#t (error (format #f "flipflop : bad op [~a]: operation not understood ~a" id op)))))))





;; (define f (make-flip-flop))
;; (f 'get-state)
;; (f 'pulse (low))
;; (f 'pulse (low))
;; (f 'pulse (high))
;; (f 'set-id 'f)


;; (define output (lambda (pulse id) (format #t "received a (~a) pulse from module ~a~%" pulse id)))
;; (f 'add-outputs (list output))

;; ----------------------------------------------------
;; from conjunction.scm

#|

conjunction has to know where all its inputs are coming from

when receives a flip message it updates its internal memory for that module

looks at all modules and

if remembers high pulses for all inputs - sends a low pulse
otherwise it sends a high pulse

|#


(define (make-alist-of-symbols symbols)  
  (map (lambda (sym)
	 (cond
	  ((not (symbol? sym))
	   (error (format #f "make-alist-of-symbols not a symbol (~a)" sym)))
	  (#t (list sym 0))))
       symbols))

(define (make-low-alist-of-symbols symbols)  
  (map (lambda (sym)
	 (cond
	  ((not (symbol? sym))
	   (error (format #f "make-low-alist-of-symbols not a symbol (~a)" sym)))
	  (#t (list sym (low)))))
       symbols))


(define (set-alist-value! alist sym val)
  (let ((kv (assoc sym alist)))
    (cond
     ((pair? kv)
      (set-car! (cdr kv) val))
     (#t (error (format #f "set-alist-value! no key-value found for symbol (~a)" sym))))))

(define (lookup-alist-value alist sym)
  (let ((kv (assoc sym alist)))
    (cond
     ((pair? kv)
      (car (cdr kv)))
     (#t (error (format #f "lookup-alist-value no key-value found for symbol (~a)" sym))))))


(define (all-values alist)
  (map (lambda (x)(car (cdr x))) alist))

(define (all-high? alist)
  (null? (filter (lambda (x)(not (high? x))) (all-values alist))))


;; conjunctions have their state in an association list of symbols 
  ;; a module has to take
;; 1) pulse either low or high
;; 2) symbol id on who sent the message

(define (make-conjunction)
  (let ((id (gensym "flipflop"))
	(inputs '())
	(outputs '()))
    (lambda (op . args)
      (cond
       ((eq? op 'get-type) 'conjunction)
       ((eq? op 'get-state) inputs)
       ((eq? op 'get-id) id)
       ((eq? op 'set-id) (let ((new-id (car args)))
			    (cond
			     ((symbol? new-id) (set! id new-id))
			     (#t (error (format #f "conjunction : set-id! : bad id not a symbol ~a" new-id))))))
       ;; so we can add inputs but what do they do tho?
       ((eq? op 'add-inputs)
	(format #t "flipflop : add-inputs : args given = (~a)~%" (car args))
	(set! inputs (append inputs (make-low-alist-of-symbols (car args))))
	(format #t "flipflop : inputs now : (~a)~%" inputs))
       
       ;; outputs get send a pulse with an id after a flip has occurred
       ((eq? op 'add-outputs)
	(format #t "flipflop : add-outputs : args given = (~a)~%" (car args))
	(set! outputs (append outputs (car args)))
	(format #t "flipflop : outputs now : (~a)~%" outputs))
       
       ((eq? op 'pulse)
	(let ((input (car args))
	      (from (car (cdr args))))
	  (delay! ;; DELAY MACRO 
	  (format #t "conjunction (~a) : received pulse(~a) from ~a ~%" id input from)
	  ;; save signal
	  (set-alist-value! inputs from input)
	  ;; all high? - send a low pulse
	  ;; otherwise - send a high pulse
	  (cond
	   ((all-high? inputs)
	    (map (lambda (output)
		   (let ((factual (eval output)))
		     (incf low-pulse-sent-count)
		     (factual 'pulse (low) id))) outputs))
	   (#t
	    (map (lambda (output)
		   (let ((factual (eval output)))
		     (incf high-pulse-sent-count)
		     (factual 'pulse (high) id))) outputs))))))
       
       (#t (error (format #f "conjunction : (~a) bad op : operation not understood ~a" id op)))))))




;; (define f (make-flip-flop))
;; (f 'get-state)
;; (f 'pulse (low))
;; (f 'pulse (low))
;; (f 'pulse (high))
;; (f 'set-id! 'f)



;; (define output (lambda (pulse id) (format #t "module [OUT] : received a (~a) pulse from module ~a~%" pulse id)))


;; ;; (define aa (make-low-alist-of-symbols '(a b c d e f g)))
;; (define bb (make-conjunction))
;; ;;
;; (bb 'add-inputs '(a b c))
;; ;;
;; (bb 'set-id 'BB)
;; (bb 'add-outputs (list output))

;; ;; have a go at sending signals to bb  
;; (bb 'pulse (low) 'a)
;; (bb 'pulse (low) 'b)
;; (bb 'pulse (low) 'c)

;; (bb 'pulse (high) 'a)
;; (bb 'pulse (high) 'b)
;; (bb 'pulse (high) 'c)


(define procedure-vector (make-vector (vector-length symbol-vector)))


;; make all symbols defined as #f false
;; (eval X (scheme-report-environment 5))
;; (eval X (null-environment 5))
;; (eval X) ;; defaults to (interaction-environment) which is what we want
(map (lambda (x) (eval `(define ,x #f))) all-symbols)

(map (lambda (x) (cond
		  ((flipflop? x)
		   (let ((code `(begin
					  (let ((func (make-flip-flop)))
					    (set! ,x func)
					    (func 'set-id ',x)
					    (func 'add-outputs ',(flipflop-outputs x))
					    ))))
		     (format #t "evaluating this code ...~%~a~%" code)
		     (eval code)))
		  ((conjunction? x)
		   (let ((code `(begin
					  (let ((func (make-conjunction)))
					    (set! ,x func)
					    (func 'set-id ',x)
					    (func 'add-inputs ',(conjunction-inputs x))
					    (func 'add-outputs ',(conjunction-outputs x))
					    ))))
		     (format #t "evaluating this code ...~%~a~%" code)
		     (eval code)))))
     all-symbols)



;; move todo into
(define keep-working
  (lambda ()
    (cond
     ((null? working-group)
      (set! working-group todo-group)
      (set! todo-group '())
      (when (not (null? working-group))
	(keep-working)))
     (#t (let ((task (car working-group)))
	   (set! working-group (cdr working-group))
	   (task)
	   (keep-working))))))

      
    


(define (push)
  ;; broadcast sends a low pulse to [ sr gd mg hf ]
  (incf low-pulse-sent-count)
  (a 'pulse (low) 'broadcast)
  (incf low-pulse-sent-count)
  (b 'pulse (low) 'broadcast)
  (incf low-pulse-sent-count)
  (c 'pulse (low) 'broadcast)
  (keep-working))
  
(define (button)
  (incf low-pulse-sent-count)
  (push))


(define (repeat n func)
  (cond
   ((<= n 0) #t)
   (#t (func)
       (repeat (- n 1) func))))


(define (part1)
  (repeat 1000 (lambda () (button)))
  (let ((result (* low-pulse-sent-count
		   high-pulse-sent-count)))
    (list result low-pulse-sent-count high-pulse-sent-count)))

  

(define (debug)
  (repeat 1 (lambda () (button)))
  (let ((result (* low-pulse-sent-count
		   high-pulse-sent-count)))
    (list result low-pulse-sent-count high-pulse-sent-count)))


  

;; too low ...
;; > (part1)
;; 640176120


  
       











					    





