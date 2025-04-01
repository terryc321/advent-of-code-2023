
#|

conjunction has to know where all its inputs are coming from

when receives a flip message it updates its internal memory for that module

looks at all modules and

if remembers high pulses for all inputs - sends a low pulse
otherwise it sends a high pulse


|#

(import (chicken format))
(import (srfi-1)) ;; first second filter

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
       
       ((eq? op 'flip) (let ((input (car args))
			     (from (car (cdr args))))
			 (format #t "conjunction : flip : input(~a) from(~a)~%" input from)
			 ;; save signal
			 (set-alist-value! inputs from input)
			 ;; all high? - send a low pulse
			 ;; otherwise - send a high pulse
			 (cond
			  ((all-high? inputs)
			   (map (lambda (output) (output (low) id)) outputs))
			  (#t
			   (map (lambda (output) (output (high) id)) outputs)))))
       
       (#t (error (format #f "flipflop : bad op : operation not understood ~a" op)))))))


;; (define f (make-flip-flop))
;; (f 'get-state)
;; (f 'flip (low))
;; (f 'flip (low))
;; (f 'flip (high))
;; (f 'set-id! 'f)



(define output (lambda (pulse id) (format #t "module [OUT] : received a (~a) pulse from module ~a~%" pulse id)))


;; (define aa (make-low-alist-of-symbols '(a b c d e f g)))
(define bb (make-conjunction))
;;
(bb 'add-inputs '(a b c))
;;
(bb 'set-id 'BB)
(bb 'add-outputs (list output))

;; have a go at sending signals to bb  
(bb 'flip (low) 'a)
(bb 'flip (low) 'b)
(bb 'flip (low) 'c)

(bb 'flip (high) 'a)
(bb 'flip (high) 'b)
(bb 'flip (high) 'c)














