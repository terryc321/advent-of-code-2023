
#|

flipflop.scm 

how does a flip flop work

Flip-flop modules (prefix %) are either on or off;

they are initially off.

If a flip-flop module receives a high pulse, it is ignored and nothing happens.

However, if a flip-flop module receives a low pulse, it flips between on and off.

If it was off, it turns on and sends a high pulse.
If it was on, it turns off and sends a low pulse.

flip flop has state - on or off
receives a pulse either high or low

-- pulse is either a high pulse or low pulse
datatype Pulse = HighPulse | LowPulse

-- flip flop on or off state
datatype FlipFlopState = FFOn | FFOff

-- flip flop module
receives a pulse and then tells other 'modules' what its output is

potential for infinite loop , non ending computation
a -> b -> a 

does entire system need to be controlled by a global clock , so nobody with
time of 2 seconds be processed before everyone with time of 1 seconds



|#

(import (chicken format))

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
       ((eq? op 'flip) (let ((input (car args)))
			 (cond
			  ((high? input) #f)
			  ((low?  input)
			   (let ((old-state state))
			     (set! state (flip-state state))
			     (cond
			      ((off? old-state)
			       (map (lambda (output) (output (high) id)) outputs))
			      ((on? old-state)
			       (map (lambda (output) (output (low) id)) outputs))
			      (#t (error (format #f "flipflop : flip : bad state ~a" old-state))))))			   			   
			  (#t (error (format #f "flipflop : flip : bad input ~a" input))))))
       (#t (error (format #f "flipflop : bad op : operation not understood ~a" op)))))))


(define f (make-flip-flop))
(f 'get-state)
(f 'flip (low))
(f 'flip (low))
(f 'flip (high))
(f 'set-id 'f)


(define output (lambda (pulse id) (format #t "received a (~a) pulse from module ~a~%" pulse id)))
(f 'add-outputs (list output))











