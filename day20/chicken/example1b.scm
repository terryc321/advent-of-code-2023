
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

;; (load "example1b.scm")
;; (load "solver.scm")
;; (part-1)
;; (32000000 8000 4000)

(define (push)
  ;; broadcast sends a low pulse to [ sr gd mg hf ]
  (incf low-pulse-sent-count)
  (a 'pulse (low) 'broadcast)
  (incf low-pulse-sent-count)
  (b 'pulse (low) 'broadcast)
  (incf low-pulse-sent-count)
  (c 'pulse (low) 'broadcast)
  (keep-working))

