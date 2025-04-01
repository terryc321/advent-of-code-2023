

(define puzzle '(
		 (broadcast a)
		 (flipflops
		  (output)
		  ( a inv con)
		  ( b con)
		  )
		 (conjunctions
		  ( inv b)
		  ( con output)		  
		  )))


;; (load "example2.scm")
(load "solver.scm")
;; (part1)
;; (32000000 8000 4000)

(define (push)
  ;; broadcast sends a low pulse to [ sr gd mg hf ]
  (incf low-pulse-sent-count)
  (a 'pulse (low) 'broadcast)
  (keep-working))


