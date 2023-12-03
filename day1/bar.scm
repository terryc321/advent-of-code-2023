
(import scheme)

(import simple-exceptions)

(import (chicken pretty-print))
(define pp pretty-print)

(import (chicken format))
(import (chicken sort))
(import (chicken file))
(import (chicken process-context))
;; (change-directory "day1")
;; (current-directory)



(import procedural-macros)
(import regex)

(import simple-md5)
(import simple-loops)

(import srfi-69)
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


;;------------------------- code -----------------------------------

;; change input file ! 
(define (get-input) (call-with-input-file "input"
		      (lambda (port)
			(read port))))

(define input (get-input))


(define (1+ x) (+ x 1))
(define (1- x) (- x 1))

(define (10+ x) (+ x 10))
(define (10- x) (- x 10))

(define rest cdr)

;; ---------------------------------------------------------------------

#|
get first and last digit from string
|#

(define (digit-char? c)
  (and (char>=? c #\0) (char<=? c #\9)))
  
(define (fickle-substring s i k)
  (condition-case
      (with-exn-handler (lambda (e) 
			  ;;(format #t "caught excpetion ~a : returning empty string ~%" e)
			  "") 
			;;(lambda () (substring s i k))
			(lambda () (substring s i k))
			)
    ((exn) ;;(format #t "caught every excpetion : returning empty string ~%")
           "")))


(define (foo)
  (condition-case (substring "asdf" 4 2)
    ((exn) (print "catch all"))
    ((exn file) (print "ignored"))
    ((exn runtime) (print "ignored again"))
    ))

(define (trib s)
  (let ((lim (string-length s))
	(words '("one"  "two"  "three"  "four"  "five"  "six"  "seven"  "eight"  "nine"))
	(a #f)
	(b #f)
	(ndigits 0))
    (do-for (k 0 lim)
	    (let ((ch (string-ref s k)))
	      (cond
	       ((digit-char? ch)
		(let ((val (- (char->integer ch) (char->integer #\0))))
		  (set! ndigits (+ ndigits 1))
		  (set! b val)
		  (when (not a)
		    (set! a val))))
	       (#t ;; alphanumeric
		(call/cc (lambda (escape)
			   (let ((val 1))
			     (do-list (w words)
				      (let* ((wlen (string-length w))
					    (w2 (substring s (min k lim) 
								  (min lim (+ k wlen)))))
					;;(format #t "w = [~a] : w2 = [~a] ~%" w w2)
					(cond
					 ((string=? w w2) 
					  (format #t "matched numeric string ~a ~%" w2)
					  (escape (begin 
						    (set! ndigits (+ ndigits 1))
						    (set! b val)
						    (when (not a)
						      (set! a val))
						    #t)))
					 (#t (set! val (+ val 1)))))))))))))

    (cond
     ((>= ndigits 2) 'ok)
     (#t ;;(format #t "trib ndigits ~a : input ~a ~%" ndigits s)
	 ))
    (format #t "~a : ~a : ~a ~%" a b s)
    (+ (* a 10) b)
    ;;(error "debug...")
))



(define (part-2)
  (apply + (map trib input)))



#|
(part-2)
53855

acepted
|#



  
