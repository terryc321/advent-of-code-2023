(import scheme)

(import simple-exceptions)

(import (chicken string))
(import (chicken pretty-print))
(define pp pretty-print)

(import (chicken io))
(import (chicken format))
(import (chicken sort))
(import (chicken file))
(import (chicken process-context))
;; (change-directory "day15")
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

;;--------------------------------------

;; change input file ! 
(define (get-input) (call-with-input-file "input"
		      (lambda (port)
			(read port))))

(define input (get-input))

(define example #f)

;;(set! input example)

(define (1+ x) (+ x 1))
(define (1- x) (- x 1))

(define (10+ x) (+ x 10))
(define (10- x) (- x 10))

(define rest cdr)

;;------------------------------------------

#|
    Determine the ASCII code for the current character of the string.
    Increase the current value by the ASCII code you just determined.
    Set the current value to itself multiplied by 17.
    Set the current value to the remainder of dividing itself by 256.
|#

(define (compute-hash s)
  (let ((slen (string-length s))
	(hash 0))
    (do-for i (0 slen 1)
	    (let ((asc (char->integer (string-ref s i))))
	      (set! hash (modulo (* 17 (+ hash asc)) 256))
	      (format #t "s[~a] = ~a : ascii ~a : hash ~a~%" i (string-ref s i) asc hash)))
    hash))

(define (part-1)
  (apply + (map compute-hash (string-split input ","))))

#|

(part-1)

518107

|#























