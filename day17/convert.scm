
#|

convert example and input data to a suitable standard ml data structure


|#

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

(define (read-all f)
  (call-with-input-file f
    (lambda (port)
      (let ((lines '()))
	(letrec ((foo (lambda ()
			(let ((line (read-line port)))
			  (format #t "~a ~%" line)
			  (cond
			   ((eof-object? line) (reverse lines))
			   (#t (set! lines (cons line lines))
			       (foo)))))))
	  (foo))))))


(define (char->int ch)
  (let ((n (- (char->integer ch) (char->integer #\0))))
    (cond
     ((and (>= n 0) (<= n 9)) #t)
     (#t (set! fails (cons `((char->int ,ch) ,n expected 0 to 9) fails))))
    n))


(define (list->grid v)
  (list->vector (map (lambda (x) (list->vector (map char->int (string->list x))))  v)))

(define input  (list->grid (read-all "input")))
(define cost-input  (list->grid (read-all "input")))


(define example (list->grid (read-all "example")))
(define cost-example (list->grid (read-all "example")))

;;(define example2 (list->grid (read-all "example2")))

(define (get-xy vv x y)
  (vector-ref (vector-ref vv y) x))

(define (grid-width vv)
  (vector-length (vector-ref vv 0)))

(define (grid-height vv)
  (vector-length vv))

(define (set-xy! vv x y z)
  (vector-set! (vector-ref vv y) x z))

  
;; BUG set-xy! vv j #f
(define (flatten-cost vv)
  (do-list (i (iota (grid-width vv)))
	   (do-list (j (iota (grid-height vv)))
		    (set-xy! vv i j #f))))



(define (grid->sml vv var)
  (format #t "~%val ~a = [~%" var)
  (do-list (j (iota (grid-height vv)))
	   (when (not (= j 0))
	     (format #t ",~%"))
	   (format #t "[")
	   (do-list (i (iota (grid-width vv)))
		    (when (not (= i 0))
		      (format #t ","))
	   	    (format #t "~a" (get-xy vv i j)))	     
	   (format #t "]"))	   
  (format #t "];~%"))


(define (grid->sml0 vv var)
  (format #t "~%val ~a = [~%" var)
  (do-list (j (iota (grid-height vv)))
	   (when (not (= j 0))
	     (format #t ",~%"))
	   (format #t "[")
	   (do-list (i (iota (grid-width vv)))
		    (when (not (= i 0))
		      (format #t ","))
	   	    (format #t "~a" 0))
	   (format #t "]"))	   
  (format #t "];~%"))







  

