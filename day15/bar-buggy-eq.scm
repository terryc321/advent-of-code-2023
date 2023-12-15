#|

somewhere in the code uses symbols like qt pc ot

list containing ((ot 9)(ab 5)(pc 6))

BUG :
symbol is not EQ with itself

data structure was using strings that looked like symbols but were not , simply strings

list actually contained (("ot" 9)("ab" 5)("pc" 6))
but presented as ((ot 9)(ab 5)(pc 6))



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

#|

Determine the ASCII code for the current character of the string.
Increase the current value by the ASCII code you just determined.
Set the current value to itself multiplied by 17.
Set the current value to the remainder of dividing itself by 256.

|#

(define (chash s)
  (let ((slen (string-length s))
	(hash 0))
    (do-for i (0 slen 1)
	    (let ((asc (char->integer (string-ref s i))))
	      (set! hash (modulo (* 17 (+ hash asc)) 256))
	      ;;(format #t "s[~a] = ~a : ascii ~a : hash ~a~%" i (string-ref s i) asc hash)
	      )
	    )
    hash))

(define (part-1)
  (apply + (map chash (string-split input ","))))


(define (chop s)
  (let ((slen (string-length s)))
    (call/cc (lambda (escape)
	       (do-for i (0 slen 1)
		       (let ((ch (string-ref s i)))
			 (cond
			  ((char=? ch #\=) (escape (list '=
							 (string->symbol (substring s 0 i))
							 (chash (substring s 0 i))
							 (string->number
							  (substring s (+ i 1) slen)))))
			  ((char=? ch #\-) (escape (list '-
							 (string->symbol (substring s 0 i))
							 (chash (substring s 0 i))
							 ))))))))))

	      
(define example "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")

(define (replace-or-append xs lab key)
  (cond
   ((null? xs) (list (list lab key)))
   ((eq? (car (car xs)) lab) (cons (list lab key) (cdr xs))) ;; replace EQUAL? with EQ?
   (#t (cons (car xs) (replace-or-append (cdr xs) lab key)))))

(replace-or-append '((a 1)(b 2)(c 3)) 'a 4)
(replace-or-append '((a 1)(b 2)(c 3)) 'b 5)
(replace-or-append '((a 1)(b 2)(c 3)) 'c 6)
(replace-or-append '((a 1)(b 2)(c 3)) 'd 7)


#|
BUG :
symbol is not EQ with itself

data structure was using strings that looked like symbols but were not , simply strings

|#

(define (remove-lab2 xs lab)
  (cond
   ((null? xs) xs)
   ((eq? (car (car xs)) lab) (cdr xs))  ;; <<<<<<< replace equal with eq?  fails .....
   (#t (cons (car xs) (remove-lab2 (cdr xs) lab)))))

(define (remove-lab xs lab)
  (format #t "removing label ~a from ~a ~%" lab xs)
  (let ((res (remove-lab2 xs lab)))
    res))



(remove-lab '((a 1)(b 2)(c 3)) 'a)
(remove-lab '((a 1)(b 2)(c 3)) 'b)
(remove-lab '((a 1)(b 2)(c 3)) 'c)
(remove-lab '((a 1)(b 2)(c 3)) 'd)

(define (show-boxes v)
  (format #t "~%<boxes>~%")
  (let ((vlen (vector-length v)))
    (do-for i (0 vlen 1)
	    (let ((vref (vector-ref v i)))
	      (cond
	       ((null? vref) #f)
	       (#t (format #t "~a : ~a ~%" i vref)))))
    (format #t "~%")))

  

(define (chop-xs s)
  (define boxes (make-vector 256 '()))
  (do-list (inst (map chop (string-split s ",")))
	   (match inst
	     (('= lab box key) (vector-set! boxes box (replace-or-append (vector-ref boxes box) lab key)))
	     (('- lab box)     (vector-set! boxes box (remove-lab (vector-ref boxes box) lab)))
	     (_ (error "chop-xs no match.")))
	   (format #t "after ~a : ~%" inst)
	   (show-boxes boxes)))



(define (debug-chop-xs s)
  (define boxes (make-vector 256 '()))
  (let  ((inst (map chop (string-split s ","))))
    (format #t "inst ~%~a~%" inst)
    (newline)
    (display inst)
    inst))

  



(map (lambda (x) (eq? 'pc (second x)))
     (debug-chop-xs example))

(map (lambda (x) (symbol? (second x)))
     (debug-chop-xs example))

(map (lambda (x) (second x))
     (debug-chop-xs example))



(debug-chop-xs example)





#|
#;1059> (chop-xs example)
after (= rn 0 1) : 

<boxes>
0 : ((rn 1)) 

removing label cm from ((rn 1)) 
after (- cm 0) : 

<boxes>
0 : ((rn 1)) 

after (= qp 1 3) : 

<boxes>
0 : ((rn 1)) 
1 : ((qp 3)) 

after (= cm 0 2) : 

<boxes>
0 : ((rn 1) (cm 2)) 
1 : ((qp 3)) 

removing label qp from ((qp 3)) 
after (- qp 1) : 

<boxes>
0 : ((rn 1) (cm 2)) 

after (= pc 3 4) : 

<boxes>
0 : ((rn 1) (cm 2)) 
3 : ((pc 4)) 

after (= ot 3 9) : 

<boxes>
0 : ((rn 1) (cm 2)) 
3 : ((pc 4) (ot 9)) 

after (= ab 3 5) : 

<boxes>
0 : ((rn 1) (cm 2)) 
3 : ((pc 4) (ot 9) (ab 5)) 

removing label pc from ((pc 4) (ot 9) (ab 5)) 
after (- pc 3) : 

<boxes>
0 : ((rn 1) (cm 2)) 
3 : ((ot 9) (ab 5)) 

after (= pc 3 6) : 

<boxes>
0 : ((rn 1) (cm 2)) 
3 : ((ot 9) (ab 5) (pc 6)) 

after (= ot 3 7) : 

<boxes>
0 : ((rn 1) (cm 2)) 
3 : ((ot 9) (ab 5) (pc 6) (ot 7))    <<<<<<<< using EQ? we have OT 9   

#;1069> (chop-xs example)  -----------------------------------------
after (= rn 0 1) : 

<boxes>
0 : ((rn 1)) 

removing label cm from ((rn 1)) 
after (- cm 0) : 

<boxes>
0 : ((rn 1)) 

after (= qp 1 3) : 

<boxes>
0 : ((rn 1)) 
1 : ((qp 3)) 

after (= cm 0 2) : 

<boxes>
0 : ((rn 1) (cm 2)) 
1 : ((qp 3)) 

removing label qp from ((qp 3)) 
after (- qp 1) : 

<boxes>
0 : ((rn 1) (cm 2)) 

after (= pc 3 4) : 

<boxes>
0 : ((rn 1) (cm 2)) 
3 : ((pc 4)) 

after (= ot 3 9) : 

<boxes>
0 : ((rn 1) (cm 2)) 
3 : ((pc 4) (ot 9)) 

after (= ab 3 5) : 

<boxes>
0 : ((rn 1) (cm 2)) 
3 : ((pc 4) (ot 9) (ab 5)) 

removing label pc from ((pc 4) (ot 9) (ab 5)) 
after (- pc 3) : 

<boxes>
0 : ((rn 1) (cm 2)) 
3 : ((ot 9) (ab 5)) 

after (= pc 3 6) : 

<boxes>
0 : ((rn 1) (cm 2)) 
3 : ((ot 9) (ab 5) (pc 6)) 

after (= ot 3 7) : 

<boxes>
0 : ((rn 1) (cm 2)) 
3 : ((ot 7) (ab 5) (pc 6))   ;; <<<<<<<<<<<<< using EQUAL? have OT 7

#;1106> 
|#

