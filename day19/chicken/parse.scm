
(import scheme)
(import (chicken format))
(import srfi-1)
(import srfi-2)
(import (chicken pretty-print))
;;; scheme code for the parse

;; (+ 7540 4623 6951  )  ;; 19114

;; (define tests
;;   '(
;;     "vr{a>3414:A,R}"
;;     "lx{x>2140:A,A}"
;;     "vkm{s>411:A,a>443:R,A}"
;;     ))

#|
changelog

1) for sake simplicity assume all parse routines take a list of characters 
2) rather than typing parse-number at repl make shortcut aliases like pn
to mean parse-number
3) continuation passing style parser - see if it gives us what we want
coompare to other type of parsing
4) if success and failure continuation that makes next thing - exponential ?
 each try at something gives rise to two paths ...
5) note to self - cannot use quote ' mark as can in haskell , as quote ' in lisp
means something else entirely rather than x' meaning x prime variable its
variable x followed by an expected quotation
6) not verified parse matches input string completely , we end on #\} closing brace character
so , guess thats something i suppose , functional parser , so should be okay 
7) nested failures are not nice at all - callback hell
8) make code generator to solve the puzzle 

bit long winded with 

cps parser
got a word -> success
no word -> failure ... 

functional parser ?
assume string is a sequence of characters like a list of characters ?

word may be A - to accept , R - to reject , another word - in which case go to that
workflow unconditionally 

word - bracket {  word  < > = INT  :COLON  word   ,COMMA?
                  word  < > = INT  :COLON  word    ,COMMA?
                  word  < > = INT  :COLON  word     ,COMMA?  
                  final-word 


peos  : parse end of string : match empty string - as in successfully parsed everything else
pw    : parse word
pnum  : parse number : integer in this case - converted to an integer also
pcol  : parse colon  : 
psemi : parse semicolon : 
pcom  : parse comma , 
pcb   : parse close bracket }
pob   : parse open bracket {
p>    : parse greater than
p<    : parse less than 


;; stage 1 parses handles word { word < > NUM : word , .... , word }  type patterns 
#;1> (test-st1)
((s= "vr{a>3414:A,R}" ":parse-> " (success (vr (if (> a 3414) A) R)))
 (s= "lx{x>2140:A,A}" ":parse-> " (success (lx (if (> x 2140) A) A)))
 (s= "vkm{s>411:A,a>443:R,A}"
     ":parse-> "
     (success (vkm (if (> s 411) A) (if (> a 443) R) A)))
 (s= "nr{x>1463:R,A}" ":parse-> " (success (nr (if (> x 1463) R) A)))
 (s= "xd{m>375:R,x<2704:R,R}"
     ":parse-> "
     (success (xd (if (> m 375) R) (if (< x 2704) R) R)))

;;; stage 2 parses handles the simple strings like {x=1977,m=955,a=492,s=199}
#;34> (test-st2)
((s= "{x=674,m=504,a=708,s=190}"
     ":parse-> "
     (success got ((x 674) (m 504) (a 708) (s 190))))
 (s= "{x=1977,m=955,a=492,s=199}"
     ":parse-> "
     (success got ((x 1977) (m 955) (a 492) (s 199))))
 (s= "{x=973,m=239,a=1114,s=895}"
     ":parse-> "
     (success got ((x 973) (m 239) (a 1114) (s 895))))


|#
(define get-input-stage-1 #f)
(define get-input-stage-2 #f)

(define *output-file* #f)

;; choose one below to be true #t , all rest false #f
(define *real-problem* #t)
(define *example-1* #f)



(when *real-problem*
  (set! *output-file* "real-cogen.scm")
  (set! get-input-stage-1
    (lambda ()
      (let* ((port (open-input-file "lisp-input-stage-1.scm"))
	     (result (read port)))
	(close-input-port port)
	result)))

  (set! get-input-stage-2
    (lambda ()
      (let* ((port (open-input-file "lisp-input-stage-2.scm"))
	     (result (read port)))
	(close-input-port port)
	result))))


(when *example-1*
  (set! *output-file* "example-cogen.scm")
  (set! get-input-stage-1
    (lambda ()
  (list
   "px{a<2006:qkq,m>2090:A,rfg}"
   "pv{a>1716:R,A}"
   "lnx{m>1548:A,A}"
   "rfg{s<537:gd,x>2440:R,A}"
   "qs{s>3448:A,lnx}"
   "qkq{x<1416:A,crn}"
   "crn{x>2662:A,R}"
   "in{s<1351:px,qqz}"
   "qqz{s>2770:qs,m<1801:hdj,R}"
   "gd{a>3333:R,R}"
   "hdj{m>838:A,pv}"
   )))

  (set! get-input-stage-2
    (lambda ()
  (list 
   "{x=787,m=2655,a=1222,s=2876}"
   "{x=1679,m=44,a=2067,s=496}"
   "{x=2036,m=264,a=79,s=2244}"
   "{x=2461,m=1339,a=466,s=291}"
   "{x=2127,m=1623,a=2188,s=1013}"
   ))))

;; one or the other puzzles to solve , not both ! 
(assert (and (or *example-1* *real-problem*)
	     (not (and *example-1* *real-problem*))))


;; characters sequences split by { } , : 
(define (is-lower? ch)
  (and (char>=? ch #\a)
       (char<=? ch #\z)))

(define (is-upper? ch)
  (and (char>=? ch #\A)
       (char<=? ch #\Z)))

(define (is-digit? ch)
  (and (char>=? ch #\0)
       (char<=? ch #\9)))


;; end of string 
(define (parse-end-of-string xs scont fcont)
  (cond
   ((null? xs) (scont "" xs))
   (#t (fcont "nope" xs))))
(define peos parse-end-of-string)

(define (parse-equal-to xs scont fcont)
  (cond
   ((null? xs) (fcont "nope" xs))
   (#t (let ((ch (car xs)))
	 (cond
	  ((char=? ch #\= ) (scont "=" (cdr xs)))
	  (#t (fcont "nope" xs)))))))
(define p= parse-equal-to)


(define (parse-greater-than xs scont fcont)
  (cond
   ((null? xs) (fcont "nope" xs))
   (#t (let ((ch (car xs)))
	 (cond
	  ((char=? ch #\> ) (scont ">" (cdr xs)))
	  (#t (fcont "nope" xs)))))))
(define p> parse-greater-than)


(define (parse-less-than xs scont fcont)
  (cond
   ((null? xs) (fcont "nope" xs))
   (#t (let ((ch (car xs)))
	 (cond
	  ((char=? ch #\< ) (scont "<" (cdr xs)))
	  (#t (fcont "nope" xs)))))))
(define p< parse-less-than)


(define (parse-colon xs scont fcont)
  (cond
   ((null? xs) (fcont "nope" xs))
   (#t (let ((ch (car xs)))
	 (cond
	  ((char=? ch #\: ) (scont ":" (cdr xs)))
	  (#t (fcont "nope" xs)))))))
(define pcol parse-colon)


(define (parse-semicolon xs scont fcont)
  (cond
   ((null? xs) (fcont "nope" xs))
   (#t (let ((ch (car xs)))
	 (cond
	  ((char=? ch #\; ) (scont ";" (cdr xs)))
	  (#t (fcont "nope" xs)))))))
(define psemi parse-semicolon)


(define (parse-comma xs scont fcont)
  (cond
   ((null? xs) (fcont "nope" xs))
   (#t (let ((ch (car xs)))
	 (cond
	  ((char=? ch #\, ) (scont "," (cdr xs)))
	  (#t (fcont "nope" xs)))))))
(define pcom parse-comma)


(define (parse-open-bracket xs scont fcont)
  (cond
   ((null? xs) (fcont "nope" xs))
   (#t (let ((ch (car xs)))
	 (cond
	  ((char=? ch #\{ ) (scont "{" (cdr xs)))
	  (#t (fcont "nope" xs)))))))
(define pob parse-open-bracket)

(define (parse-close-bracket xs scont fcont)
  (cond
   ((null? xs) (fcont "nope" xs))
   (#t (let ((ch (car xs)))
	 (cond
	  ((char=? ch #\} ) (scont "}" (cdr xs)))
	  (#t (fcont "nope" xs)))))))
(define pcb parse-close-bracket)


(define (parse-number-helper xs acc scont fcont)
  (cond
   ;;((null? acc) (fcont "no word" xs))
   ((null? xs) (scont (list->string (reverse acc)) xs))
   (#t (let ((ch (car xs)))
	 (cond
	  ((is-digit? ch) (parse-number-helper (cdr xs) (cons ch acc) scont fcont))
	  ((null? acc) (fcont "no digits" xs))
	  (#t (scont (list->string (reverse acc)) xs)))))))

;; just after successfully parsing a number - convert it to actual int using string->number
(define (parse-number xs scont fcont)
  (let ((scont2 (lambda (snum xs2)
		  (scont (string->number snum) xs2))))
  (parse-number-helper xs '() scont2 fcont)))
(define pnum parse-number)


(define (parse-word-helper xs acc scont fcont)
  (cond
   ;;((null? acc) (fcont "no word" xs))
   ((null? xs) (scont (list->string (reverse acc)) xs))
   (#t (let ((ch (car xs)))
	 (cond
	  ((is-lower? ch) (parse-word-helper (cdr xs) (cons ch acc) scont fcont))
	  ((is-upper? ch) (parse-word-helper (cdr xs) (cons ch acc) scont fcont))
	  ((null? acc) (fcont "no word" xs))
	  (#t (scont (list->string (reverse acc)) xs)))))))

;; when we parse word string - convert to actual lisp symbol and continue 
(define (parse-word xs scont fcont)
  (let ((scont2 (lambda (w xs)
		  (scont (string->symbol w) xs))))
  (parse-word-helper xs '() scont2 fcont)))
(define pw parse-word)

(define (parse-stage1 xs scont fcont)
  (pw xs scont fcont))

(define (parse xs scont fcont)
  (parse-stage1 xs scont fcont))
(define p parse)

(define sl string->list)
        
;; a  >  3414   :    A  
;; pw p> pnum  pcol  pw
(define (pw>n xs scont fcont)
  (let ((fail (lambda (m ys) (pw<n xs scont fcont))))
    (pw xs (lambda (w1 xs2)
    (p> xs2 (lambda (a> xs3)
    (pnum xs3 (lambda (n1 xs4)		      
    (pcol xs4 (lambda (bcol xs5)
    (pw xs5 (lambda (w2 xs6)
		 (scont `(if (> ,w1 ,n1) ,w2) xs6))fail))fail))fail))fail))fail)))


;; a  <  3414   :    A  
;; pw p< pnum  pcol  pw
(define (pw<n xs scont fcont)
  (let ((fail (lambda (m ys) (pw xs scont fcont))))
    (pw xs (lambda (w1 xs2)
    (p< xs2 (lambda (a< xs3)
    (pnum xs3 (lambda (n1 xs4)		      
    (pcol xs4 (lambda (bcol xs5)
    (pw xs5 (lambda (w2 xs6)
	      (scont `(if (< ,w1 ,n1) ,w2) xs6)) fail)) fail)) fail)) fail))fail)))


(define (pany xs scont fcont)
  (let ((fail (lambda (m ys) (pw xs scont fcont))))
    (pw>n xs (lambda (w1 xs2) ;; a > N : A  // a < N : A // a 
	       (pcom xs2 (lambda (_ xs3)
			   (pany xs3 (lambda (ws xs4)
				       (cond
					((symbol? ws)
					 (scont (cons w1 (list ws)) xs4))
					(#t 
					 (scont (cons w1 ws) xs4))))
				 fail))
		     (lambda (_ xs4)
		       (pcb xs2 (lambda (_ xs3)
				  (scont w1 xs2))
			    fail))))
	  fail)))



;; any number of pw<n or pw>n followed by pw 
;;  ,comma  
;; pcom

;; pw 


;; stage 1 parser .... how join continuations sequentially ? huh ...
(define (stage-1 str)
  (let ((xs (sl str))
	(fail (lambda (m ys) (list 'fail m ys))))
    (pw xs
	(lambda (w xs2) ;; word -> open bracket .... close bracket end-of-string
	  (pob xs2 (lambda (w2 xs3)
		     (pany xs3
			   (lambda (w3 xs4)
			     (pcb xs4 (lambda (w5 xs5)
					(let ((result (cons w w3)))
					(cond
					 ((null? xs5)
					  (format #t "~a~%" (process-cogen result))
					  (list 'success result))
					 (#t
					  (list 'fail 'got result 'remainder xs5)))))
				  fail))
			   fail))
	       fail))
	fail)))



(define st1 stage-1)

;; stage 2 parser needs to handle {x=674,m=504,a=708,s=190}
(define (stage-2 str)
  (let ((xs (sl str))
	(fail (lambda (m ys) (list 'fail m ys))))
    ;; open bracket
    (pob xs  (lambda (_ xs2) 
    (pw xs2  (lambda (w1 xs3)
    (p= xs3  (lambda (_ xs4)
    (pnum xs4  (lambda (n1 xs5)
    (pcom xs5  (lambda (_ xs6)
    (pw xs6  (lambda (w2 xs7)
    (p= xs7  (lambda (_ xs8)
    (pnum xs8  (lambda (n2 xs9)
    (pcom xs9  (lambda (_ xs10)
    (pw xs10  (lambda (w3 xs11)
    (p= xs11  (lambda (_ xs12)
    (pnum xs12  (lambda (n3 xs13)
    (pcom xs13  (lambda (_ xs14)
    (pw xs14  (lambda (w4 xs15)
    (p= xs15  (lambda (_ xs16)
    (pnum xs16  (lambda (n4 xs17)
    (pcb xs17  (lambda (_ xs18) 
    (peos xs18 (lambda (_ xs19)
		 (cond
		  ((null? xs19)
		   ;; x m a s  --- in that order , get it  ...
		   (assert (and (eq? w1 'x) (eq? w2 'm) (eq? w3 'a) (eq? w4 's)))
		   (pp
		    `(begin
		       (newline)
		       (when (eq? 'accept (xmas-in ,n1 ,n2 ,n3 ,n4))
			 (let ((tot (+ ,n1 ,n2 ,n3 ,n4)))
			   (display "ACCEPT : ")
			   (display tot)
			   (set! total (+ total tot))))))			      
		   (list 'success 'got `((,w1 ,n1)(,w2 ,n2)(,w3 ,n3)(,w4 ,n4)))
		   )
		  (#t
		   (list 'fail 'got `((,w1 ,n1)(,w2 ,n2)(,w3 ,n3)(,w4 ,n4)) 'remainder xs19))))
	  fail))fail))fail))fail))fail))fail))fail))fail))fail))fail))fail))fail))fail))
	  fail))fail))fail))fail))fail)))



(define st2 stage-2)

(define (test-st1)
  (pp (map (lambda (s) (list 's= s ":parse-> " (st1 s))) (get-input-stage-1))))

(define (test-st2)
  (pp (map (lambda (s) (list 's= s ":parse-> " (st2 s))) (get-input-stage-2))))

;;(test-st2)


#|
all parts start with in

    {x=787,m=2655,a=1222,s=2876}: in -> qqz -> qs -> lnx -> A
    {x=1679,m=44,a=2067,s=496}: in -> px -> rfg -> gd -> R
    {x=2036,m=264,a=79,s=2244}: in -> qqz -> hdj -> pv -> A
    {x=2461,m=1339,a=466,s=291}: in -> px -> qkq -> crn -> R
    {x=2127,m=1623,a=2188,s=1013}: in -> px -> rfg -> A

Ultimately, three parts are accepted. Adding up the x, m, a, and s
rating for each of the accepted parts gives 7540 for the part with
x=787, 4623 for the part with x=2036, and 6951 for the part with
x=2127. Adding all of the ratings for all of the accepted parts gives
the sum total of 19114.

|#

(define totals '())

(define (process-part x m a s)
  (let ((outcome (xmas-in x m a s)))
    (cond
     ((eq? outcome 'accept) (set! totals (cons (list (+ x m a s) x m a s) totals))))))

;; accept-reject-chrimastize a symbol
(define (symbol-xmas s)
  (string->symbol (format #f "xmas-~a" s)))

(define (symbol-arc s)
  (cond
   ((eq? s 'A) `(quote accept))
   ((eq? s 'R) `(quote reject))
   (#t `(,(symbol-xmas s) x m a s))))


;; prefix procedure names with xmas- 
;; entry point of all parts is xmas-in 
(define (process-cogen expr)
  (let ((name (first expr)))
    (letrec ((foo (lambda (xs)
		    (let ((ex (car xs)))
		      (cond
		       ;; last thing - call 
		       ((and (symbol? ex) (null? (cdr xs)))
			(symbol-arc ex))
		       ;; if expr
		       ((and (pair? ex) (eq? (car ex) 'if))
			`(if ,(cadr ex) ,(symbol-arc (caddr ex))
			     ,(foo (cdr xs))))
		       (#t
			(error "no matching expression")))))))
      (let ((dname (string-append "\"" (symbol->string name) "\"")))
      `(define ,(symbol-xmas name) (lambda (x m a s)
				     (display ,dname)
				     (display "\" -> \"")
				     ,(foo (cdr expr))))))))







(define (run)
  (with-output-to-file  *output-file*
    (lambda ()
      (let ((r (map st1 (get-input-stage-1))))
	(format #t "(define (run) 
		      (let ((total 0))
                   ")
	(map st2 (get-input-stage-2))
	(format #t "
          (newline)
          (display \"results : \")
          (display (list 'total total))
          (newline)
                    ))
         (run)
        ")
	))))
	
	
  ;; ;;(let ((r (map st1 (get-input-stage-1)))) #t)
  ;; (set! totals '())
  ;; (map st2 (get-input-stage-2))
  ;; totals)



;; code-generator + run + 

#|

selecting *real-problem* #t
>(run)

csc -O3 real-cogen.scm
./real-cogen

...
... 
in -> js -> bld -> ql -> kpc -> kgj -> ACCEPT : 1906
in -> js -> bld -> ql -> lmm -> qpv -> cmf -> 
in -> ktm -> dbh -> cj -> lg -> qlv -> rpb -> ACCEPT : 6666
in -> js -> bld -> ql -> lmm -> rk -> 
in -> js -> bld -> ql -> lmm -> rk -> mqt -> ACCEPT : 4389
in -> js -> bld -> ql -> lmm -> rk -> mqt -> 
in -> bz -> svg -> bm -> cmb -> kbc -> ACCEPT : 5199
results : (total 376008)


|#

