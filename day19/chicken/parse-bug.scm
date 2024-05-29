
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
9) when copied generated code into parse.scm file , run map over st2
got a ridiculously large answer for the puzzle ...
... lets see if can recreate bug .....

10 ) vital record results as go along , as trying to recreate a bug is sometimes not possible
- if knew what did wrong - would not do it -
- so did not know it was wrong - did it anyway ...



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
		   ;; 
		   (process-part n1 n2 n3 n4)
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


(define (run2)
  ;;(let ((r (map st1 (get-input-stage-1)))) #t)
  (set! totals '())
  (map st2 (get-input-stage-2))
  totals)




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

(define xmas-vr (lambda (x m a s) (display "vr") (display " -> ") (if (> a 3414) (quote accept) (quote reject))))
(define xmas-lx (lambda (x m a s) (display "lx") (display " -> ") (if (> x 2140) (quote accept) (quote accept))))
(define xmas-vkm (lambda (x m a s) (display "vkm") (display " -> ") (if (> s 411) (quote accept) (if (> a 443) (quote reject) (quote accept)))))
(define xmas-nr (lambda (x m a s) (display "nr") (display " -> ") (if (> x 1463) (quote reject) (quote accept))))
(define xmas-xd (lambda (x m a s) (display "xd") (display " -> ") (if (> m 375) (quote reject) (if (< x 2704) (quote reject) (quote reject)))))
(define xmas-zq (lambda (x m a s) (display "zq") (display " -> ") (if (< s 2226) (quote accept) (quote reject))))
(define xmas-sgf (lambda (x m a s) (display "sgf") (display " -> ") (if (> m 923) (xmas-kq x m a s) (if (< a 801) (xmas-sc x m a s) (if (< x 3251) (xmas-tt x m a s) (xmas-gm x m a s))))))
(define xmas-pqh (lambda (x m a s) (display "pqh") (display " -> ") (if (> m 3259) (quote reject) (if (> a 1418) (quote reject) (if (< s 3015) (quote accept) (quote reject))))))
(define xmas-cg (lambda (x m a s) (display "cg") (display " -> ") (if (< m 3220) (quote reject) (if (< m 3292) (quote accept) (if (> a 856) (quote reject) (quote reject))))))
(define xmas-lsf (lambda (x m a s) (display "lsf") (display " -> ") (if (> a 424) (quote reject) (quote reject))))
(define xmas-cbn (lambda (x m a s) (display "cbn") (display " -> ") (if (> m 2375) (xmas-pvh x m a s) (xmas-qgf x m a s))))
(define xmas-tl (lambda (x m a s) (display "tl") (display " -> ") (if (< x 3706) (xmas-mf x m a s) (if (< x 3839) (xmas-srt x m a s) (if (< s 2265) (xmas-vdg x m a s) (xmas-fjb x m a s))))))
(define xmas-rlp (lambda (x m a s) (display "rlp") (display " -> ") (if (< s 657) (quote reject) (if (< a 2605) (quote accept) (if (> a 2875) (quote accept) (quote accept))))))
(define xmas-hgt (lambda (x m a s) (display "hgt") (display " -> ") (if (> s 3010) (quote accept) (quote accept))))
(define xmas-lbp (lambda (x m a s) (display "lbp") (display " -> ") (if (< a 1791) (quote reject) (if (> x 2941) (xmas-dbs x m a s) (if (> s 3208) (quote accept) (xmas-sxh x m a s))))))
(define xmas-dc (lambda (x m a s) (display "dc") (display " -> ") (if (< a 559) (quote reject) (quote reject))))
(define xmas-pqm (lambda (x m a s) (display "pqm") (display " -> ") (if (< m 1994) (quote reject) (quote accept))))
(define xmas-kg (lambda (x m a s) (display "kg") (display " -> ") (if (< x 2866) (xmas-zv x m a s) (if (< a 532) (xmas-mfx x m a s) (xmas-dc x m a s)))))
(define xmas-zz (lambda (x m a s) (display "zz") (display " -> ") (if (< x 204) (quote accept) (if (> m 3750) (quote accept) (xmas-ct x m a s)))))
(define xmas-mq (lambda (x m a s) (display "mq") (display " -> ") (if (< s 3238) (xmas-xzl x m a s) (xmas-qfs x m a s))))
(define xmas-csg (lambda (x m a s) (display "csg") (display " -> ") (if (< m 1938) (quote accept) (if (< m 1995) (quote reject) (quote accept)))))
(define xmas-dr (lambda (x m a s) (display "dr") (display " -> ") (if (> a 3338) (xmas-fl x m a s) (if (> a 3276) (xmas-vc x m a s) (quote accept)))))
(define xmas-gc (lambda (x m a s) (display "gc") (display " -> ") (if (> m 3488) (quote accept) (if (< m 2931) (quote reject) (if (< x 1506) (quote reject) (quote reject))))))
(define xmas-pqx (lambda (x m a s) (display "pqx") (display " -> ") (if (< x 1518) (quote reject) (if (< x 1605) (quote reject) (if (< s 3006) (quote reject) (quote reject))))))
(define xmas-cc (lambda (x m a s) (display "cc") (display " -> ") (if (> a 1777) (xmas-pdz x m a s) (if (> s 358) (xmas-nfp x m a s) (xmas-rd x m a s)))))
(define xmas-cmb (lambda (x m a s) (display "cmb") (display " -> ") (if (< m 758) (xmas-kbc x m a s) (if (> a 2913) (xmas-vdt x m a s) (if (> a 2448) (xmas-dm x m a s) (xmas-kv x m a s))))))
(define xmas-xq (lambda (x m a s) (display "xq") (display " -> ") (if (< m 3071) (xmas-tlf x m a s) (if (< s 1142) (xmas-vdf x m a s) (if (> s 1530) (xmas-gv x m a s) (xmas-bbc x m a s))))))
(define xmas-cn (lambda (x m a s) (display "cn") (display " -> ") (if (> m 919) (xmas-slj x m a s) (if (> a 3064) (xmas-fr x m a s) (xmas-zll x m a s)))))
(define xmas-cfd (lambda (x m a s) (display "cfd") (display " -> ") (if (> m 1470) (quote accept) (if (> s 2179) (quote accept) (if (< m 840) (xmas-hg x m a s) (xmas-zcf x m a s))))))
(define xmas-dm (lambda (x m a s) (display "dm") (display " -> ") (if (> a 2757) (xmas-mmz x m a s) (if (> m 1033) (xmas-xz x m a s) (if (> a 2638) (xmas-gbh x m a s) (quote reject))))))
(define xmas-rrm (lambda (x m a s) (display "rrm") (display " -> ") (if (> x 716) (xmas-xg x m a s) (if (< x 586) (xmas-dzv x m a s) (xmas-mjb x m a s)))))
(define xmas-xhl (lambda (x m a s) (display "xhl") (display " -> ") (if (> m 3908) (quote reject) (if (> a 1503) (quote accept) (if (> a 1326) (quote reject) (quote accept))))))
(define xmas-zg (lambda (x m a s) (display "zg") (display " -> ") (if (> m 710) (quote accept) (if (< m 549) (quote reject) (quote reject)))))
(define xmas-pls (lambda (x m a s) (display "pls") (display " -> ") (if (> s 1189) (xmas-mv x m a s) (if (< a 1598) (xmas-zs x m a s) (if (> s 762) (xmas-ms x m a s) (xmas-cc x m a s))))))
(define xmas-tps (lambda (x m a s) (display "tps") (display " -> ") (if (< a 940) (xmas-mj x m a s) (if (< x 3337) (quote accept) (xmas-sm x m a s)))))
(define xmas-lvq (lambda (x m a s) (display "lvq") (display " -> ") (if (< x 1898) (quote reject) (if (< s 2829) (quote accept) (if (> m 3485) (quote accept) (quote reject))))))
(define xmas-mqt (lambda (x m a s) (display "mqt") (display " -> ") (if (> m 1462) (quote reject) (quote accept))))
(define xmas-in (lambda (x m a s) (display "in") (display " -> ") (if (> a 1983) (xmas-bz x m a s) (if (< m 1782) (xmas-js x m a s) (if (> m 2851) (xmas-hl x m a s) (xmas-ktm x m a s))))))
(define xmas-gx (lambda (x m a s) (display "gx") (display " -> ") (if (< s 1066) (quote accept) (quote reject))))
(define xmas-sv (lambda (x m a s) (display "sv") (display " -> ") (if (< x 2026) (quote reject) (if (> x 2775) (xmas-bqt x m a s) (xmas-nc x m a s)))))
(define xmas-fsg (lambda (x m a s) (display "fsg") (display " -> ") (if (< a 152) (quote accept) (if (< s 3348) (quote reject) (quote reject)))))
(define xmas-zrk (lambda (x m a s) (display "zrk") (display " -> ") (if (> a 875) (quote accept) (if (< m 416) (quote reject) (quote reject)))))
(define xmas-tmx (lambda (x m a s) (display "tmx") (display " -> ") (if (> s 3571) (quote reject) (if (> m 238) (quote reject) (quote reject)))))
(define xmas-btg (lambda (x m a s) (display "btg") (display " -> ") (if (> x 3104) (quote accept) (quote reject))))
(define xmas-fj (lambda (x m a s) (display "fj") (display " -> ") (if (< x 1633) (xmas-ltm x m a s) (xmas-kgs x m a s))))
(define xmas-bs (lambda (x m a s) (display "bs") (display " -> ") (if (< a 1535) (quote reject) (quote reject))))
(define xmas-kdr (lambda (x m a s) (display "kdr") (display " -> ") (if (> x 1967) (quote reject) (quote reject))))
(define xmas-vbl (lambda (x m a s) (display "vbl") (display " -> ") (if (< a 600) (xmas-mpb x m a s) (if (> s 1964) (xmas-zr x m a s) (xmas-ptr x m a s)))))
(define xmas-crk (lambda (x m a s) (display "crk") (display " -> ") (if (> a 3107) (xmas-tz x m a s) (if (> x 801) (quote reject) (quote accept)))))
(define xmas-lc (lambda (x m a s) (display "lc") (display " -> ") (if (> m 3491) (xmas-kr x m a s) (xmas-xq x m a s))))
(define xmas-kqx (lambda (x m a s) (display "kqx") (display " -> ") (if (> m 2976) (xmas-kdd x m a s) (if (> m 2909) (xmas-nps x m a s) (if (< a 1520) (xmas-bv x m a s) (xmas-cts x m a s))))))
(define xmas-lv (lambda (x m a s) (display "lv") (display " -> ") (if (< a 1310) (quote accept) (if (> x 2652) (quote accept) (quote accept)))))
(define xmas-zxs (lambda (x m a s) (display "zxs") (display " -> ") (if (< m 875) (quote accept) (xmas-ccd x m a s))))
(define xmas-szl (lambda (x m a s) (display "szl") (display " -> ") (if (> s 466) (xmas-kjx x m a s) (if (< a 454) (xmas-jcc x m a s) (quote accept)))))
(define xmas-ghm (lambda (x m a s) (display "ghm") (display " -> ") (if (> s 833) (quote reject) (quote reject))))
(define xmas-fzt (lambda (x m a s) (display "fzt") (display " -> ") (if (< x 1870) (xmas-psf x m a s) (if (< a 1621) (xmas-gr x m a s) (if (< m 3600) (xmas-lvq x m a s) (xmas-ddt x m a s))))))
(define xmas-hqv (lambda (x m a s) (display "hqv") (display " -> ") (if (< s 948) (quote accept) (if (< a 507) (quote accept) (if (< a 738) (xmas-szr x m a s) (quote accept))))))
(define xmas-fmj (lambda (x m a s) (display "fmj") (display " -> ") (if (< m 2475) (quote reject) (if (< s 2910) (quote accept) (if (> a 251) (quote accept) (quote reject))))))
(define xmas-zsg (lambda (x m a s) (display "zsg") (display " -> ") (if (> a 2629) (quote reject) (quote accept))))
(define xmas-cgx (lambda (x m a s) (display "cgx") (display " -> ") (if (< x 1207) (xmas-jv x m a s) (xmas-kqh x m a s))))
(define xmas-lt (lambda (x m a s) (display "lt") (display " -> ") (if (< a 3161) (quote accept) (if (< m 3426) (quote accept) (if (< x 2128) (quote accept) (quote reject))))))
(define xmas-qsz (lambda (x m a s) (display "qsz") (display " -> ") (if (> m 3289) (quote reject) (if (> x 2566) (xmas-xf x m a s) (quote reject)))))
(define xmas-pc (lambda (x m a s) (display "pc") (display " -> ") (if (< s 3779) (quote reject) (if (< m 783) (xmas-btp x m a s) (xmas-mh x m a s)))))
(define xmas-vt (lambda (x m a s) (display "vt") (display " -> ") (if (> x 3274) (quote reject) (if (< x 2529) (quote reject) (xmas-hqd x m a s)))))
(define xmas-rk (lambda (x m a s) (display "rk") (display " -> ") (if (< a 639) (quote reject) (xmas-mqt x m a s))))
(define xmas-xdl (lambda (x m a s) (display "xdl") (display " -> ") (if (< m 3533) (quote reject) (quote reject))))
(define xmas-vbp (lambda (x m a s) (display "vbp") (display " -> ") (if (> s 3091) (xmas-zrk x m a s) (if (> x 2865) (quote accept) (xmas-qz x m a s)))))
(define xmas-nx (lambda (x m a s) (display "nx") (display " -> ") (if (> s 3353) (quote reject) (xmas-fmj x m a s))))
(define xmas-hmg (lambda (x m a s) (display "hmg") (display " -> ") (if (< m 2202) (quote accept) (quote reject))))
(define xmas-gv (lambda (x m a s) (display "gv") (display " -> ") (if (< x 1581) (quote reject) (if (< s 1739) (quote accept) (if (> x 1695) (xmas-dzj x m a s) (quote reject))))))
(define xmas-mfq (lambda (x m a s) (display "mfq") (display " -> ") (if (< s 1846) (quote accept) (if (> s 2187) (quote accept) (quote reject)))))
(define xmas-zl (lambda (x m a s) (display "zl") (display " -> ") (if (< a 3740) (xmas-qtg x m a s) (quote accept))))
(define xmas-sr (lambda (x m a s) (display "sr") (display " -> ") (if (> m 1500) (xmas-rcj x m a s) (xmas-qnb x m a s))))
(define xmas-kpc (lambda (x m a s) (display "kpc") (display " -> ") (if (> a 741) (xmas-gk x m a s) (xmas-kgj x m a s))))
(define xmas-prc (lambda (x m a s) (display "prc") (display " -> ") (if (> m 3259) (quote reject) (if (< a 1739) (quote accept) (if (> a 1851) (quote accept) (quote reject))))))
(define xmas-sqj (lambda (x m a s) (display "sqj") (display " -> ") (if (> x 1393) (xmas-pqx x m a s) (if (< x 1312) (xmas-rs x m a s) (if (> x 1340) (quote reject) (quote reject))))))
(define xmas-dbh (lambda (x m a s) (display "dbh") (display " -> ") (if (< x 2248) (xmas-cj x m a s) (xmas-qx x m a s))))
(define xmas-zk (lambda (x m a s) (display "zk") (display " -> ") (if (> s 1944) (quote accept) (if (< x 2918) (quote accept) (quote accept)))))
(define xmas-bdh (lambda (x m a s) (display "bdh") (display " -> ") (if (> x 3416) (quote reject) (quote accept))))
(define xmas-bfj (lambda (x m a s) (display "bfj") (display " -> ") (if (> a 2398) (quote reject) (if (< a 2234) (quote accept) (quote reject)))))
(define xmas-qkl (lambda (x m a s) (display "qkl") (display " -> ") (if (< x 1740) (xmas-pp x m a s) (if (< a 435) (xmas-xx x m a s) (xmas-mk x m a s)))))
(define xmas-bb (lambda (x m a s) (display "bb") (display " -> ") (if (> m 935) (quote reject) (if (> m 508) (quote accept) (if (> x 2922) (quote accept) (quote reject))))))
(define xmas-skb (lambda (x m a s) (display "skb") (display " -> ") (if (> s 1570) (quote accept) (if (< m 3271) (quote accept) (if (> x 3470) (quote accept) (quote reject))))))
(define xmas-zxh (lambda (x m a s) (display "zxh") (display " -> ") (if (< m 3566) (quote accept) (quote reject))))
(define xmas-zhx (lambda (x m a s) (display "zhx") (display " -> ") (if (< x 1421) (xmas-xv x m a s) (quote accept))))
(define xmas-pd (lambda (x m a s) (display "pd") (display " -> ") (if (< m 3229) (quote accept) (quote reject))))
(define xmas-gs (lambda (x m a s) (display "gs") (display " -> ") (if (> m 3374) (xmas-vk x m a s) (if (< m 3046) (xmas-kqx x m a s) (xmas-kt x m a s)))))
(define xmas-jjr (lambda (x m a s) (display "jjr") (display " -> ") (if (> x 2310) (xmas-lmr x m a s) (xmas-jdq x m a s))))
(define xmas-hd (lambda (x m a s) (display "hd") (display " -> ") (if (> s 3385) (quote reject) (if (> a 1113) (quote reject) (if (> x 314) (quote reject) (quote accept))))))
(define xmas-vfj (lambda (x m a s) (display "vfj") (display " -> ") (if (< m 1168) (xmas-xql x m a s) (xmas-fb x m a s))))
(define xmas-dnt (lambda (x m a s) (display "dnt") (display " -> ") (if (< s 3071) (quote accept) (quote accept))))
(define xmas-tjx (lambda (x m a s) (display "tjx") (display " -> ") (if (> a 1534) (quote accept) (quote accept))))
(define xmas-dtm (lambda (x m a s) (display "dtm") (display " -> ") (if (< a 714) (xmas-zhx x m a s) (if (< m 3352) (xmas-zm x m a s) (xmas-slv x m a s)))))
(define xmas-cq (lambda (x m a s) (display "cq") (display " -> ") (if (> a 3174) (quote accept) (quote accept))))
(define xmas-psn (lambda (x m a s) (display "psn") (display " -> ") (if (< a 1284) (xmas-cjj x m a s) (if (> x 1844) (xmas-xn x m a s) (xmas-jqj x m a s)))))
(define xmas-xh (lambda (x m a s) (display "xh") (display " -> ") (if (< a 1317) (quote accept) (if (> x 2843) (quote accept) (quote reject)))))
(define xmas-sgt (lambda (x m a s) (display "sgt") (display " -> ") (if (> s 3564) (quote accept) (quote accept))))
(define xmas-lgz (lambda (x m a s) (display "lgz") (display " -> ") (if (< x 2895) (quote reject) (quote accept))))
(define xmas-fzq (lambda (x m a s) (display "fzq") (display " -> ") (if (> x 2802) (xmas-kf x m a s) (if (> a 605) (quote reject) (quote reject)))))
(define xmas-gps (lambda (x m a s) (display "gps") (display " -> ") (if (< a 822) (quote accept) (if (> a 839) (quote accept) (if (< x 2994) (quote accept) (quote accept))))))
(define xmas-mbq (lambda (x m a s) (display "mbq") (display " -> ") (if (< x 2602) (xmas-vtd x m a s) (quote reject))))
(define xmas-cts (lambda (x m a s) (display "cts") (display " -> ") (if (< m 2887) (quote reject) (quote reject))))
(define xmas-bd (lambda (x m a s) (display "bd") (display " -> ") (if (< m 3096) (quote reject) (quote reject))))
(define xmas-gk (lambda (x m a s) (display "gk") (display " -> ") (if (< a 949) (quote reject) (if (> a 1097) (quote accept) (if (< x 2044) (quote reject) (quote reject))))))
(define xmas-ds (lambda (x m a s) (display "ds") (display " -> ") (if (> s 927) (quote accept) (if (> m 1055) (quote reject) (quote reject)))))
(define xmas-mps (lambda (x m a s) (display "mps") (display " -> ") (if (< x 1131) (quote reject) (quote accept))))
(define xmas-slv (lambda (x m a s) (display "slv") (display " -> ") (if (> x 1497) (xmas-vgh x m a s) (if (< a 983) (xmas-hgt x m a s) (if (< a 1095) (xmas-tmr x m a s) (xmas-bsj x m a s))))))
(define xmas-gdn (lambda (x m a s) (display "gdn") (display " -> ") (if (> x 1376) (quote reject) (quote accept))))
(define xmas-jj (lambda (x m a s) (display "jj") (display " -> ") (if (< m 3028) (quote accept) (if (< x 1518) (quote reject) (quote accept)))))
(define xmas-bg (lambda (x m a s) (display "bg") (display " -> ") (if (> m 3463) (quote reject) (quote accept))))
(define xmas-fq (lambda (x m a s) (display "fq") (display " -> ") (if (< a 873) (quote accept) (quote reject))))
(define xmas-blg (lambda (x m a s) (display "blg") (display " -> ") (if (< x 476) (quote accept) (if (< x 743) (quote reject) (quote accept)))))
(define xmas-gqs (lambda (x m a s) (display "gqs") (display " -> ") (if (> a 1049) (quote accept) (if (> a 1029) (quote reject) (if (< a 1023) (quote reject) (quote accept))))))
(define xmas-szr (lambda (x m a s) (display "szr") (display " -> ") (if (> x 825) (quote reject) (if (< a 642) (quote accept) (if (> a 675) (quote accept) (quote reject))))))
(define xmas-jdq (lambda (x m a s) (display "jdq") (display " -> ") (if (> a 691) (quote reject) (if (< m 2018) (quote reject) (if (> x 1015) (quote accept) (quote accept))))))
(define xmas-jk (lambda (x m a s) (display "jk") (display " -> ") (if (> a 415) (quote accept) (quote accept))))
(define xmas-xvx (lambda (x m a s) (display "xvx") (display " -> ") (if (< m 3391) (quote reject) (if (> a 1694) (quote accept) (quote accept)))))
(define xmas-pb (lambda (x m a s) (display "pb") (display " -> ") (if (> s 2785) (quote reject) (quote reject))))
(define xmas-gf (lambda (x m a s) (display "gf") (display " -> ") (if (< m 1294) (quote accept) (if (< a 1958) (quote reject) (quote reject)))))
(define xmas-kgr (lambda (x m a s) (display "kgr") (display " -> ") (if (> m 2635) (xmas-hrg x m a s) (if (< a 1461) (xmas-zq x m a s) (quote reject)))))
(define xmas-cs (lambda (x m a s) (display "cs") (display " -> ") (if (> x 2439) (quote accept) (if (< a 219) (quote accept) (quote accept)))))
(define xmas-qnb (lambda (x m a s) (display "qnb") (display " -> ") (if (< m 1281) (xmas-tjx x m a s) (if (< a 1711) (quote accept) (xmas-ztp x m a s)))))
(define xmas-zvk (lambda (x m a s) (display "zvk") (display " -> ") (if (< s 1182) (quote reject) (quote reject))))
(define xmas-snd (lambda (x m a s) (display "snd") (display " -> ") (if (< a 680) (quote accept) (if (< a 723) (quote reject) (quote accept)))))
(define xmas-tnt (lambda (x m a s) (display "tnt") (display " -> ") (if (< x 876) (quote reject) (if (< s 3391) (quote accept) (if (< x 1365) (quote reject) (quote accept))))))
(define xmas-ddt (lambda (x m a s) (display "ddt") (display " -> ") (if (< m 3788) (quote accept) (if (> s 2800) (quote reject) (quote reject)))))
(define xmas-zx (lambda (x m a s) (display "zx") (display " -> ") (if (> s 3399) (quote reject) (if (> x 984) (quote reject) (xmas-pfj x m a s)))))
(define xmas-tp (lambda (x m a s) (display "tp") (display " -> ") (if (> a 731) (quote accept) (quote accept))))
(define xmas-ztl (lambda (x m a s) (display "ztl") (display " -> ") (if (> a 1938) (quote accept) (if (< m 676) (quote accept) (if (< x 997) (quote reject) (quote reject))))))
(define xmas-mpb (lambda (x m a s) (display "mpb") (display " -> ") (if (> s 2483) (xmas-kg x m a s) (xmas-zhk x m a s))))
(define xmas-jb (lambda (x m a s) (display "jb") (display " -> ") (if (> s 1027) (quote reject) (quote accept))))
(define xmas-hv (lambda (x m a s) (display "hv") (display " -> ") (if (< x 1034) (quote reject) (xmas-nr x m a s))))
(define xmas-slj (lambda (x m a s) (display "slj") (display " -> ") (if (< s 1035) (quote accept) (if (< s 1462) (xmas-vx x m a s) (if (< s 1617) (xmas-mx x m a s) (quote accept))))))
(define xmas-nxp (lambda (x m a s) (display "nxp") (display " -> ") (if (> x 2789) (xmas-zvk x m a s) (if (> s 1209) (xmas-kd x m a s) (xmas-cs x m a s)))))
(define xmas-mds (lambda (x m a s) (display "mds") (display " -> ") (if (> x 353) (quote reject) (quote reject))))
(define xmas-hl (lambda (x m a s) (display "hl") (display " -> ") (if (< x 1920) (xmas-dlb x m a s) (xmas-xlj x m a s))))
(define xmas-rcj (lambda (x m a s) (display "rcj") (display " -> ") (if (< a 1686) (quote reject) (if (> x 1892) (xmas-bpc x m a s) (if (< s 1510) (quote reject) (xmas-hsl x m a s))))))
(define xmas-sg (lambda (x m a s) (display "sg") (display " -> ") (if (> x 2708) (xmas-rm x m a s) (quote reject))))
(define xmas-kkf (lambda (x m a s) (display "kkf") (display " -> ") (if (< m 1967) (quote reject) (if (< m 1976) (xmas-gxd x m a s) (if (> a 763) (xmas-mfq x m a s) (quote accept))))))
(define xmas-vp (lambda (x m a s) (display "vp") (display " -> ") (if (< m 2517) (xmas-nd x m a s) (if (> x 1859) (xmas-srv x m a s) (if (> s 810) (xmas-pg x m a s) (xmas-szl x m a s))))))
(define xmas-zxf (lambda (x m a s) (display "zxf") (display " -> ") (if (< m 1614) (xmas-ftg x m a s) (if (> m 1724) (quote accept) (quote reject)))))
(define xmas-vqs (lambda (x m a s) (display "vqs") (display " -> ") (if (> m 736) (quote accept) (if (< m 362) (quote reject) (quote accept)))))
(define xmas-tn (lambda (x m a s) (display "tn") (display " -> ") (if (< s 2744) (quote reject) (if (< m 3203) (quote reject) (if (> m 3300) (quote accept) (quote reject))))))
(define xmas-hsd (lambda (x m a s) (display "hsd") (display " -> ") (if (< x 2643) (quote reject) (quote accept))))
(define xmas-tss (lambda (x m a s) (display "tss") (display " -> ") (if (> s 922) (quote reject) (if (< a 1019) (quote accept) (if (> a 1154) (quote accept) (quote reject))))))
(define xmas-tlr (lambda (x m a s) (display "tlr") (display " -> ") (if (< a 3231) (quote reject) (if (> x 2029) (quote accept) (quote accept)))))
(define xmas-qqg (lambda (x m a s) (display "qqg") (display " -> ") (if (< x 2873) (quote reject) (if (> x 3359) (quote reject) (if (> s 3047) (quote accept) (xmas-fq x m a s))))))
(define xmas-tbh (lambda (x m a s) (display "tbh") (display " -> ") (if (> x 1566) (quote accept) (if (> x 1286) (quote reject) (if (> x 1138) (quote reject) (quote accept))))))
(define xmas-dzj (lambda (x m a s) (display "dzj") (display " -> ") (if (> m 3298) (quote accept) (quote accept))))
(define xmas-sbx (lambda (x m a s) (display "sbx") (display " -> ") (if (< m 3341) (quote reject) (if (< a 991) (quote accept) (quote accept)))))
(define xmas-zs (lambda (x m a s) (display "zs") (display " -> ") (if (< a 1423) (quote accept) (if (< a 1498) (xmas-nb x m a s) (quote accept)))))
(define xmas-lgq (lambda (x m a s) (display "lgq") (display " -> ") (if (< a 1200) (xmas-dtm x m a s) (if (< x 1546) (xmas-hp x m a s) (xmas-brm x m a s)))))
(define xmas-ql (lambda (x m a s) (display "ql") (display " -> ") (if (< m 880) (xmas-kpc x m a s) (xmas-lmm x m a s))))
(define xmas-zn (lambda (x m a s) (display "zn") (display " -> ") (if (< s 1750) (quote accept) (if (< x 901) (quote reject) (if (< m 3644) (quote accept) (quote accept))))))
(define xmas-mjs (lambda (x m a s) (display "mjs") (display " -> ") (if (> x 2426) (xmas-fxr x m a s) (if (< s 2233) (quote accept) (if (> s 2497) (quote accept) (quote reject))))))
(define xmas-tbs (lambda (x m a s) (display "tbs") (display " -> ") (if (< x 3370) (quote accept) (if (> a 2833) (quote reject) (if (> m 1245) (quote accept) (quote reject))))))
(define xmas-mp (lambda (x m a s) (display "mp") (display " -> ") (if (> a 3625) (quote accept) (if (< x 819) (quote accept) (quote reject)))))
(define xmas-hjp (lambda (x m a s) (display "hjp") (display " -> ") (if (< x 429) (quote accept) (if (> x 774) (quote accept) (quote reject)))))
(define xmas-dzv (lambda (x m a s) (display "dzv") (display " -> ") (if (< m 3698) (quote reject) (if (< a 837) (xmas-ztm x m a s) (xmas-ltt x m a s)))))
(define xmas-bqt (lambda (x m a s) (display "bqt") (display " -> ") (if (< x 3520) (quote accept) (quote reject))))
(define xmas-bn (lambda (x m a s) (display "bn") (display " -> ") (if (< m 3802) (quote reject) (if (< x 278) (quote accept) (if (< x 324) (quote accept) (quote reject))))))
(define xmas-mkq (lambda (x m a s) (display "mkq") (display " -> ") (if (> m 321) (quote reject) (if (> s 686) (quote reject) (if (< s 253) (quote accept) (quote reject))))))
(define xmas-tz (lambda (x m a s) (display "tz") (display " -> ") (if (> x 883) (quote reject) (if (> m 3465) (quote accept) (quote reject)))))
(define xmas-rm (lambda (x m a s) (display "rm") (display " -> ") (if (< m 683) (quote reject) (if (< a 1520) (quote reject) (quote accept)))))
(define xmas-gcp (lambda (x m a s) (display "gcp") (display " -> ") (if (> s 3097) (quote accept) (xmas-pb x m a s))))
(define xmas-kp (lambda (x m a s) (display "kp") (display " -> ") (if (< a 632) (xmas-bt x m a s) (if (> a 666) (xmas-qmk x m a s) (if (< a 651) (xmas-xd x m a s) (xmas-lzh x m a s))))))
(define xmas-jg (lambda (x m a s) (display "jg") (display " -> ") (if (< s 2959) (quote accept) (quote accept))))
(define xmas-mg (lambda (x m a s) (display "mg") (display " -> ") (if (> x 1316) (xmas-xdl x m a s) (xmas-xvx x m a s))))
(define xmas-qp (lambda (x m a s) (display "qp") (display " -> ") (if (> a 3467) (quote reject) (quote accept))))
(define xmas-fp (lambda (x m a s) (display "fp") (display " -> ") (if (< x 1546) (quote reject) (quote accept))))
(define xmas-sp (lambda (x m a s) (display "sp") (display " -> ") (if (> a 3712) (quote accept) (if (< s 1594) (quote reject) (quote reject)))))
(define xmas-vjs (lambda (x m a s) (display "vjs") (display " -> ") (if (> m 2044) (quote accept) (quote accept))))
(define xmas-zc (lambda (x m a s) (display "zc") (display " -> ") (if (> a 2110) (quote accept) (if (> s 243) (quote accept) (if (> a 2028) (quote accept) (quote accept))))))
(define xmas-nk (lambda (x m a s) (display "nk") (display " -> ") (if (< m 1119) (quote accept) (if (> m 1895) (quote accept) (if (> m 1627) (quote accept) (quote reject))))))
(define xmas-vk (lambda (x m a s) (display "vk") (display " -> ") (if (< a 1458) (xmas-zj x m a s) (if (> s 1515) (xmas-bf x m a s) (xmas-rb x m a s)))))
(define xmas-fz (lambda (x m a s) (display "fz") (display " -> ") (if (< m 513) (quote reject) (if (< a 685) (xmas-pvs x m a s) (if (> m 702) (xmas-sst x m a s) (xmas-zrt x m a s))))))
(define xmas-qc (lambda (x m a s) (display "qc") (display " -> ") (if (> x 626) (quote reject) (if (> a 446) (quote reject) (quote reject)))))
(define xmas-pvz (lambda (x m a s) (display "pvz") (display " -> ") (if (< m 924) (quote accept) (if (> m 1466) (quote accept) (quote reject)))))
(define xmas-xql (lambda (x m a s) (display "xql") (display " -> ") (if (> x 3205) (xmas-nt x m a s) (xmas-dnt x m a s))))
(define xmas-hk (lambda (x m a s) (display "hk") (display " -> ") (if (> s 901) (xmas-nvv x m a s) (xmas-sx x m a s))))
(define xmas-shp (lambda (x m a s) (display "shp") (display " -> ") (if (> a 552) (xmas-lrf x m a s) (if (< x 2379) (xmas-cd x m a s) (xmas-fhn x m a s)))))
(define xmas-gqb (lambda (x m a s) (display "gqb") (display " -> ") (if (< x 2096) (xmas-rx x m a s) (xmas-zxh x m a s))))
(define xmas-hgv (lambda (x m a s) (display "hgv") (display " -> ") (if (< x 2779) (quote reject) (if (> s 1337) (quote accept) (quote reject)))))
(define xmas-kz (lambda (x m a s) (display "kz") (display " -> ") (if (< a 3763) (xmas-sp x m a s) (xmas-lp x m a s))))
(define xmas-jmc (lambda (x m a s) (display "jmc") (display " -> ") (if (> a 932) (xmas-psn x m a s) (if (< x 1954) (xmas-cgx x m a s) (if (> a 579) (xmas-sgf x m a s) (xmas-vfj x m a s))))))
(define xmas-ptr (lambda (x m a s) (display "ptr") (display " -> ") (if (> m 3398) (xmas-cks x m a s) (if (< m 3105) (xmas-hr x m a s) (xmas-dh x m a s)))))
(define xmas-vq (lambda (x m a s) (display "vq") (display " -> ") (if (> a 1722) (quote accept) (if (< s 3208) (quote reject) (if (> s 3621) (quote accept) (quote reject))))))
(define xmas-jm (lambda (x m a s) (display "jm") (display " -> ") (if (< x 1649) (quote reject) (if (> a 1786) (quote accept) (if (< x 1723) (quote reject) (quote reject))))))
(define xmas-sx (lambda (x m a s) (display "sx") (display " -> ") (if (> x 1431) (quote reject) (if (< x 659) (quote reject) (quote reject)))))
(define xmas-rjz (lambda (x m a s) (display "rjz") (display " -> ") (if (> a 2733) (quote reject) (if (< s 3280) (quote reject) (quote reject)))))
(define xmas-ns (lambda (x m a s) (display "ns") (display " -> ") (if (> m 3790) (quote accept) (quote accept))))
(define xmas-vz (lambda (x m a s) (display "vz") (display " -> ") (if (> m 2278) (xmas-kgr x m a s) (xmas-hvp x m a s))))
(define xmas-xnx (lambda (x m a s) (display "xnx") (display " -> ") (if (< s 1150) (quote reject) (quote reject))))
(define xmas-xx (lambda (x m a s) (display "xx") (display " -> ") (if (< a 173) (xmas-kk x m a s) (if (< a 325) (xmas-dsn x m a s) (quote reject)))))
(define xmas-mj (lambda (x m a s) (display "mj") (display " -> ") (if (< m 3015) (quote accept) (quote reject))))
(define xmas-zvd (lambda (x m a s) (display "zvd") (display " -> ") (if (< s 2061) (xmas-tbs x m a s) (if (< m 1477) (xmas-sl x m a s) (xmas-kl x m a s)))))
(define xmas-jx (lambda (x m a s) (display "jx") (display " -> ") (if (> x 145) (xmas-bn x m a s) (xmas-xnq x m a s))))
(define xmas-nc (lambda (x m a s) (display "nc") (display " -> ") (if (> m 683) (quote accept) (if (< x 2504) (quote reject) (if (> a 997) (quote reject) (quote accept))))))
(define xmas-nnq (lambda (x m a s) (display "nnq") (display " -> ") (if (> m 2255) (xmas-qtq x m a s) (xmas-mdj x m a s))))
(define xmas-kdd (lambda (x m a s) (display "kdd") (display " -> ") (if (< a 1665) (xmas-dd x m a s) (if (> s 2337) (quote reject) (if (< m 3014) (quote accept) (quote accept))))))
(define xmas-hqk (lambda (x m a s) (display "hqk") (display " -> ") (if (< s 3409) (quote accept) (if (> s 3722) (quote accept) (quote reject)))))
(define xmas-jhf (lambda (x m a s) (display "jhf") (display " -> ") (if (< x 2557) (quote accept) (if (< a 730) (quote reject) (quote accept)))))
(define xmas-vdg (lambda (x m a s) (display "vdg") (display " -> ") (if (< x 3930) (xmas-ghm x m a s) (if (< m 3341) (quote reject) (if (< a 949) (xmas-dmx x m a s) (quote accept))))))
(define xmas-vg (lambda (x m a s) (display "vg") (display " -> ") (if (< a 1470) (quote accept) (if (> x 2764) (xmas-pdt x m a s) (xmas-prc x m a s)))))
(define xmas-lmm (lambda (x m a s) (display "lmm") (display " -> ") (if (< s 1595) (xmas-rk x m a s) (xmas-qpv x m a s))))
(define xmas-kf (lambda (x m a s) (display "kf") (display " -> ") (if (> x 3482) (quote accept) (if (< m 2709) (quote reject) (quote reject)))))
(define xmas-gml (lambda (x m a s) (display "gml") (display " -> ") (if (> a 3626) (xmas-kz x m a s) (if (< m 3322) (xmas-fvs x m a s) (xmas-jmr x m a s)))))
(define xmas-hsj (lambda (x m a s) (display "hsj") (display " -> ") (if (< x 3421) (quote reject) (if (< x 3726) (quote reject) (if (< s 1156) (quote accept) (quote reject))))))
(define xmas-qx (lambda (x m a s) (display "qx") (display " -> ") (if (< a 1612) (xmas-vz x m a s) (xmas-nnq x m a s))))
(define xmas-mjb (lambda (x m a s) (display "mjb") (display " -> ") (if (> a 1017) (quote accept) (if (> a 433) (xmas-mqz x m a s) (xmas-pf x m a s)))))
(define xmas-dpv (lambda (x m a s) (display "dpv") (display " -> ") (if (> m 3697) (quote accept) (if (< s 872) (quote accept) (if (> m 3573) (quote reject) (xmas-fkv x m a s))))))
(define xmas-dmx (lambda (x m a s) (display "dmx") (display " -> ") (if (< m 3718) (quote accept) (if (> x 3961) (quote accept) (if (> a 916) (quote accept) (quote accept))))))
(define xmas-fc (lambda (x m a s) (display "fc") (display " -> ") (if (> m 722) (quote accept) (if (< m 376) (quote reject) (if (< m 538) (quote reject) (quote accept))))))
(define xmas-cxd (lambda (x m a s) (display "cxd") (display " -> ") (if (< s 1218) (xmas-snb x m a s) (if (> a 1837) (quote accept) (if (> m 2273) (quote accept) (quote accept))))))
(define xmas-jv (lambda (x m a s) (display "jv") (display " -> ") (if (> s 3082) (xmas-kkd x m a s) (if (< s 2674) (xmas-hjp x m a s) (xmas-xvb x m a s)))))
(define xmas-tnv (lambda (x m a s) (display "tnv") (display " -> ") (if (> x 800) (quote reject) (if (< s 1826) (quote reject) (if (< a 692) (quote accept) (quote accept))))))
(define xmas-kl (lambda (x m a s) (display "kl") (display " -> ") (if (< s 2444) (quote reject) (if (> s 2526) (quote reject) (quote reject)))))
(define xmas-zf (lambda (x m a s) (display "zf") (display " -> ") (if (> a 334) (quote reject) (quote accept))))
(define xmas-kt (lambda (x m a s) (display "kt") (display " -> ") (if (> s 2518) (xmas-rph x m a s) (xmas-vg x m a s))))
(define xmas-dhk (lambda (x m a s) (display "dhk") (display " -> ") (if (< a 1618) (quote reject) (quote reject))))
(define xmas-bhl (lambda (x m a s) (display "bhl") (display " -> ") (if (< m 3024) (quote reject) (quote reject))))
(define xmas-ppg (lambda (x m a s) (display "ppg") (display " -> ") (if (> x 3162) (quote reject) (quote accept))))
(define xmas-nvv (lambda (x m a s) (display "nvv") (display " -> ") (if (< a 2532) (quote accept) (if (> m 2216) (quote accept) (quote accept)))))
(define xmas-xvb (lambda (x m a s) (display "xvb") (display " -> ") (if (> x 458) (xmas-fc x m a s) (xmas-vhq x m a s))))
(define xmas-flg (lambda (x m a s) (display "flg") (display " -> ") (if (> s 3250) (xmas-tmx x m a s) (if (< x 3592) (xmas-bdh x m a s) (xmas-mxc x m a s)))))
(define xmas-thh (lambda (x m a s) (display "thh") (display " -> ") (if (< s 1567) (quote accept) (quote reject))))
(define xmas-xdj (lambda (x m a s) (display "xdj") (display " -> ") (if (< m 3434) (xmas-zxn x m a s) (if (< x 356) (xmas-hpx x m a s) (xmas-rrm x m a s)))))
(define xmas-zr (lambda (x m a s) (display "zr") (display " -> ") (if (> x 2998) (xmas-smv x m a s) (if (< x 2379) (xmas-gqb x m a s) (if (< s 2997) (xmas-ml x m a s) (xmas-tdr x m a s))))))
(define xmas-fjb (lambda (x m a s) (display "fjb") (display " -> ") (if (< a 961) (quote reject) (quote accept))))
(define xmas-pz (lambda (x m a s) (display "pz") (display " -> ") (if (< x 1962) (quote accept) (quote reject))))
(define xmas-nkj (lambda (x m a s) (display "nkj") (display " -> ") (if (> x 2911) (quote accept) (if (< m 3767) (quote reject) (if (> x 2553) (quote reject) (quote accept))))))
(define xmas-bf (lambda (x m a s) (display "bf") (display " -> ") (if (< x 2727) (xmas-tq x m a s) (if (< m 3716) (quote reject) (if (< x 3522) (xmas-btg x m a s) (xmas-sb x m a s))))))
(define xmas-hpx (lambda (x m a s) (display "hpx") (display " -> ") (if (< s 1574) (xmas-zz x m a s) (if (> a 1130) (xmas-mn x m a s) (xmas-jx x m a s)))))
(define xmas-skq (lambda (x m a s) (display "skq") (display " -> ") (if (< m 3265) (xmas-kdr x m a s) (if (< a 3095) (xmas-dfl x m a s) (xmas-lgh x m a s)))))
(define xmas-mdj (lambda (x m a s) (display "mdj") (display " -> ") (if (> m 2009) (quote reject) (quote accept))))
(define xmas-hfl (lambda (x m a s) (display "hfl") (display " -> ") (if (< x 2221) (xmas-hv x m a s) (if (> a 525) (xmas-qqg x m a s) (xmas-nx x m a s)))))
(define xmas-qcg (lambda (x m a s) (display "qcg") (display " -> ") (if (< a 743) (xmas-px x m a s) (if (> s 2697) (xmas-vd x m a s) (quote reject)))))
(define xmas-brm (lambda (x m a s) (display "brm") (display " -> ") (if (< x 1786) (xmas-vbq x m a s) (if (> m 3294) (xmas-fzt x m a s) (if (> x 1840) (xmas-cr x m a s) (xmas-hgf x m a s))))))
(define xmas-fhx (lambda (x m a s) (display "fhx") (display " -> ") (if (< a 833) (quote accept) (if (< s 2988) (xmas-jq x m a s) (if (< m 453) (quote accept) (quote accept))))))
(define xmas-tqd (lambda (x m a s) (display "tqd") (display " -> ") (if (> x 3713) (quote reject) (if (> a 3304) (quote accept) (quote accept)))))
(define xmas-qg (lambda (x m a s) (display "qg") (display " -> ") (if (< x 3734) (quote accept) (if (< s 3427) (quote accept) (quote reject)))))
(define xmas-fxs (lambda (x m a s) (display "fxs") (display " -> ") (if (< a 1567) (quote reject) (if (> m 3691) (quote accept) (if (< a 1626) (quote accept) (quote reject))))))
(define xmas-js (lambda (x m a s) (display "js") (display " -> ") (if (> s 2419) (xmas-jmc x m a s) (xmas-bld x m a s))))
(define xmas-mv (lambda (x m a s) (display "mv") (display " -> ") (if (> s 1609) (quote reject) (if (> s 1439) (quote reject) (if (< a 1646) (quote accept) (xmas-cx x m a s))))))
(define xmas-gxd (lambda (x m a s) (display "gxd") (display " -> ") (if (< a 556) (quote accept) (if (< m 1972) (quote accept) (quote reject)))))
(define xmas-bfx (lambda (x m a s) (display "bfx") (display " -> ") (if (> m 2202) (quote reject) (quote accept))))
(define xmas-xm (lambda (x m a s) (display "xm") (display " -> ") (if (< m 3293) (quote reject) (if (< m 3313) (quote reject) (if (> s 1326) (quote accept) (quote accept))))))
(define xmas-qcc (lambda (x m a s) (display "qcc") (display " -> ") (if (> a 1359) (quote reject) (if (> m 3131) (quote accept) (quote accept)))))
(define xmas-kqh (lambda (x m a s) (display "kqh") (display " -> ") (if (> x 1651) (xmas-dqd x m a s) (if (< m 1113) (xmas-vqs x m a s) (if (> m 1461) (xmas-sqj x m a s) (xmas-hj x m a s))))))
(define xmas-st (lambda (x m a s) (display "st") (display " -> ") (if (< x 3852) (quote reject) (quote accept))))
(define xmas-bt (lambda (x m a s) (display "bt") (display " -> ") (if (< s 3362) (quote reject) (if (> x 2509) (quote reject) (if (< m 415) (quote accept) (quote accept))))))
(define xmas-kbc (lambda (x m a s) (display "kbc") (display " -> ") (if (< x 670) (xmas-mkq x m a s) (quote accept))))
(define xmas-mnc (lambda (x m a s) (display "mnc") (display " -> ") (if (> a 759) (quote reject) (if (> a 697) (quote accept) (if (< a 654) (quote reject) (quote accept))))))
(define xmas-mqz (lambda (x m a s) (display "mqz") (display " -> ") (if (< s 2072) (quote accept) (if (> m 3722) (quote reject) (quote reject)))))
(define xmas-fn (lambda (x m a s) (display "fn") (display " -> ") (if (< x 1666) (quote reject) (quote reject))))
(define xmas-zrt (lambda (x m a s) (display "zrt") (display " -> ") (if (> m 590) (quote accept) (if (< a 749) (quote reject) (if (< a 766) (quote reject) (quote reject))))))
(define xmas-dl (lambda (x m a s) (display "dl") (display " -> ") (if (< x 2405) (xmas-gt x m a s) (if (> m 1832) (xmas-bfx x m a s) (if (> a 3717) (xmas-hsj x m a s) (xmas-sh x m a s))))))
(define xmas-xnl (lambda (x m a s) (display "xnl") (display " -> ") (if (> a 524) (quote reject) (if (< s 2581) (xmas-qlz x m a s) (xmas-rrx x m a s)))))
(define xmas-qpv (lambda (x m a s) (display "qpv") (display " -> ") (if (< s 2019) (xmas-cmf x m a s) (if (< a 695) (quote accept) (if (< m 1257) (quote reject) (quote reject))))))
(define xmas-dlj (lambda (x m a s) (display "dlj") (display " -> ") (if (< x 192) (quote reject) (quote accept))))
(define xmas-rg (lambda (x m a s) (display "rg") (display " -> ") (if (> m 848) (xmas-zdx x m a s) (if (< a 1237) (xmas-nl x m a s) (xmas-hsd x m a s)))))
(define xmas-lf (lambda (x m a s) (display "lf") (display " -> ") (if (< a 3204) (xmas-lcd x m a s) (if (< x 910) (xmas-cfd x m a s) (if (< a 3565) (xmas-dr x m a s) (xmas-zl x m a s))))))
(define xmas-nps (lambda (x m a s) (display "nps") (display " -> ") (if (> m 2943) (xmas-dhk x m a s) (if (< x 2837) (quote reject) (quote accept)))))
(define xmas-pk (lambda (x m a s) (display "pk") (display " -> ") (if (< s 3133) (quote reject) (if (< s 3500) (quote accept) (quote reject)))))
(define xmas-hb (lambda (x m a s) (display "hb") (display " -> ") (if (< x 2063) (quote reject) (if (< a 3400) (quote reject) (if (< m 384) (quote reject) (quote accept))))))
(define xmas-tt (lambda (x m a s) (display "tt") (display " -> ") (if (> x 2490) (xmas-vbp x m a s) (if (< s 3152) (xmas-cmp x m a s) (xmas-qq x m a s)))))
(define xmas-sh (lambda (x m a s) (display "sh") (display " -> ") (if (< s 1092) (quote reject) (if (> a 3463) (quote accept) (if (< s 1392) (quote reject) (quote reject))))))
(define xmas-tpl (lambda (x m a s) (display "tpl") (display " -> ") (if (> m 3508) (xmas-gz x m a s) (if (> m 2949) (quote reject) (if (> s 1047) (xmas-fp x m a s) (quote accept))))))
(define xmas-sst (lambda (x m a s) (display "sst") (display " -> ") (if (> m 832) (quote accept) (quote accept))))
(define xmas-qvs (lambda (x m a s) (display "qvs") (display " -> ") (if (> x 2274) (xmas-ksm x m a s) (xmas-lf x m a s))))
(define xmas-nq (lambda (x m a s) (display "nq") (display " -> ") (if (< s 3159) (quote accept) (quote reject))))
(define xmas-qgf (lambda (x m a s) (display "qgf") (display " -> ") (if (> m 2065) (xmas-hbj x m a s) (if (< s 2483) (xmas-sd x m a s) (xmas-qkl x m a s)))))
(define xmas-lk (lambda (x m a s) (display "lk") (display " -> ") (if (< m 1052) (quote reject) (if (> m 1945) (quote reject) (quote reject)))))
(define xmas-qv (lambda (x m a s) (display "qv") (display " -> ") (if (< s 3357) (quote accept) (quote accept))))
(define xmas-blv (lambda (x m a s) (display "blv") (display " -> ") (if (> x 3046) (xmas-dtj x m a s) (if (> s 3114) (quote accept) (quote reject)))))
(define xmas-lg (lambda (x m a s) (display "lg") (display " -> ") (if (> m 2254) (xmas-qlv x m a s) (xmas-cmr x m a s))))
(define xmas-nh (lambda (x m a s) (display "nh") (display " -> ") (if (> x 431) (quote reject) (if (> a 1433) (quote accept) (quote accept)))))
(define xmas-pps (lambda (x m a s) (display "pps") (display " -> ") (if (> a 1893) (quote accept) (quote reject))))
(define xmas-qk (lambda (x m a s) (display "qk") (display " -> ") (if (< x 3254) (xmas-jtz x m a s) (if (> m 3294) (xmas-nvz x m a s) (xmas-tps x m a s)))))
(define xmas-qlv (lambda (x m a s) (display "qlv") (display " -> ") (if (> s 1566) (xmas-rpb x m a s) (if (> s 575) (quote reject) (if (< x 1750) (xmas-jzd x m a s) (quote reject))))))
(define xmas-qq (lambda (x m a s) (display "qq") (display " -> ") (if (> m 375) (xmas-zg x m a s) (if (< a 867) (quote reject) (if (< m 250) (xmas-lx x m a s) (quote reject))))))
(define xmas-rx (lambda (x m a s) (display "rx") (display " -> ") (if (< x 2021) (quote accept) (if (< x 2053) (quote reject) (if (< m 3343) (quote accept) (quote accept))))))
(define xmas-jmr (lambda (x m a s) (display "jmr") (display " -> ") (if (> m 3710) (xmas-xnx x m a s) (if (< s 1515) (xmas-tlr x m a s) (if (< x 1654) (xmas-gnn x m a s) (quote accept))))))
(define xmas-lcd (lambda (x m a s) (display "lcd") (display " -> ") (if (< x 793) (xmas-nk x m a s) (if (< a 2779) (quote reject) (quote accept)))))
(define xmas-sl (lambda (x m a s) (display "sl") (display " -> ") (if (< a 3170) (quote reject) (if (< x 3352) (quote accept) (if (> s 2348) (quote reject) (quote reject))))))
(define xmas-nl (lambda (x m a s) (display "nl") (display " -> ") (if (> m 394) (quote accept) (quote reject))))
(define xmas-rt (lambda (x m a s) (display "rt") (display " -> ") (if (> a 1895) (xmas-ztl x m a s) (xmas-tnt x m a s))))
(define xmas-mlt (lambda (x m a s) (display "mlt") (display " -> ") (if (> s 1178) (quote accept) (if (> x 2340) (quote accept) (if (> s 610) (quote accept) (quote accept))))))
(define xmas-srt (lambda (x m a s) (display "srt") (display " -> ") (if (> x 3764) (xmas-fbh x m a s) (if (> m 3302) (quote accept) (quote accept)))))
(define xmas-pg (lambda (x m a s) (display "pg") (display " -> ") (if (< s 1473) (quote reject) (if (> m 2572) (quote accept) (xmas-qc x m a s)))))
(define xmas-btc (lambda (x m a s) (display "btc") (display " -> ") (if (> m 2364) (quote reject) (if (< m 2104) (xmas-rqj x m a s) (if (< a 1891) (xmas-hmg x m a s) (xmas-mr x m a s))))))
(define xmas-md (lambda (x m a s) (display "md") (display " -> ") (if (< a 2896) (xmas-rh x m a s) (if (< m 2539) (quote accept) (xmas-hn x m a s)))))
(define xmas-zxn (lambda (x m a s) (display "zxn") (display " -> ") (if (< x 618) (xmas-ctj x m a s) (if (> s 1748) (xmas-bgt x m a s) (if (< a 1183) (xmas-hqv x m a s) (xmas-bts x m a s))))))
(define xmas-qbd (lambda (x m a s) (display "qbd") (display " -> ") (if (> a 1300) (quote reject) (if (> a 1195) (quote reject) (quote accept)))))
(define xmas-gm (lambda (x m a s) (display "gm") (display " -> ") (if (< a 888) (xmas-fhx x m a s) (if (< m 490) (xmas-flg x m a s) (xmas-ckn x m a s)))))
(define xmas-sb (lambda (x m a s) (display "sb") (display " -> ") (if (< x 3829) (quote accept) (if (< x 3943) (quote accept) (if (> x 3979) (quote accept) (quote reject))))))
(define xmas-fpv (lambda (x m a s) (display "fpv") (display " -> ") (if (> x 2671) (quote accept) (if (> x 2630) (quote accept) (if (> a 2602) (quote reject) (xmas-lk x m a s))))))
(define xmas-cmr (lambda (x m a s) (display "cmr") (display " -> ") (if (> a 1618) (xmas-ndl x m a s) (if (> x 1648) (xmas-nvs x m a s) (xmas-rqg x m a s)))))
(define xmas-fr (lambda (x m a s) (display "fr") (display " -> ") (if (> x 2491) (quote reject) (xmas-hb x m a s))))
(define xmas-zll (lambda (x m a s) (display "zll") (display " -> ") (if (< x 2443) (xmas-xvp x m a s) (xmas-zsg x m a s))))
(define xmas-xg (lambda (x m a s) (display "xg") (display " -> ") (if (> a 1229) (xmas-zn x m a s) (if (< x 905) (xmas-tnv x m a s) (xmas-db x m a s)))))
(define xmas-llt (lambda (x m a s) (display "llt") (display " -> ") (if (< m 1846) (quote reject) (quote accept))))
(define xmas-mjq (lambda (x m a s) (display "mjq") (display " -> ") (if (< x 1653) (xmas-xqn x m a s) (xmas-fzq x m a s))))
(define xmas-vj (lambda (x m a s) (display "vj") (display " -> ") (if (< x 2815) (quote reject) (quote reject))))
(define xmas-kfh (lambda (x m a s) (display "kfh") (display " -> ") (if (< s 3145) (quote accept) (if (> s 3678) (quote reject) (quote accept)))))
(define xmas-ckn (lambda (x m a s) (display "ckn") (display " -> ") (if (> a 910) (quote accept) (if (> x 3622) (quote reject) (if (< a 902) (xmas-dx x m a s) (xmas-pcn x m a s))))))
(define xmas-zcf (lambda (x m a s) (display "zcf") (display " -> ") (if (> s 1918) (quote reject) (if (< x 546) (quote accept) (if (< a 3666) (quote reject) (quote accept))))))
(define xmas-ms (lambda (x m a s) (display "ms") (display " -> ") (if (< m 585) (quote reject) (if (< s 953) (xmas-ghg x m a s) (if (> x 1498) (xmas-fpr x m a s) (quote accept))))))
(define xmas-nbz (lambda (x m a s) (display "nbz") (display " -> ") (if (< a 400) (quote reject) (quote accept))))
(define xmas-svg (lambda (x m a s) (display "svg") (display " -> ") (if (> m 2523) (xmas-kx x m a s) (if (< s 1755) (xmas-bm x m a s) (xmas-qvs x m a s)))))
(define xmas-dfl (lambda (x m a s) (display "dfl") (display " -> ") (if (< m 3731) (quote reject) (quote accept))))
(define xmas-qs (lambda (x m a s) (display "qs") (display " -> ") (if (> m 3253) (quote reject) (if (> a 1665) (quote reject) (quote accept)))))
(define xmas-hg (lambda (x m a s) (display "hg") (display " -> ") (if (< x 510) (quote accept) (if (< s 2024) (quote reject) (if (< m 479) (quote reject) (quote accept))))))
(define xmas-qtg (lambda (x m a s) (display "qtg") (display " -> ") (if (> m 1184) (quote reject) (if (< s 2253) (quote reject) (if (> m 704) (quote reject) (quote reject))))))
(define xmas-psf (lambda (x m a s) (display "psf") (display " -> ") (if (< a 1654) (quote accept) (quote reject))))
(define xmas-ck (lambda (x m a s) (display "ck") (display " -> ") (if (> x 804) (quote reject) (if (< s 2886) (xmas-mds x m a s) (xmas-bd x m a s)))))
(define xmas-jz (lambda (x m a s) (display "jz") (display " -> ") (if (> s 1351) (quote accept) (if (< s 637) (quote accept) (if (> m 3673) (quote reject) (quote accept))))))
(define xmas-pdt (lambda (x m a s) (display "pdt") (display " -> ") (if (< a 1696) (quote reject) (quote accept))))
(define xmas-dsn (lambda (x m a s) (display "dsn") (display " -> ") (if (< x 3241) (quote reject) (if (> m 1921) (quote accept) (quote reject)))))
(define xmas-cr (lambda (x m a s) (display "cr") (display " -> ") (if (< s 3051) (quote reject) (xmas-ntj x m a s))))
(define xmas-rqg (lambda (x m a s) (display "rqg") (display " -> ") (if (< m 2046) (quote accept) (if (< x 1469) (quote reject) (if (> x 1558) (quote reject) (quote reject))))))
(define xmas-ggf (lambda (x m a s) (display "ggf") (display " -> ") (if (< a 802) (quote accept) (if (< s 1148) (quote reject) (if (< x 3200) (quote reject) (quote accept))))))
(define xmas-dh (lambda (x m a s) (display "dh") (display " -> ") (if (> a 758) (xmas-gps x m a s) (xmas-jb x m a s))))
(define xmas-cjd (lambda (x m a s) (display "cjd") (display " -> ") (if (> s 1079) (quote reject) (if (< s 695) (quote accept) (if (< s 830) (quote accept) (quote reject))))))
(define xmas-pdz (lambda (x m a s) (display "pdz") (display " -> ") (if (> a 1874) (quote reject) (quote accept))))
(define xmas-jtg (lambda (x m a s) (display "jtg") (display " -> ") (if (< m 3797) (quote accept) (quote reject))))
(define xmas-jsf (lambda (x m a s) (display "jsf") (display " -> ") (if (< m 2622) (xmas-rzd x m a s) (if (< x 1372) (xmas-fx x m a s) (if (> x 2301) (xmas-kpl x m a s) (xmas-nm x m a s))))))
(define xmas-zm (lambda (x m a s) (display "zm") (display " -> ") (if (< a 963) (quote reject) (if (> s 3171) (xmas-jj x m a s) (if (> m 3023) (xmas-tn x m a s) (xmas-lb x m a s))))))
(define xmas-ctj (lambda (x m a s) (display "ctj") (display " -> ") (if (< s 2660) (xmas-cg x m a s) (xmas-hd x m a s))))
(define xmas-dlb (lambda (x m a s) (display "dlb") (display " -> ") (if (< x 1015) (xmas-xdj x m a s) (if (> s 1984) (xmas-lgq x m a s) (xmas-lc x m a s)))))
(define xmas-vgh (lambda (x m a s) (display "vgh") (display " -> ") (if (< a 978) (quote reject) (quote accept))))
(define xmas-sd (lambda (x m a s) (display "sd") (display " -> ") (if (< m 1949) (xmas-pz x m a s) (if (< s 926) (xmas-jjr x m a s) (if (> m 1989) (xmas-ht x m a s) (xmas-kkf x m a s))))))
(define xmas-ccd (lambda (x m a s) (display "ccd") (display " -> ") (if (> m 1353) (quote accept) (if (> a 1394) (quote accept) (quote accept)))))
(define xmas-fxr (lambda (x m a s) (display "fxr") (display " -> ") (if (> a 2560) (quote accept) (if (> m 1609) (quote reject) (quote accept)))))
(define xmas-xt (lambda (x m a s) (display "xt") (display " -> ") (if (> a 300) (quote reject) (if (< a 166) (quote accept) (if (< x 2401) (quote accept) (quote accept))))))
(define xmas-rd (lambda (x m a s) (display "rd") (display " -> ") (if (> m 441) (quote accept) (if (> a 1690) (quote reject) (if (< m 260) (quote reject) (quote reject))))))
(define xmas-ccs (lambda (x m a s) (display "ccs") (display " -> ") (if (< s 838) (quote accept) (quote reject))))
(define xmas-ztm (lambda (x m a s) (display "ztm") (display " -> ") (if (> a 542) (quote accept) (if (< m 3852) (quote accept) (quote accept)))))
(define xmas-pcn (lambda (x m a s) (display "pcn") (display " -> ") (if (> s 3269) (quote accept) (if (< a 907) (quote reject) (quote accept)))))
(define xmas-hp (lambda (x m a s) (display "hp") (display " -> ") (if (> s 2757) (xmas-mg x m a s) (xmas-qrq x m a s))))
(define xmas-ldc (lambda (x m a s) (display "ldc") (display " -> ") (if (> x 3475) (quote accept) (if (< s 3762) (xmas-dz x m a s) (xmas-dn x m a s)))))
(define xmas-lzh (lambda (x m a s) (display "lzh") (display " -> ") (if (< s 3256) (quote reject) (if (< a 658) (quote reject) (quote reject)))))
(define xmas-gnn (lambda (x m a s) (display "gnn") (display " -> ") (if (< m 3490) (quote accept) (if (> s 1926) (quote reject) (if (< a 3250) (quote accept) (quote reject))))))
(define xmas-scp (lambda (x m a s) (display "scp") (display " -> ") (if (> x 764) (quote reject) (if (> a 1806) (quote reject) (quote reject)))))
(define xmas-vtd (lambda (x m a s) (display "vtd") (display " -> ") (if (> s 785) (quote reject) (if (< m 1742) (quote accept) (quote accept)))))
(define xmas-jqj (lambda (x m a s) (display "jqj") (display " -> ") (if (> a 1728) (xmas-rt x m a s) (if (< a 1550) (xmas-zxs x m a s) (xmas-mps x m a s)))))
(define xmas-hsl (lambda (x m a s) (display "hsl") (display " -> ") (if (> x 1037) (quote reject) (if (> x 580) (quote accept) (quote accept)))))
(define xmas-gxg (lambda (x m a s) (display "gxg") (display " -> ") (if (> m 1274) (quote reject) (if (< s 3369) (quote reject) (if (< m 937) (quote reject) (quote reject))))))
(define xmas-rh (lambda (x m a s) (display "rh") (display " -> ") (if (< a 2411) (quote accept) (if (< x 2226) (quote reject) (if (< x 2849) (quote reject) (quote reject))))))
(define xmas-hrd (lambda (x m a s) (display "hrd") (display " -> ") (if (< s 3098) (xmas-tp x m a s) (xmas-jhf x m a s))))
(define xmas-nvz (lambda (x m a s) (display "nvz") (display " -> ") (if (> m 3703) (quote reject) (if (< s 2096) (xmas-hs x m a s) (xmas-kfx x m a s)))))
(define xmas-kx (lambda (x m a s) (display "kx") (display " -> ") (if (< a 3022) (xmas-tqs x m a s) (xmas-gml x m a s))))
(define xmas-xv (lambda (x m a s) (display "xv") (display " -> ") (if (> x 1180) (quote reject) (quote accept))))
(define xmas-tmr (lambda (x m a s) (display "tmr") (display " -> ") (if (> s 2918) (quote reject) (quote reject))))
(define xmas-lnc (lambda (x m a s) (display "lnc") (display " -> ") (if (> x 3019) (quote accept) (if (> m 3500) (quote reject) (if (> a 3032) (xmas-nq x m a s) (quote reject))))))
(define xmas-bpc (lambda (x m a s) (display "bpc") (display " -> ") (if (< m 1680) (quote reject) (if (< x 3257) (quote accept) (if (< a 1791) (quote accept) (quote reject))))))
(define xmas-fkv (lambda (x m a s) (display "fkv") (display " -> ") (if (> a 828) (quote accept) (if (< s 1349) (quote accept) (if (< m 3525) (quote reject) (quote reject))))))
(define xmas-dqd (lambda (x m a s) (display "dqd") (display " -> ") (if (< x 1784) (xmas-jf x m a s) (if (< s 3044) (xmas-zf x m a s) (xmas-pvz x m a s)))))
(define xmas-vkn (lambda (x m a s) (display "vkn") (display " -> ") (if (> m 3521) (quote accept) (if (> a 420) (quote accept) (if (< s 2784) (quote reject) (quote reject))))))
(define xmas-kpl (lambda (x m a s) (display "kpl") (display " -> ") (if (< x 3416) (xmas-lnc x m a s) (xmas-lh x m a s))))
(define xmas-bts (lambda (x m a s) (display "bts") (display " -> ") (if (> x 848) (quote reject) (if (< x 712) (xmas-cjd x m a s) (if (< a 1645) (quote reject) (xmas-scp x m a s))))))
(define xmas-psz (lambda (x m a s) (display "psz") (display " -> ") (if (> a 1926) (xmas-gf x m a s) (if (> x 3134) (xmas-gxg x m a s) (if (< s 3215) (xmas-pps x m a s) (quote accept))))))
(define xmas-qtq (lambda (x m a s) (display "qtq") (display " -> ") (if (< s 2450) (quote accept) (if (< s 3447) (xmas-shm x m a s) (if (< a 1787) (xmas-ppg x m a s) (xmas-str x m a s))))))
(define xmas-dmp (lambda (x m a s) (display "dmp") (display " -> ") (if (< x 447) (quote accept) (quote reject))))
(define xmas-lgh (lambda (x m a s) (display "lgh") (display " -> ") (if (> m 3647) (quote reject) (if (< a 3490) (quote reject) (if (< a 3775) (quote accept) (quote accept))))))
(define xmas-xzl (lambda (x m a s) (display "xzl") (display " -> ") (if (> m 1524) (quote reject) (if (> m 1326) (quote accept) (if (< s 2750) (quote reject) (quote reject))))))
(define xmas-srv (lambda (x m a s) (display "srv") (display " -> ") (if (< m 2556) (quote reject) (if (< s 843) (xmas-vkm x m a s) (if (< s 1416) (xmas-ggf x m a s) (quote accept))))))
(define xmas-mr (lambda (x m a s) (display "mr") (display " -> ") (if (< m 2264) (quote reject) (quote accept))))
(define xmas-th (lambda (x m a s) (display "th") (display " -> ") (if (< s 835) (quote reject) (if (> m 3729) (quote reject) (if (> x 2872) (quote reject) (quote reject))))))
(define xmas-jzd (lambda (x m a s) (display "jzd") (display " -> ") (if (> s 258) (quote reject) (if (> m 2563) (quote accept) (quote accept)))))
(define xmas-sk (lambda (x m a s) (display "sk") (display " -> ") (if (< a 1710) (xmas-fxs x m a s) (xmas-ns x m a s))))
(define xmas-cjj (lambda (x m a s) (display "cjj") (display " -> ") (if (< a 1060) (xmas-sv x m a s) (if (> a 1170) (xmas-rg x m a s) (if (> x 2037) (xmas-mt x m a s) (xmas-zx x m a s))))))
(define xmas-jf (lambda (x m a s) (display "jf") (display " -> ") (if (> m 928) (quote reject) (quote accept))))
(define xmas-mf (lambda (x m a s) (display "mf") (display " -> ") (if (> a 973) (xmas-hvz x m a s) (quote reject))))
(define xmas-vd (lambda (x m a s) (display "vd") (display " -> ") (if (> x 3203) (quote accept) (if (< s 2797) (quote accept) (quote reject)))))
(define xmas-hrg (lambda (x m a s) (display "hrg") (display " -> ") (if (< a 1371) (quote accept) (if (> a 1483) (quote accept) (quote reject)))))
(define xmas-nt (lambda (x m a s) (display "nt") (display " -> ") (if (> m 670) (xmas-qg x m a s) (if (< a 362) (quote accept) (quote accept)))))
(define xmas-qmk (lambda (x m a s) (display "qmk") (display " -> ") (if (> a 675) (quote accept) (if (> s 3143) (quote accept) (if (< a 671) (quote reject) (quote reject))))))
(define xmas-rzd (lambda (x m a s) (display "rzd") (display " -> ") (if (< a 2988) (xmas-zdk x m a s) (xmas-nn x m a s))))
(define xmas-kd (lambda (x m a s) (display "kd") (display " -> ") (if (< a 309) (quote reject) (if (< s 1769) (quote reject) (quote reject)))))
(define xmas-xqn (lambda (x m a s) (display "xqn") (display " -> ") (if (< m 2720) (xmas-gvf x m a s) (quote reject))))
(define xmas-lrf (lambda (x m a s) (display "lrf") (display " -> ") (if (> m 2123) (quote accept) (quote reject))))
(define xmas-kgs (lambda (x m a s) (display "kgs") (display " -> ") (if (< x 3178) (xmas-md x m a s) (xmas-ldc x m a s))))
(define xmas-zqf (lambda (x m a s) (display "zqf") (display " -> ") (if (> x 2658) (quote reject) (if (> x 2351) (quote reject) (if (< m 3669) (quote reject) (quote accept))))))
(define xmas-lq (lambda (x m a s) (display "lq") (display " -> ") (if (< x 3090) (xmas-prq x m a s) (if (> x 3401) (xmas-tl x m a s) (xmas-qk x m a s)))))
(define xmas-zh (lambda (x m a s) (display "zh") (display " -> ") (if (> m 985) (xmas-sr x m a s) (xmas-pls x m a s))))
(define xmas-hvz (lambda (x m a s) (display "hvz") (display " -> ") (if (> s 2459) (quote accept) (quote reject))))
(define xmas-tdr (lambda (x m a s) (display "tdr") (display " -> ") (if (> x 2697) (quote accept) (if (< a 745) (quote accept) (if (> a 790) (xmas-zt x m a s) (xmas-lvb x m a s))))))
(define xmas-hqd (lambda (x m a s) (display "hqd") (display " -> ") (if (> a 113) (quote reject) (quote reject))))
(define xmas-qz (lambda (x m a s) (display "qz") (display " -> ") (if (> x 2674) (quote reject) (if (< x 2580) (quote accept) (quote reject)))))
(define xmas-cmd (lambda (x m a s) (display "cmd") (display " -> ") (if (> a 3700) (quote reject) (quote reject))))
(define xmas-gr (lambda (x m a s) (display "gr") (display " -> ") (if (> m 3628) (quote accept) (quote reject))))
(define xmas-kkd (lambda (x m a s) (display "kkd") (display " -> ") (if (< s 3400) (quote reject) (if (< m 714) (xmas-jk x m a s) (xmas-dmp x m a s)))))
(define xmas-qfs (lambda (x m a s) (display "qfs") (display " -> ") (if (> x 3836) (quote reject) (if (< m 1400) (quote accept) (if (< x 3759) (quote reject) (quote reject))))))
(define xmas-shm (lambda (x m a s) (display "shm") (display " -> ") (if (> a 1800) (quote accept) (if (< a 1730) (quote reject) (if (> a 1770) (quote accept) (quote reject))))))
(define xmas-zt (lambda (x m a s) (display "zt") (display " -> ") (if (> m 3276) (quote reject) (if (< m 3120) (quote reject) (if (> a 831) (quote reject) (quote reject))))))
(define xmas-ghg (lambda (x m a s) (display "ghg") (display " -> ") (if (< a 1820) (quote reject) (quote accept))))
(define xmas-tbx (lambda (x m a s) (display "tbx") (display " -> ") (if (< x 3172) (quote accept) (if (< a 196) (quote reject) (quote reject)))))
(define xmas-zdk (lambda (x m a s) (display "zdk") (display " -> ") (if (< m 1472) (xmas-lsb x m a s) (quote accept))))
(define xmas-kr (lambda (x m a s) (display "kr") (display " -> ") (if (> a 1320) (xmas-sk x m a s) (xmas-dpv x m a s))))
(define xmas-pfj (lambda (x m a s) (display "pfj") (display " -> ") (if (> s 3032) (quote reject) (quote reject))))
(define xmas-cf (lambda (x m a s) (display "cf") (display " -> ") (if (> x 1919) (xmas-lt x m a s) (xmas-fd x m a s))))
(define xmas-bbc (lambda (x m a s) (display "bbc") (display " -> ") (if (> x 1594) (quote reject) (if (< m 3225) (xmas-gdn x m a s) (if (> a 1201) (quote accept) (quote accept))))))
(define xmas-lsb (lambda (x m a s) (display "lsb") (display " -> ") (if (< x 2500) (quote reject) (if (< a 2407) (quote reject) (if (> a 2649) (quote reject) (quote accept))))))
(define xmas-sm (lambda (x m a s) (display "sm") (display " -> ") (if (> m 3048) (quote accept) (if (> a 1017) (quote reject) (quote accept)))))
(define xmas-sjv (lambda (x m a s) (display "sjv") (display " -> ") (if (> a 429) (xmas-hzr x m a s) (if (> a 402) (xmas-vkn x m a s) (quote reject)))))
(define xmas-mh (lambda (x m a s) (display "mh") (display " -> ") (if (> a 2662) (quote accept) (quote accept))))
(define xmas-kgj (lambda (x m a s) (display "kgj") (display " -> ") (if (> m 485) (quote accept) (if (> s 920) (xmas-nbz x m a s) (quote accept)))))
(define xmas-sxh (lambda (x m a s) (display "sxh") (display " -> ") (if (> m 1060) (quote reject) (if (> s 2864) (quote reject) (quote reject)))))
(define xmas-jq (lambda (x m a s) (display "jq") (display " -> ") (if (< m 596) (quote accept) (quote reject))))
(define xmas-rpb (lambda (x m a s) (display "rpb") (display " -> ") (if (> a 1574) (quote accept) (quote accept))))
(define xmas-grp (lambda (x m a s) (display "grp") (display " -> ") (if (> a 3284) (xmas-dl x m a s) (if (> m 1831) (xmas-hk x m a s) (if (< m 1617) (xmas-rlp x m a s) (xmas-mbq x m a s))))))
(define xmas-dxh (lambda (x m a s) (display "dxh") (display " -> ") (if (< m 3650) (xmas-nfh x m a s) (if (> m 3848) (xmas-nxp x m a s) (if (> m 3717) (xmas-nkj x m a s) (xmas-ntv x m a s))))))
(define xmas-jtz (lambda (x m a s) (display "jtz") (display " -> ") (if (> m 3480) (xmas-jz x m a s) (if (< s 2415) (quote reject) (quote reject)))))
(define xmas-hs (lambda (x m a s) (display "hs") (display " -> ") (if (> m 3505) (quote accept) (if (> s 1154) (quote reject) (if (< a 1005) (quote accept) (quote accept))))))
(define xmas-cmf (lambda (x m a s) (display "cmf") (display " -> ") (if (< s 1821) (quote reject) (if (< a 458) (quote reject) (if (< s 1920) (quote reject) (quote reject))))))
(define xmas-zv (lambda (x m a s) (display "zv") (display " -> ") (if (> x 2271) (quote reject) (quote accept))))
(define xmas-fd (lambda (x m a s) (display "fd") (display " -> ") (if (< m 3519) (quote reject) (if (> x 1581) (quote reject) (if (> a 2722) (quote reject) (quote accept))))))
(define xmas-dbs (lambda (x m a s) (display "dbs") (display " -> ") (if (> s 3121) (quote reject) (if (> x 3421) (quote accept) (if (< m 1352) (quote reject) (quote accept))))))
(define xmas-xsc (lambda (x m a s) (display "xsc") (display " -> ") (if (< s 3091) (quote reject) (if (> x 792) (quote accept) (if (< s 3555) (quote accept) (quote reject))))))
(define xmas-rht (lambda (x m a s) (display "rht") (display " -> ") (if (> x 607) (quote reject) (if (> m 2014) (quote reject) (if (> a 503) (quote accept) (quote accept))))))
(define xmas-lz (lambda (x m a s) (display "lz") (display " -> ") (if (> x 3564) (xmas-xm x m a s) (if (< x 3325) (quote accept) (xmas-skb x m a s)))))
(define xmas-zdx (lambda (x m a s) (display "zdx") (display " -> ") (if (> s 3219) (quote accept) (if (< x 2480) (quote reject) (if (< x 3484) (quote accept) (quote reject))))))
(define xmas-kv (lambda (x m a s) (display "kv") (display " -> ") (if (> x 1016) (xmas-ds x m a s) (quote reject))))
(define xmas-snb (lambda (x m a s) (display "snb") (display " -> ") (if (> s 574) (quote reject) (if (> a 1880) (quote accept) (if (> x 923) (quote reject) (quote reject))))))
(define xmas-mn (lambda (x m a s) (display "mn") (display " -> ") (if (> s 3187) (quote reject) (if (> s 2164) (xmas-bs x m a s) (if (< m 3739) (xmas-dlj x m a s) (xmas-xhl x m a s))))))
(define xmas-mxc (lambda (x m a s) (display "mxc") (display " -> ") (if (< m 242) (quote accept) (if (< x 3746) (quote reject) (quote accept)))))
(define xmas-px (lambda (x m a s) (display "px") (display " -> ") (if (> a 644) (quote accept) (if (< s 2728) (quote reject) (if (> x 3287) (quote reject) (quote reject))))))
(define xmas-hbj (lambda (x m a s) (display "hbj") (display " -> ") (if (< m 2211) (xmas-shp x m a s) (if (> m 2308) (xmas-nqq x m a s) (xmas-xnl x m a s)))))
(define xmas-bsj (lambda (x m a s) (display "bsj") (display " -> ") (if (> s 3147) (quote accept) (quote reject))))
(define xmas-qd (lambda (x m a s) (display "qd") (display " -> ") (if (> s 3693) (quote reject) (if (< a 3660) (xmas-qp x m a s) (if (> s 3612) (quote accept) (xmas-vjs x m a s))))))
(define xmas-zhk (lambda (x m a s) (display "zhk") (display " -> ") (if (< x 2632) (xmas-mlt x m a s) (quote reject))))
(define xmas-ksm (lambda (x m a s) (display "ksm") (display " -> ") (if (> x 2854) (xmas-zvd x m a s) (if (> a 3133) (xmas-tv x m a s) (if (> x 2578) (xmas-fpv x m a s) (xmas-mjs x m a s))))))
(define xmas-rnp (lambda (x m a s) (display "rnp") (display " -> ") (if (< x 1831) (quote accept) (quote accept))))
(define xmas-vdt (lambda (x m a s) (display "vdt") (display " -> ") (if (< x 769) (quote accept) (if (< a 3443) (xmas-cq x m a s) (if (> s 619) (quote reject) (xmas-cmd x m a s))))))
(define xmas-xnq (lambda (x m a s) (display "xnq") (display " -> ") (if (> m 3721) (quote accept) (quote reject))))
(define xmas-qcs (lambda (x m a s) (display "qcs") (display " -> ") (if (> s 3317) (quote reject) (if (< a 1598) (quote accept) (if (< s 2533) (quote accept) (quote accept))))))
(define xmas-vdf (lambda (x m a s) (display "vdf") (display " -> ") (if (< a 883) (quote accept) (if (> m 3275) (quote reject) (xmas-tbh x m a s)))))
(define xmas-xvp (lambda (x m a s) (display "xvp") (display " -> ") (if (< a 2548) (quote reject) (if (> x 1951) (quote accept) (if (> s 1071) (quote accept) (quote accept))))))
(define xmas-xjp (lambda (x m a s) (display "xjp") (display " -> ") (if (< a 76) (quote accept) (xmas-fsg x m a s))))
(define xmas-nm (lambda (x m a s) (display "nm") (display " -> ") (if (> s 2996) (xmas-cf x m a s) (xmas-skq x m a s))))
(define xmas-rkn (lambda (x m a s) (display "rkn") (display " -> ") (if (< s 3726) (quote accept) (quote accept))))
(define xmas-tgd (lambda (x m a s) (display "tgd") (display " -> ") (if (> x 2757) (quote reject) (xmas-xt x m a s))))
(define xmas-str (lambda (x m a s) (display "str") (display " -> ") (if (> x 3224) (quote accept) (if (> m 2530) (quote reject) (quote accept)))))
(define xmas-ht (lambda (x m a s) (display "ht") (display " -> ") (if (< m 2033) (xmas-fn x m a s) (quote accept))))
(define xmas-pf (lambda (x m a s) (display "pf") (display " -> ") (if (< x 659) (quote reject) (quote accept))))
(define xmas-kfx (lambda (x m a s) (display "kfx") (display " -> ") (if (> x 3352) (quote accept) (if (> s 3192) (quote reject) (if (< a 1007) (quote reject) (quote accept))))))
(define xmas-fb (lambda (x m a s) (display "fb") (display " -> ") (if (< x 3282) (xmas-mzr x m a s) (if (< a 202) (xmas-xjp x m a s) (if (> x 3702) (xmas-mq x m a s) (xmas-fbl x m a s))))))
(define xmas-hmm (lambda (x m a s) (display "hmm") (display " -> ") (if (< x 3203) (xmas-tgd x m a s) (if (< m 3096) (xmas-grd x m a s) (if (> m 3217) (xmas-lz x m a s) (xmas-np x m a s))))))
(define xmas-hr (lambda (x m a s) (display "hr") (display " -> ") (if (> m 2968) (quote reject) (quote reject))))
(define xmas-fvs (lambda (x m a s) (display "fvs") (display " -> ") (if (> x 1734) (quote accept) (if (> s 1359) (xmas-vr x m a s) (if (< a 3421) (xmas-bkc x m a s) (quote accept))))))
(define xmas-mx (lambda (x m a s) (display "mx") (display " -> ") (if (< m 1237) (quote reject) (if (< m 1330) (quote accept) (if (< m 1387) (quote accept) (quote accept))))))
(define xmas-dtj (lambda (x m a s) (display "dtj") (display " -> ") (if (< m 3403) (quote reject) (if (< m 3689) (quote reject) (if (< a 346) (quote accept) (quote accept))))))
(define xmas-cqs (lambda (x m a s) (display "cqs") (display " -> ") (if (< a 986) (quote reject) (if (> a 1025) (xmas-fs x m a s) (quote accept)))))
(define xmas-bv (lambda (x m a s) (display "bv") (display " -> ") (if (> s 2301) (xmas-hqk x m a s) (quote accept))))
(define xmas-nfp (lambda (x m a s) (display "nfp") (display " -> ") (if (> x 2587) (quote reject) (quote accept))))
(define xmas-mlc (lambda (x m a s) (display "mlc") (display " -> ") (if (< a 1713) (quote reject) (quote accept))))
(define xmas-lmr (lambda (x m a s) (display "lmr") (display " -> ") (if (< a 556) (quote accept) (if (> s 441) (quote reject) (if (< a 924) (quote reject) (quote reject))))))
(define xmas-xf (lambda (x m a s) (display "xf") (display " -> ") (if (< m 3026) (quote accept) (if (> a 912) (quote reject) (if (> m 3139) (quote accept) (quote reject))))))
(define xmas-vbq (lambda (x m a s) (display "vbq") (display " -> ") (if (> m 3572) (xmas-jtg x m a s) (if (> a 1680) (xmas-jm x m a s) (if (> x 1650) (xmas-pd x m a s) (xmas-pqh x m a s))))))
(define xmas-zmg (lambda (x m a s) (display "zmg") (display " -> ") (if (< x 2959) (quote reject) (if (< m 1833) (quote accept) (quote accept)))))
(define xmas-cx (lambda (x m a s) (display "cx") (display " -> ") (if (< a 1868) (quote accept) (if (> a 1928) (quote accept) (if (< m 412) (quote reject) (quote accept))))))
(define xmas-vfz (lambda (x m a s) (display "vfz") (display " -> ") (if (< s 720) (quote reject) (if (> m 3667) (quote reject) (if (< m 3658) (quote accept) (quote accept))))))
(define xmas-gz (lambda (x m a s) (display "gz") (display " -> ") (if (< s 1382) (quote accept) (quote accept))))
(define xmas-qlz (lambda (x m a s) (display "qlz") (display " -> ") (if (< m 2261) (quote accept) (if (> m 2278) (quote reject) (if (< x 1617) (quote reject) (quote reject))))))
(define xmas-lh (lambda (x m a s) (display "lh") (display " -> ") (if (< a 2877) (xmas-bfj x m a s) (if (< m 3524) (quote reject) (if (< s 3202) (xmas-tqd x m a s) (quote accept))))))
(define xmas-xlj (lambda (x m a s) (display "xlj") (display " -> ") (if (> a 1080) (xmas-gs x m a s) (if (< a 466) (xmas-bjt x m a s) (if (> a 868) (xmas-lq x m a s) (xmas-vbl x m a s))))))
(define xmas-gbh (lambda (x m a s) (display "gbh") (display " -> ") (if (> s 602) (quote accept) (quote reject))))
(define xmas-lvb (lambda (x m a s) (display "lvb") (display " -> ") (if (> m 3613) (quote reject) (if (> a 766) (quote reject) (quote accept)))))
(define xmas-hj (lambda (x m a s) (display "hj") (display " -> ") (if (> m 1236) (xmas-pk x m a s) (if (> m 1173) (quote accept) (quote accept)))))
(define xmas-nd (lambda (x m a s) (display "nd") (display " -> ") (if (< a 571) (quote accept) (if (> x 1952) (quote accept) (xmas-tss x m a s)))))
(define xmas-qrq (lambda (x m a s) (display "qrq") (display " -> ") (if (> s 2394) (quote reject) (quote reject))))
(define xmas-ltm (lambda (x m a s) (display "ltm") (display " -> ") (if (> a 3281) (xmas-qd x m a s) (if (> m 2147) (xmas-dbd x m a s) (xmas-pc x m a s)))))
(define xmas-pcv (lambda (x m a s) (display "pcv") (display " -> ") (if (< x 2504) (xmas-rmd x m a s) (quote reject))))
(define xmas-vx (lambda (x m a s) (display "vx") (display " -> ") (if (> m 1249) (quote reject) (if (< m 1122) (quote accept) (quote accept)))))
(define xmas-kk (lambda (x m a s) (display "kk") (display " -> ") (if (> a 102) (quote accept) (quote reject))))
(define xmas-hrm (lambda (x m a s) (display "hrm") (display " -> ") (if (< x 3482) (quote reject) (quote reject))))
(define xmas-rb (lambda (x m a s) (display "rb") (display " -> ") (if (< m 3705) (xmas-mlc x m a s) (quote accept))))
(define xmas-ct (lambda (x m a s) (display "ct") (display " -> ") (if (> m 3575) (quote reject) (if (< s 923) (quote accept) (if (> m 3498) (quote reject) (quote reject))))))
(define xmas-bbl (lambda (x m a s) (display "bbl") (display " -> ") (if (< m 2954) (quote reject) (if (> x 1234) (quote accept) (if (< x 1131) (quote reject) (quote reject))))))
(define xmas-hzr (lambda (x m a s) (display "hzr") (display " -> ") (if (> a 444) (quote accept) (quote reject))))
(define xmas-smv (lambda (x m a s) (display "smv") (display " -> ") (if (< m 3555) (quote accept) (quote reject))))
(define xmas-hn (lambda (x m a s) (display "hn") (display " -> ") (if (> a 3443) (quote reject) (if (< s 3762) (quote accept) (quote reject)))))
(define xmas-bld (lambda (x m a s) (display "bld") (display " -> ") (if (< a 1301) (xmas-ql x m a s) (xmas-zh x m a s))))
(define xmas-mzr (lambda (x m a s) (display "mzr") (display " -> ") (if (> s 3296) (quote accept) (quote accept))))
(define xmas-pq (lambda (x m a s) (display "pq") (display " -> ") (if (> x 714) (quote accept) (if (> a 2725) (quote accept) (if (> s 3201) (quote accept) (quote reject))))))
(define xmas-vc (lambda (x m a s) (display "vc") (display " -> ") (if (< m 1661) (quote accept) (if (< s 2236) (quote reject) (if (> m 1963) (quote accept) (quote reject))))))
(define xmas-fl (lambda (x m a s) (display "fl") (display " -> ") (if (< a 3463) (quote reject) (quote reject))))
(define xmas-pt (lambda (x m a s) (display "pt") (display " -> ") (if (< s 3499) (xmas-jsf x m a s) (xmas-fj x m a s))))
(define xmas-xs (lambda (x m a s) (display "xs") (display " -> ") (if (> a 1018) (xmas-gqs x m a s) (if (< x 2524) (xmas-sbx x m a s) (if (> m 3346) (xmas-th x m a s) (quote reject))))))
(define xmas-rp (lambda (x m a s) (display "rp") (display " -> ") (if (> s 3756) (quote reject) (quote reject))))
(define xmas-rs (lambda (x m a s) (display "rs") (display " -> ") (if (< m 1607) (quote reject) (if (< x 1267) (quote accept) (if (> m 1707) (quote reject) (quote accept))))))
(define xmas-mmz (lambda (x m a s) (display "mmz") (display " -> ") (if (< x 746) (quote accept) (if (< m 1213) (quote reject) (if (< m 1326) (quote reject) (quote accept))))))
(define xmas-fhn (lambda (x m a s) (display "fhn") (display " -> ") (if (< x 3152) (quote accept) (if (< s 2125) (quote accept) (if (< x 3527) (quote reject) (quote accept))))))
(define xmas-dx (lambda (x m a s) (display "dx") (display " -> ") (if (> a 893) (quote accept) (if (> s 3322) (quote accept) (quote reject)))))
(define xmas-np (lambda (x m a s) (display "np") (display " -> ") (if (< a 206) (quote reject) (xmas-ccs x m a s))))
(define xmas-xjt (lambda (x m a s) (display "xjt") (display " -> ") (if (< a 2267) (quote accept) (if (> a 2500) (quote reject) (quote reject)))))
(define xmas-bz (lambda (x m a s) (display "bz") (display " -> ") (if (< s 2656) (xmas-svg x m a s) (xmas-pt x m a s))))
(define xmas-bjt (lambda (x m a s) (display "bjt") (display " -> ") (if (> s 2419) (xmas-gq x m a s) (if (< m 3336) (xmas-hmm x m a s) (xmas-dxh x m a s)))))
(define xmas-pdn (lambda (x m a s) (display "pdn") (display " -> ") (if (> x 3155) (xmas-mnc x m a s) (quote reject))))
(define xmas-nqq (lambda (x m a s) (display "nqq") (display " -> ") (if (< a 653) (xmas-rnp x m a s) (quote reject))))
(define xmas-kjx (lambda (x m a s) (display "kjx") (display " -> ") (if (< s 660) (quote accept) (quote reject))))
(define xmas-rqj (lambda (x m a s) (display "rqj") (display " -> ") (if (> m 1961) (quote accept) (if (> a 1896) (quote reject) (quote reject)))))
(define xmas-hq (lambda (x m a s) (display "hq") (display " -> ") (if (< m 240) (quote reject) (if (< a 1873) (xmas-kfh x m a s) (if (> a 1916) (xmas-vj x m a s) (quote accept))))))
(define xmas-btp (lambda (x m a s) (display "btp") (display " -> ") (if (> m 271) (quote reject) (if (< m 99) (quote reject) (quote reject)))))
(define xmas-bkc (lambda (x m a s) (display "bkc") (display " -> ") (if (> x 1069) (quote reject) (if (> m 2900) (quote accept) (if (> m 2668) (quote reject) (quote accept))))))
(define xmas-tlf (lambda (x m a s) (display "tlf") (display " -> ") (if (> x 1415) (quote accept) (if (< s 1196) (xmas-bbl x m a s) (if (< m 2985) (quote reject) (quote reject))))))
(define xmas-rmd (lambda (x m a s) (display "rmd") (display " -> ") (if (< x 2173) (quote reject) (if (> m 3498) (quote accept) (quote accept)))))
(define xmas-ndl (lambda (x m a s) (display "ndl") (display " -> ") (if (< m 1968) (quote reject) (if (> a 1791) (quote reject) (quote reject)))))
(define xmas-ml (lambda (x m a s) (display "ml") (display " -> ") (if (< a 771) (xmas-snd x m a s) (quote reject))))
(define xmas-tqs (lambda (x m a s) (display "tqs") (display " -> ") (if (> a 2458) (xmas-tpl x m a s) (if (> s 1393) (xmas-sj x m a s) (xmas-gb x m a s)))))
(define xmas-gvf (lambda (x m a s) (display "gvf") (display " -> ") (if (> x 914) (quote reject) (if (< a 781) (quote accept) (quote accept)))))
(define xmas-cj (lambda (x m a s) (display "cj") (display " -> ") (if (< x 1186) (xmas-lsd x m a s) (xmas-lg x m a s))))
(define xmas-cmp (lambda (x m a s) (display "cmp") (display " -> ") (if (> s 2743) (xmas-jg x m a s) (if (< a 860) (quote reject) (if (> m 312) (quote reject) (quote accept))))))
(define xmas-hgf (lambda (x m a s) (display "hgf") (display " -> ") (if (< m 3130) (xmas-vq x m a s) (if (> m 3236) (quote accept) (xmas-qcs x m a s)))))
(define xmas-vhq (lambda (x m a s) (display "vhq") (display " -> ") (if (< s 2884) (quote accept) (if (< a 584) (quote reject) (if (< x 224) (quote accept) (quote accept))))))
(define xmas-dbd (lambda (x m a s) (display "dbd") (display " -> ") (if (> x 1077) (xmas-rp x m a s) (if (> a 2743) (quote reject) (xmas-xjt x m a s)))))
(define xmas-fpr (lambda (x m a s) (display "fpr") (display " -> ") (if (< x 2722) (quote accept) (if (> a 1735) (quote reject) (if (< a 1670) (quote reject) (quote accept))))))
(define xmas-mfx (lambda (x m a s) (display "mfx") (display " -> ") (if (> a 500) (quote accept) (quote accept))))
(define xmas-kq (lambda (x m a s) (display "kq") (display " -> ") (if (< m 1491) (xmas-gcp x m a s) (if (> s 3146) (xmas-zxf x m a s) (if (< s 2894) (xmas-qcg x m a s) (xmas-pdn x m a s))))))
(define xmas-db (lambda (x m a s) (display "db") (display " -> ") (if (< x 945) (quote reject) (quote reject))))
(define xmas-lp (lambda (x m a s) (display "lp") (display " -> ") (if (> x 2173) (quote accept) (quote reject))))
(define xmas-fs (lambda (x m a s) (display "fs") (display " -> ") (if (> s 2533) (quote reject) (if (< x 2621) (quote accept) (quote reject)))))
(define xmas-nn (lambda (x m a s) (display "nn") (display " -> ") (if (< x 1662) (xmas-mp x m a s) (xmas-bb x m a s))))
(define xmas-tq (lambda (x m a s) (display "tq") (display " -> ") (if (< s 2467) (quote reject) (if (< m 3674) (quote accept) (quote reject)))))
(define xmas-nb (lambda (x m a s) (display "nb") (display " -> ") (if (> s 529) (quote reject) (if (< x 1923) (quote reject) (if (> s 292) (quote accept) (quote accept))))))
(define xmas-ztp (lambda (x m a s) (display "ztp") (display " -> ") (if (> a 1837) (quote accept) (quote reject))))
(define xmas-lsd (lambda (x m a s) (display "lsd") (display " -> ") (if (< a 1724) (xmas-jmb x m a s) (if (< x 412) (xmas-btc x m a s) (if (< s 1937) (xmas-cxd x m a s) (xmas-xsc x m a s))))))
(define xmas-prq (lambda (x m a s) (display "prq") (display " -> ") (if (> s 1741) (xmas-cqs x m a s) (if (> a 956) (xmas-xs x m a s) (if (< s 904) (xmas-qsz x m a s) (xmas-pcv x m a s))))))
(define xmas-fx (lambda (x m a s) (display "fx") (display " -> ") (if (< s 3176) (xmas-ck x m a s) (if (< s 3348) (xmas-dq x m a s) (xmas-crk x m a s)))))
(define xmas-hvp (lambda (x m a s) (display "hvp") (display " -> ") (if (< a 1406) (xmas-xh x m a s) (if (< x 3381) (quote accept) (if (< m 2017) (quote accept) (quote accept))))))
(define xmas-cd (lambda (x m a s) (display "cd") (display " -> ") (if (> x 1361) (quote reject) (quote accept))))
(define xmas-rrx (lambda (x m a s) (display "rrx") (display " -> ") (if (< s 3496) (quote reject) (if (< a 348) (quote reject) (quote reject)))))
(define xmas-grd (lambda (x m a s) (display "grd") (display " -> ") (if (< a 238) (xmas-hrm x m a s) (if (> x 3713) (xmas-st x m a s) (if (> m 2992) (quote reject) (xmas-thh x m a s))))))
(define xmas-ntj (lambda (x m a s) (display "ntj") (display " -> ") (if (> s 3380) (quote accept) (quote reject))))
(define xmas-dn (lambda (x m a s) (display "dn") (display " -> ") (if (< a 2690) (quote accept) (quote accept))))
(define xmas-gq (lambda (x m a s) (display "gq") (display " -> ") (if (< a 278) (xmas-vt x m a s) (if (< a 382) (xmas-blv x m a s) (if (> s 3001) (xmas-cmc x m a s) (xmas-sjv x m a s))))))
(define xmas-gt (lambda (x m a s) (display "gt") (display " -> ") (if (< x 1389) (quote reject) (if (> s 897) (quote accept) (quote reject)))))
(define xmas-lb (lambda (x m a s) (display "lb") (display " -> ") (if (< s 2723) (quote reject) (quote accept))))
(define xmas-sj (lambda (x m a s) (display "sj") (display " -> ") (if (> x 1458) (xmas-zk x m a s) (if (< s 1859) (quote reject) (xmas-bhl x m a s)))))
(define xmas-nvs (lambda (x m a s) (display "nvs") (display " -> ") (if (< s 1811) (quote reject) (quote reject))))
(define xmas-pvs (lambda (x m a s) (display "pvs") (display " -> ") (if (< s 3335) (quote reject) (if (> a 628) (quote reject) (quote reject)))))
(define xmas-ntv (lambda (x m a s) (display "ntv") (display " -> ") (if (> m 3686) (xmas-gx x m a s) (if (< s 1154) (xmas-vfz x m a s) (xmas-zqf x m a s)))))
(define xmas-rph (lambda (x m a s) (display "rph") (display " -> ") (if (> x 3245) (xmas-qs x m a s) (if (< s 3035) (quote reject) (if (> x 2642) (xmas-lgz x m a s) (xmas-sgt x m a s))))))
(define xmas-ltt (lambda (x m a s) (display "ltt") (display " -> ") (if (> m 3838) (quote accept) (if (> m 3769) (quote accept) (quote accept)))))
(define xmas-pvh (lambda (x m a s) (display "pvh") (display " -> ") (if (> m 2607) (xmas-mjq x m a s) (if (> s 2408) (xmas-hfl x m a s) (xmas-vp x m a s)))))
(define xmas-jmb (lambda (x m a s) (display "jmb") (display " -> ") (if (< m 2232) (quote accept) (if (< m 2528) (xmas-blg x m a s) (xmas-nh x m a s)))))
(define xmas-fbl (lambda (x m a s) (display "fbl") (display " -> ") (if (> s 3421) (quote reject) (if (> x 3561) (quote reject) (quote accept)))))
(define xmas-bgt (lambda (x m a s) (display "bgt") (display " -> ") (if (> a 1011) (xmas-qcc x m a s) (if (< s 2773) (quote accept) (xmas-qv x m a s)))))
(define xmas-mk (lambda (x m a s) (display "mk") (display " -> ") (if (< s 3271) (xmas-csg x m a s) (if (< a 971) (xmas-rkn x m a s) (if (< m 1911) (xmas-zmg x m a s) (xmas-pqm x m a s))))))
(define xmas-ktm (lambda (x m a s) (display "ktm") (display " -> ") (if (< a 1247) (xmas-cbn x m a s) (xmas-dbh x m a s))))
(define xmas-dd (lambda (x m a s) (display "dd") (display " -> ") (if (< m 3002) (quote reject) (if (< a 1464) (quote accept) (quote accept)))))
(define xmas-nfh (lambda (x m a s) (display "nfh") (display " -> ") (if (< m 3471) (xmas-tbx x m a s) (if (< m 3540) (quote accept) (quote reject)))))
(define xmas-dz (lambda (x m a s) (display "dz") (display " -> ") (if (> a 3220) (quote reject) (if (> m 2286) (quote reject) (if (> x 3358) (quote reject) (quote accept))))))
(define xmas-bm (lambda (x m a s) (display "bm") (display " -> ") (if (> m 1464) (xmas-grp x m a s) (if (> x 1525) (xmas-cn x m a s) (xmas-cmb x m a s)))))
(define xmas-tv (lambda (x m a s) (display "tv") (display " -> ") (if (< x 2655) (quote accept) (if (> a 3616) (quote reject) (quote accept)))))
(define xmas-fbh (lambda (x m a s) (display "fbh") (display " -> ") (if (< a 948) (quote accept) (quote reject))))
(define xmas-jcc (lambda (x m a s) (display "jcc") (display " -> ") (if (< m 2572) (quote reject) (if (> m 2595) (quote accept) (if (> s 252) (quote reject) (quote accept))))))
(define xmas-ftg (lambda (x m a s) (display "ftg") (display " -> ") (if (< s 3673) (quote accept) (if (> x 3286) (quote reject) (if (< m 1558) (quote accept) (quote accept))))))
(define xmas-dq (lambda (x m a s) (display "dq") (display " -> ") (if (> s 3243) (xmas-rjz x m a s) (if (< a 3153) (xmas-pq x m a s) (xmas-bg x m a s)))))
(define xmas-gb (lambda (x m a s) (display "gb") (display " -> ") (if (> a 2230) (quote reject) (if (< s 668) (xmas-zc x m a s) (xmas-gc x m a s)))))
(define xmas-pp (lambda (x m a s) (display "pp") (display " -> ") (if (< m 1968) (xmas-llt x m a s) (xmas-rht x m a s))))
(define xmas-zj (lambda (x m a s) (display "zj") (display " -> ") (if (> m 3662) (xmas-lv x m a s) (if (< m 3521) (xmas-hgv x m a s) (if (> s 1468) (xmas-qbd x m a s) (quote accept))))))
(define xmas-xn (lambda (x m a s) (display "xn") (display " -> ") (if (< a 1704) (xmas-sg x m a s) (if (< m 680) (xmas-hq x m a s) (if (< a 1861) (xmas-lbp x m a s) (xmas-psz x m a s))))))
(define xmas-sc (lambda (x m a s) (display "sc") (display " -> ") (if (> x 3263) (xmas-fz x m a s) (if (> a 688) (xmas-hrd x m a s) (xmas-kp x m a s)))))
(define xmas-cks (lambda (x m a s) (display "cks") (display " -> ") (if (< m 3797) (quote reject) (if (> a 741) (quote accept) (quote accept)))))
(define xmas-cmc (lambda (x m a s) (display "cmc") (display " -> ") (if (> m 3299) (quote accept) (if (> x 2953) (quote reject) (xmas-lsf x m a s)))))
(define xmas-xz (lambda (x m a s) (display "xz") (display " -> ") (if (> s 683) (quote reject) (if (> a 2623) (quote accept) (quote accept)))))
(define xmas-mt (lambda (x m a s) (display "mt") (display " -> ") (if (< m 826) (quote accept) (quote reject))))



