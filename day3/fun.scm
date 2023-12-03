
(import scheme)

(import simple-exceptions)

(import (chicken string))
(import (chicken pretty-print))
(define pp pretty-print)

(import (chicken format))
(import (chicken sort))
(import (chicken file))
(import (chicken process-context))
;; (change-directory "../day3")
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

(define (input) (get-input))

(define (1+ x) (+ x 1))
(define (1- x) (- x 1))

(define (10+ x) (+ x 10))
(define (10- x) (- x 10))

(define rest cdr)

;; ---------------------------------------------------------------------



#|
split input based on newline

split-newline
splpit


(define (convert in) 
  (letrec ((split-newline (lambda (xs)
			    (string-split xs "\n")))
	   (split-colon (lambda (xs)
			    (string-split xs ":")))
	   (split-semicolon (lambda (xs)
			    (string-split xs ";")))
	   (split-comma (lambda (xs)
			    (string-split xs ",")))
	   (split-space (lambda (xs)
			  (string-split xs " ")))
	   (faz (lambda (xs) ;; "Game 100: 3 red, 3 blue, 10 green; 3 green, 1 blue, 6 red; 5 red, 4 green, 7 blue"
		  (let* ((games (split-newline xs)))
		    (map fbz games))))
	   (fbz (lambda (game) ;; "Game 100:" "3 red, 3 blue, 10 green; 3 green, 1 blue, 6 red; 5 red, 4 green, 7 blue"
		  (let* ((pair (split-colon game))
			 (hd (list 'game (string->number (first (string-split (car pair) "Game ")))))
			 (entries (fcz (second pair))))
		    (cons hd entries))))
	   (fcz (lambda (game) 
		  (let* ((bags (split-semicolon game)))
		    (map fdz bags))))
	   (fdz (lambda (game) ;; "3 red, 3 blue, 10 green"
		  (let* ((bags (split-comma game)))
		    (map fez bags))))
	   (fez (lambda (game) ;; "3 red"
		  (let* ((bag (split-space game )))
		    (ffz bag))))
	   (ffz (lambda (game) ;; "3 red"
		  (let* ((n (string->number (first game)))
			 (col (second (assoc (second game) '(("red" red)("green" green)("blue" blue))))))
		    (list col n)))))
    (faz in)))
|#




#|
 140 x 140 
grid of characters 
|#

(define (example)
  "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")


(define (process in)    
  (list->vector (map (lambda (x) (list->vector (string->list x))) 
		     (string-split in "\n"))))



#|

vector of characters 
symbols presumably non digits #\0 

(define (scan xs)
  (let* ((in (process xs))
	 (lim (vector-length in))
	 (j -1)
	 (k -1))
    (letrec ((two-d (lambda (v x y)(vector-ref (vector-ref v y) x)))
	     (get-c (lambda (x y)  (two-d in x y)))
	     (is-digit? (lambda (ch) (and (char>=? ch #\0) (char<=? ch #\9))))
	     (vert (lambda ()
		     (cond
		      ((>= k (- lim 1)) #f)
		      (#t 
		       (set! k (+ k 1))
		       (set! j -1)
		       (horz)
		       (vert )))))
	     (horz (lambda ()
		     (set! j (+ j 1))
		     (cond
		      ((>= j lim) (vert))
		      (#t
		       (let ((ch (get-c j k)))
			 (cond
			  ((is-digit? ch)
			   (letrec ((found (lambda (from)
					     (let ((to from))
					       (call/cc (lambda (end)
							  (do-while (< j lim)
							  (set! ch (get-c j k))
							  (cond
							   ((is-digit? ch)
							    (set! to j)
							    (set! j (+ j 1))
							    (when (>= j (- lim 1))
							      (end to)))
							   (#t (end to))))))
					       (format #t "found ~a - ~a : ~a ~%" from to k)
					       #t))))
			   (found j)
			   ;;(set! j (+ j 1))
			   ))
			  (#t 
			   ;;(set! j (+ j 1))
			   (horz)))))))))
      (vert))))
|#

;; (scan-digits (example))
;; (scan-digits (input))


(define (scan-digits xs)
  (let* ((in (process xs))
	 (lim (vector-length in))
	 (j 0)
	 (k 0)
	 (res '()))
    (letrec ((two-d (lambda (v x y)(vector-ref (vector-ref v y) x)))
	     (get-c (lambda (x y)  (two-d in x y)))
	     (is-digit? (lambda (ch) (and (char>=? ch #\0) (char<=? ch #\9)))))
      (do-for (k 0 lim)
	      (do-for (j 0 lim)
		      (let ((ch (get-c j k)))
			(cond
			 ((is-digit? ch)
			(set! res (cons (list ch j k) res)))))))
      (reverse res))))


#|
     (char x y)  (char2 x2 y2) ...
 take 
 drop

|#

;; are two coordinates compatible ? 
(define (compat? a b)
  (cond
   ((and (pair? a) (pair? b))
    (match a
      ((ch1 x1 y1) (match b
		     ((ch2 x2 y2)			 
		      (and (= x2 (+ x1 1)) (= y1 y2)))
		     (_ (error "compat.a.b no match"))))
      (_ (error "compat. no match"))))
   (#t #f)))


(compat? '(#\4 0 0) '(#\6 1 0))
(compat? '(#\6 1 0) '(#\7 2 0))
(compat? '(#\1 5 0) '(#\1 6 0))

#|
(define (compat-lis xs)
  (define (compat-lis2 xs y ys)
    (cond
     ((null? xs) (reverse ys))
     ((compat (car xs) y) (compat-lis2 (cdr xs) (car xs) (cons (car xs) ys)))
     (#t (compat-lis xs))))
  (cond
   ((null? xs) xs)
   ((null? (cdr xs)) xs)
   ((compat (car xs) (cadr xs))
    (compat-lis2 (cdr (cdr xs)) (cadr xs) (list (cadr xs) (car xs))))
   (#t
    (cons (list (car xs)) (compat-lis (cdr xs))))))
|#

(define (compat-lis xs)
  (let ((prev #f)
	(cur #f)
	(res '()))
    (do-list (x xs)
	     ;;(format #t " x = ~a : prev = ~a : cur = ~a : ~%" x prev cur)
	     (cond
	      ((compat? prev x)
	       ;;(format #t "          compatible ~a ; ~a ~%" x prev)
	       (set! prev x)
	       (set! cur (cons x cur)))
	      (cur
	       ;;(format #t "          not compatible ~a ; ~a ~%" x prev)
	       (set! res (cons (reverse cur) res))
	       (set! prev x)
	       (set! cur (list x)))
	      (#t 
	       ;;(format #t "         default case ~a ; ~a ~%" x prev)
	       (set! cur (list x))
	       (set! prev x))))
    (cond
     (cur (set! res (cons (reverse cur) res))))
    (set! res (reverse res))
    res))

    
(define (flat-compat-lis xs)
  (let ((cs (compat-lis xs)))
    (map (lambda (r)
	   (let ((val (string->number (list->string (map first r)))))
	     (append (list val (cdr (first r)) (cdr (last r))))))
	 cs)))

;; (pp (flat-compat-lis (scan-digits (example))))

;; in is a 2d vector
;; x 
(define (part-number? p in)
  (let ((lim-x (vector-length (vector-ref in 0)))
	(lim-y (vector-length in)))
    (letrec ((two-d (lambda (v x y)(vector-ref (vector-ref v y) x)))
	     (get-c (lambda (x y)  (two-d in x y)))
	     (is-digit? (lambda (ch) (and (char>=? ch #\0) (char<=? ch #\9))))
	     (is-dot? (lambda (ch) (char=? ch #\.)))
	     (is-symbol? (lambda (ch) (not (or (is-dot? ch) (is-digit? ch))))))      
      ;;(format #t "in = ~a ~%" in)
      ;; (format #t "x = ~a ~%" x)
      (match p
	((val (x1 y1)(x2 y1))
	 ;;(format #t "matched ~a : x1 ~a : x2 ~a : y1 ~a ~%" val x1 x2 y1)
	 (call/cc (lambda (escape)
		    (do-for (j (- x1 1) (+ x2 1 1))
			     (do-for (k (- y1 1) (+ y1 1 1))
				      (cond
				       ((and (>= j 0) (< j lim-x)
					     (>= k 0) (< k lim-y)
					     (is-symbol? (get-c j k)))
					(escape #t)))))				    
		    #f)))
	(_ (error "part.number unmatched"))))))





;; (part-number? x (example))
;; (part-number? x (input))

;; bit weird way to write this ...
(define (part-1)
  (let* ((in (process (input)))
	 (flats (flat-compat-lis (scan-digits (input)))))
    (map (lambda (x)(cons (part-number? x in) x))
	 flats)))

(define (sol-1)
  (apply + 
  (map second (filter first (part-1)))))

#|

220721

rejectedd ...
bug -- do-for upper limit is exclusive not inclusive so only checking above or at same line level
(do-for (k (- y1 1) (+ y1 1))

should have read
(do-for (k (- y1 1) (+ y1 1 1))

#;4904> (sol-1)
536576


|#


(define (example-1)
  (let* ((in (process (example)))
	 (flats (flat-compat-lis (scan-digits (example)))))
    (map (lambda (x)(cons (part-number? x in) x))
	 flats)))

    
  






	 

#|
(define (collate xs)
  (let ((sd (scan-digits xs))
	(x 0)
	(y 0))
    (letrec ((foo (lambda (ys tmp)
		    (cond 
		     ((null? ys) (format #t "tmp = ~a ~%" tmp))
		     ((null? (cdr ys))



|#


(define (scan-symbols xs)
  (let* ((in (process xs))
	 (lim (vector-length in))
	 (j 0)
	 (k 0)
	 (res '()))
    (letrec ((two-d (lambda (v x y)(vector-ref (vector-ref v y) x)))
	     (get-c (lambda (x y)  (two-d in x y)))
	     (is-digit? (lambda (ch) (and (char>=? ch #\0) (char<=? ch #\9))))
	     (is-dot? (lambda (ch) (char=? ch #\.)))
	     (is-symbol? (lambda (ch) (not (or (is-dot? ch) (is-digit? ch))))))
      (do-for (k 0 lim)
	      (do-for (j 0 lim)
		      (let ((ch (get-c j k)))
			(cond
			 ((is-symbol? ch) 
			(set! res (cons (list ch j k) res)))))))
      (reverse res))))






	      








