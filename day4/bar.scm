
(import scheme)

(import simple-exceptions)

(import (chicken string))
(import (chicken pretty-print))
(define pp pretty-print)

(import (chicken format))
(import (chicken sort))
(import (chicken file))
(import (chicken process-context))
;; (change-directory "day4")
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
in the abscence of an easy to use parser 
we resort to regexp string splitting
|#

(define (example)
  "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")

(define (parse dat)
  (let* ((in dat)
	 (lines (string-split in "\n"))
	 (lines-bar (map (lambda (x) (string-split x "|")) lines))
	 (lines-card (map (lambda (x) (list (string-split (car x) ":") (cdr x))) lines-bar))
	 (lines-no (map (lambda (x) (append (list (cons 'card 
							(list (string->number (car (string-split (caar x) "Card ")))))
						  (map string->number (string-split (cadar x) " ")))
					    (list (map string->number
						       (string-split (car (car (cdr x))) " ")))))
			lines-card))
	 (dummy-card     '((card 0)()()))
	 )
    (cons dummy-card lines-no)))



;; card 0 
;; id=0 is a dummy card simply there to make vector access easier
(define (score card)
  (let ((have (third card))
	(id (second (first card)))
	(win (second card))
	(score 0))    
    (do-list (h have)
	     (cond
	      ((= id 0) #f)
	      ((and (= score 0) (member h win))
	       (set! score 1))
	      ((member h win)
	       (set! score (* score 2)))))
    score))


(define (total-score dat)
  (let* ((p (parse dat))
	 (z (map score p)))
    (values z (list 'tot-score (apply + z)))))

(define (part-1)
  (total-score (input)))

(define (example-1)
  (total-score (example)))

#|

|#


;; card 0 
;; id=0 is a dummy card simply there to make vector access easier
(define (winnings card)
  (let ((have (third card))
	(id (second (first card)))
	(win (second card))
	(res '()))    
    (do-list (h have)
	     (cond
	      ((= id 0) #f)
	      ((member h win)
	       (set! res (cons h res)))))
    res))

(define (next-cards n k)
  (let ((res '()))
    (do-for (j 0 k)
	    (let ((val (+ n j 1)))
	      (set! res (cons val res))))
    (reverse res)))

;;(next-cards 1 4) => (2 3 4 5)
;;(next-cards 1 1)  (2)
;;(next-cards 1 0)  ()
;; (next-cards 3 2)  (4 5)
;; (next-cards 2 2)  (3 4)


(define (all-wins dat)
  (let* ((p (parse dat))
	 (z (map (lambda (x) 
		   (list (car x) (winnings x))) p))
	 (s (map (lambda (x)
		   (match x 
		     ((('card no) wins)
		      ;;(format #t "no ~a  : wins ~a ~%" no wins)
		      (let ((todo (next-cards no (length wins))))
		      `((card ,no) ,todo)))
		     (_ (error "no match"))))
		 z)))
    s))

;; rec is a macro in chicken-scheme
;; cut 
;; 



#|
api 

from the example
wins = list of all cards
wins = (() (2 3 4 5) (3 4) (4 5) (5) () ())

(do-list (w wins)
            w = (2 3 4 5)
                

|#
(define (rec2 dat)  
  (let* ((wins (map second (all-wins dat)))
	 (vec (make-vector (+ 1 (length wins)) 0)) ;; all initial cards have them already
	 (res '()))
    (letrec ((inc (lambda (k)
		    (vector-set! vec k (+ 1 (vector-ref vec k)))))
	     (winner? (lambda (n)
			(not (null? (elt wins n)))))
	     (bar (lambda (xs)
		    (do-list (x xs)
			     (let ((w (elt wins x)))
			       ;;(format #t "winner = ~a ~%" x)
			       ;;(format #t "copy = ~a ~%" x)
			       (inc x)
			       (do-list (k w)
					(inc k) ;;
					(foo wins k (+ k 1)))
			       ;;(bar w)
			       ))))		      	     
	     (foo (lambda (xs i lim)
		    (cond
		     ((>= i lim) #f)
		     (#t
		      (let ((w (elt wins i)))
			(format #t "foo : ~a of ~a ~%" i lim)
			;;(format #t "w = ~a ~%" w)
			;;(set! tot (+ tot 1))
			(bar w)
			)
		      (foo xs (+ i 1) lim))))))
      ;; we hold 1 of each card initially - except 0 dummy card
      (do-list (v (map 1+ (iota (length (cdr wins)))))
	       (inc v))    
      (foo wins 0 (length wins))
      (format #t "vec = ~a ~%" vec)
      (let* ((tot-list (vector->list vec))
	     (sum  (apply + tot-list)))
	(format #t "tot-list ~a ~%" tot-list)
	(format #t "sum ~a ~%" sum)
	(values tot-list
		sum)))))



(define (part-2)
  (rec2 (input)))

;;(part-2)
	

#|
vec = #(0 1 2 4 8 12 16 32 52 104 192 384 767 956 1912 3720 7440 4761 9490 8482 1 1 2 4 8 16 32 64 128 256 512 1024 1519 2908 5812 8452 16904 33776 61676 114900 229800 459600 918176 1181753 1411554 1 2 4 8 11 20 40 40 80 140 280 480 772 1264 1265 1 1 2 4 8 16 32 64 128 256 510 1020 2039 4078 7132 14256 21300 20758 41516 80865 161474 55773 55774 1 2 4 8 12 24 48 96 192 375 702 1404 2674 5348 8215 13564 1 2 4 8 16 32 64 128 256 504 944 1487 2028 4052 7600 13140 1 1 2 4 8 16 32 64 128 256 510 1003 2006 3502 7000 6992 13984 27936 55680 95370 190484 285598 570193 1140386 2058850 3547507 1 2 4 8 16 32 56 56 112 224 448 671 1340 2564 2613 2646 1 2 4 1 1 2 4 4 7 14 14 24 48 96 144 288 574 574 860 1713 3412 1579 1 2 4 8 16 28 56 112 224 448 896 1341 2674 5348 9728 19456 8247 16494 0) 
tot-list (0 1 2 4 8 12 16 32 52 104 192 384 767 956 1912 3720 7440 4761 9490 8482 1 1 2 4 8 16 32 64 128 256 512 1024 1519 2908 5812 8452 16904 33776 61676 114900 229800 459600 918176 1181753 1411554 1 2 4 8 11 20 40 40 80 140 280 480 772 1264 1265 1 1 2 4 8 16 32 64 128 256 510 1020 2039 4078 7132 14256 21300 20758 41516 80865 161474 55773 55774 1 2 4 8 12 24 48 96 192 375 702 1404 2674 5348 8215 13564 1 2 4 8 16 32 64 128 256 504 944 1487 2028 4052 7600 13140 1 1 2 4 8 16 32 64 128 256 510 1003 2006 3502 7000 6992 13984 27936 55680 95370 190484 285598 570193 1140386 2058850 3547507 1 2 4 8 16 32 56 56 112 224 448 671 1340 2564 2613 2646 1 2 4 1 1 2 4 4 7 14 14 24 48 96 144 288 574 574 860 1713 3412 1579 1 2 4 8 16 28 56 112 224 448 896 1341 2674 5348 9728 19456 8247 16494 0) 

sum 
13114317 

accepted answer.

probably one of the worst spec'd puzzles seen so far 




|#
















#|

|#
;;(define rec rec2)





;; (define (vec-wins dat)
;;   (list->vector (map second (all-wins dat))))


#|
not sure how they are counting this ...

|#




