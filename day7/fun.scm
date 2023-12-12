#|
(getcwd)
(chdir "day7")
|#

(use-modules (ice-9 format)) ;; format common lisp 
(use-modules (ice-9 pretty-print)) ;; pretty-print
(define pp pretty-print)
;;(use-modules (rnrs)) ;; assert 
(use-modules (srfi srfi-1)) ;; first second third ...
(use-modules (srfi srfi-2)) ;; first second third ...


(use-modules (rnrs)) ;; assert

;; regular expression
(use-modules (ice-9 regex)) 

;; pattern matcher ?
(use-modules (ice-9 match))

;; binary io -- dont know if this helped any as read-u8 is for reading ints no??
(use-modules (ice-9 binary-ports))

;; r7rs 
(use-modules (scheme base))

(define-macro (dolist varls . body)
  (let* ((fn (gensym "fn"))
	 (xs (gensym "xs"))
	 (var (car varls))
	 (ls  (car (cdr varls))))
    `(letrec ((,fn (lambda (,xs)
		     (cond
		      ((null? ,xs) #f)
		      (#t (let ((,var (car ,xs)))
			    ,@body
			    (,fn (cdr ,xs))))))))
       (,fn ,ls))))


#|

(defmacro while (condition . body)
  (let ((lup (gensym "loop")))
    `(letrec ((,lup (lambda ()
		      (when ,condition
			,@body
			(,lup)))))
       (,lup))))

|#

;; (let ((i 0))
;;   (while (< i 10)
;;     (format #t " i = ~a ~%" i )
;;     (set!  i (+ i 1))))

;; --------------------- macros --------------------------

#|
puzzle

|#


(define *debug* #f)

(define input #f)
(define input2 #f)

(define (get-input filename)  
  (call-with-port (open-input-file filename)
    (lambda (port)
      (read port))))


(set! input (get-input "input"))
;;(set! input (get-input "input2"))

(define example
  '(
    32T3K 765
	  T55J5 684
	  KK677 28
	  KTJJT 220
	  QQQJA 483
	  ))







#|

type of card

Five of a kind, where all five cards have the same label: AAAAA

Four of a kind, where four cards have the same label and one card has a different label: AA8AA

Full house, where three cards have the same label, and the remaining two cards share a different label: 23332

Three of a kind, where three cards have the same label, and the remaining two cards are each different from any other card in the hand: TTT98

Two pair, where two cards share one label, two other cards share a second label, and the remaining card has a third label: 23432

One pair, where two cards share one label, and the other three cards have a different label from the pair and each other: A23A4

High card, where all cards' labels are distinct: 23456

strength of card 
(let ((cards '(A  K  Q  J  T  9  8  7  6  5  4  3  2)))
(length cards))

(exp 13 5)

two hands of same type - then it goes down to who has strongest first card, second card ,third card ...

rank each hand from 1 strongest hand , to

|#


(define (string->hand sym)
  (letrec ((ch->symbol (lambda (ch)
			 (cond
			  ((char=? ch #\A) 14)
			  ((char=? ch #\K) 13)
			  ((char=? ch #\Q) 12)
			  ((char=? ch #\J) 11)
			  ((char=? ch #\T) 10)
			  ((char=? ch #\9) 9)
			  ((char=? ch #\8) 8)
			  ((char=? ch #\7) 7)
			  ((char=? ch #\6) 6)
			  ((char=? ch #\5) 5)
			  ((char=? ch #\4) 4)
			  ((char=? ch #\3) 3)
			  ((char=? ch #\2) 2)
			  (#t (error "unknown char" (list ch sym)))))))
    ;;(format #t "sym = ~a ~%" sym)
    (cond
     ((number? sym)
      (map ch->symbol (string->list (number->string sym))))
     (#t
      (map ch->symbol (string->list (symbol->string sym)))))))


(define (poop)
  (map (lambda (x) (list (string->hand (first x)) (second x))) input)
  )


(set! input (poop))


 

#|
(define (type hand)
    (match s
      ((c1 c2 c3 c4 c5)
       (cond
	((and (eqv c1 c2)(eqv c2 c3)(eqv c3 c4)(eqv c4 c5))
	 'five-of-a-kind)
	((and (eqv c1 c2)(eqv c2 c3)(eqv c3 c4)(eqv c4 c5))
	 'five-of-a-kind)
	(#t #t)))))
|#



#|
full house 3 same and 2 same

generate code identify four of a kind
three of a kind
two of a kind

|#
	 

#|

hand
0 1 2 3 4
5 cards 

|#

(define (without xs . vals)
  (define (without2 xs vals)
    (cond
     ((null? xs) xs)
     ((member (car xs) vals) (without2 (cdr xs) vals))
     (#t (cons (car xs) (without2 (cdr xs) vals)))))
  (without2 xs vals)) 


#|
(define (five-of-a-kind hand)
  (let ((s1 #f)
	(s2 #f)
	(s3 #f)
	(s4 #f)
	(s5 #f))
    (call/cc (lambda (escape)
	       (dolist (c1 '(0 1 2 3 4))
		       (dolist (c2 (without '(0 1 2 3 4) c1))
			       (dolist (c3 (without '(0 1 2 3 4) c1 c2))
				       (dolist (c4 (without '(0 1 2 3 4) c1 c2 c3))
					       (dolist (c5 (without '(0 1 2 3 4) c1 c2 c3 c4))
						       (set! s1 (list-ref hand c1))
						       (set! s2 (list-ref hand c2))
						       (set! s3 (list-ref hand c3))
						       (set! s4 (list-ref hand c4))
						       (set! s5 (list-ref hand c5))
						       (when (and (eq? s1 s2)
								  (eq? s2 s3)
								  (eq? s3 s4)
								  (eq? s4 s5))
							 (escape #t)))))))
	       #f))))
|#


(define-macro (hand-identifier name expr)
  `(define (,name hand)
     (let ((s1 #f)
	   (s2 #f)
	   (s3 #f)
	   (s4 #f)
	   (s5 #f))
       (call/cc (lambda (escape)
		  (dolist (c1 '(0 1 2 3 4))
			  (dolist (c2 (without '(0 1 2 3 4) c1))
				  (dolist (c3 (without '(0 1 2 3 4) c1 c2))
					  (dolist (c4 (without '(0 1 2 3 4) c1 c2 c3))
						  (dolist (c5 (without '(0 1 2 3 4) c1 c2 c3 c4))
							  (set! s1 (list-ref hand c1))
							  (set! s2 (list-ref hand c2))
							  (set! s3 (list-ref hand c3))
							  (set! s4 (list-ref hand c4))
							  (set! s5 (list-ref hand c5))
							  ,expr)))))
		  #f)))))



#|
five of a kind
all same values
|#
(hand-identifier five-of-kind? (when (and (eqv? s1 s2)
				  (eqv? s2 s3)
				  (eqv? s3 s4)
				  (eqv? s4 s5))
			 (escape #t)))

(five-of-kind? '(A A A A A))

(five-of-kind? '(A A A A 8))

#|
four of a kind 
four same value , one is different 
|#
(hand-identifier four-of-kind? (when (and (eqv? s1 s2)
				  (eqv? s2 s3)
				  (eqv? s3 s4)
				  (not (eqv? s4 s5)))
				 (escape #t)))

(four-of-kind? '(A A A A 8))

(four-of-kind? '(A J A A 8))

#|
full house
three same value , two other cards share different same value

1   1  1   2   2
s1 s2  s3  s4  s5


|#
(hand-identifier full-house? (when (and (eqv? s1 s2)
					(eqv? s1 s3)
					(not (eqv? s1 s4))
					(not (eqv? s1 s5))
					(eqv? s2 s3)
					(not (eqv? s2 s4))
					(not (eqv? s2 s5))
					(not (eqv? s3 s4))
					(not (eqv? s3 s5))
					(eqv? s4 s5))
			       (escape #t)))

(full-house? '(A A A 8 8))



#|
three kind 
three same value , two other cards differ from any other card

1   1  1   2   3
s1 s2  s3  s4  s5

s1 = s2
s2 = s3
s3 /= s4
s3 /= s5
s4 = s5


|#
(hand-identifier three-kind? (when (and (eqv? s1 s2)
					(eqv? s1 s3)
					(not (eqv? s1 s4))
					(not (eqv? s1 s5))					
					(eqv? s2 s3)
					(not (eqv? s2 s4))
					(not (eqv? s2 s5))
					(not (eqv? s3 s4))
					(not (eqv? s3 s5))
					(not (eqv? s4 s5)))
			       (escape #t)))

(three-kind? '(A A A 8 9))



#|
Two pair, where two cards share one label, two other cards share a second label, and the remaining card has a third label: 23432

1   1  2   2   3
s1 s2  s3  s4  s5

s1 = s2
s2 /= s3
s3 = s4
s4 /= s5

|#

(hand-identifier two-pair? (when (and (eqv? s1 s2)
				      (not (eqv? s2 s3))
				      (not (eqv? s2 s5))
				      (eqv? s3 s4)
				      (not (eqv? s4 s5)))
			     (escape #t)))


(two-pair? '(A A J J 9))


#|
One pair, where two cards share one label, and the other three cards have a different label from the pair and each other: A23A4

1   1  2   3   4
s1 s2  s3  s4  s5

s1 = s2
s2 /= s3
s3 /= s4
s4 /= s5

|#

(hand-identifier one-pair? (when (and (eqv? s1 s2)
				      (not (eqv? s2 s3))
				      (not (eqv? s2 s4))
				      (not (eqv? s2 s5))
				      (not (eqv? s3 s4))
				      (not (eqv? s3 s5))
				      (not (eqv? s4 s5)))
			     ;;(format #t "s1 s2 s3 s4 s5 : ~a ~a ~a ~a ~a ~%" s1 s2 s3 s4 s5)
			     (escape #t)))

(one-pair? '(A A 8 9 J))


#|
high card all different
|#

(hand-identifier high-card? (when (and
				   (not (eqv? s1 s2))
				   (not (eqv? s1 s3))
				   (not (eqv? s1 s4))
				   (not (eqv? s1 s5))
				   (not (eqv? s2 s3))
				   (not (eqv? s2 s4))
				   (not (eqv? s2 s5))
				   (not (eqv? s3 s4))
				   (not (eqv? s3 s5))
				   (not (eqv? s4 s5)))
			     ;;(format #t "s1 s2 s3 s4 s5 : ~a ~a ~a ~a ~a ~%" s1 s2 s3 s4 s5)
			     (escape #t)))

(high-card? '(A K Q J 10))

(define true? (lambda (x) (if x x #f)))

(define (classify hand)
  (filter true?
	  (list
	   (if (five-of-kind? hand) '(7 five-of-kind) #f)
	   (if (four-of-kind? hand) '(6 four-of-kind) #f)
	   (if (full-house? hand) '(5 full-house) #f)	   
	   (if (three-kind? hand) '(4 three-kind) #f)
	   (if (two-pair? hand) '(3 two-pair) #f)
	   (if (one-pair? hand) '(2 one-pair) #f)
	   (if (high-card? hand) '(1 high-card) #f))))



(classify '(A A A A A))


(define (enum)
  (let ((n 0)
	(fails 0)
	;;(cards '(A  K  Q  J  10  9  8  7  6  5  4  3  2))
	(cards '(14  13  12  11  10  9  8  7  6  5  4  3  2))
	)
    (dolist (c1 cards)
	    (format #t "c1 = ~a ~%" c1)
	    (dolist (c2 cards)
		    (dolist (c3 cards)
			    (dolist (c4 cards)
				    (dolist (c5 cards)
					    (set! n (1+ n))
					    (let* ((hand (list c1 c2 c3 c4 c5))
						   (type (classify hand)))
					      ;;(format #t "hand ~a : ~a : ~a~%"  n hand type)
					      (when (or (< (length type) 1) (> (length type) 1))
						(set! fails (+ 1 fails))
						;;(format #t "FAILL classify .~%")
						(format #t "FAIL . hand ~a : ~a : ~a~%"  n hand type)
						)))))))
    (format #t "failed to classify ~a hands of cards ~%" fails)))


(define (score)
  (map
   (lambda (x)
     (list (first (classify (first x)))
	   (first x)
	   (second x)))
   input))


#|
A    K   Q   J  10 9 8 7 6 5 4 3 2
14  13  12  11


|#

(define (stronger hand1 hand2)
  (cond
   ((null? hand1) #f)
   (#t
    (let ((c1 (car hand1))
	  (c2 (car hand2)))
      (cond
       ((eqv? c1 c2) (stronger (cdr hand1) (cdr hand2)))
       ((> c1 c2) #t)
       (#t #f))))))



(define (sort-by-strength xs)
  (sort xs (lambda (x y) (stronger (second x) (second y)))))



(define (sorted-input)
  (let ((scored (score)))
    (let ((sevens (sort-by-strength (filter (lambda (x) (equal? (first x) '(7 five-of-kind))) scored)))
	  (sixes (sort-by-strength (filter (lambda (x) (equal? (first x) '(6 four-of-kind))) scored)))
	  (fives (sort-by-strength (filter (lambda (x) (equal? (first x) '(5 full-house))) scored)))
	  (fours (sort-by-strength (filter (lambda (x) (equal? (first x) '(4 three-kind))) scored)))
	  (threes (sort-by-strength (filter (lambda (x) (equal? (first x) '(3 two-pair))) scored)))
	  (twos (sort-by-strength (filter (lambda (x) (equal? (first x) '(2 one-pair))) scored)))
	  (ones (sort-by-strength (filter (lambda (x) (equal? (first x) '(1 high-card))) scored)))
	  ;; fives
	  ;; fours
	  ;; threes
	  ;; twos
	  ;; ones
	  )
      (append sevens
	      sixes
	      fives
	      fours
	      threes
	      twos
	      ones
	      ))))


(define (rankings)
  (map list (sorted-input) (reverse (cdr (iota (+ 1 (length input)))))))

(define (bet-rankings)
  (map (lambda (x)
	 (list (last (first x)) (last x)))
       (rankings)))

(define (prod-bet-rankings)
  (map (lambda (x)
	 (* (last (first x)) (last x)))
       (rankings)))

(define (part-1)
  (apply + (prod-bet-rankings)))

#|
scheme@(#{ g8590}#) [12]> (part-1)

248396258


|#

