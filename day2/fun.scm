
(import scheme)

(import simple-exceptions)

(import (chicken string))
(import (chicken pretty-print))
(define pp pretty-print)

(import (chicken format))
(import (chicken sort))
(import (chicken file))
(import (chicken process-context))
;; (change-directory "../day2")
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
split input based on newline

split-newline
splpit

|#
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


(define input (convert input))

(define (allow? xs)
  (letrec ((fassoc (lambda (x ys nballs)
		     (let ((val (assoc x ys)))
		       (cond
			(val (<= (second val) nballs))
			(#t #t))))))
    (and (fassoc 'red xs 12)
	 (fassoc 'green xs 13)
	 (fassoc 'blue xs 14)
	 )))
			    
	   
(define (allow-game? g)
  (let* ((game-no (second (first g)))
	 (all (filter not (map allow? (rest g)))))
    (cond
     ((null? all) game-no)
     (#t 0))))


(define (part-1)
  (map allow-game? input))

(define (sol-1)
  (apply + (part-1)))

#|
bug - 1
got wrong number of coloured balls in allow?
green as  14 , blue as 13 

#;3630> ,t (sol-1)
0.001s CPU time, 3865/184 mutations (total/tracked), 0/14 GCs (major/minor), maximum live heap: 1.45 MiB
2512


|#

	   
	   
