
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
			    
	   
(define (power-set g)
  (letrec ((fassoc (lambda (x ys)
		     (let ((val (assoc x ys)))
		       (cond
			(val (second val))
			(#t 0))))))
    (let* ((get-reds (apply max (map (lambda (ys) (fassoc 'red ys)) (rest g))))
	   (get-blues (apply max (map (lambda (ys) (fassoc 'blue ys)) (rest g))))
	   (get-greens (apply max (map (lambda (ys) (fassoc 'green ys)) (rest g))))
	   (pwr (* get-reds get-blues get-greens)))
      (format #t "~a ~% ~a : ~a : ~a : ~a ~%" g get-reds get-blues get-greens pwr)
      pwr)))



(define (part-2)
  (apply + (map power-set input)))

#|
0.011s CPU time, 6877/171 mutations (total/tracked), 0/18 GCs (major/minor), maximum live heap: 1.5 MiB
67335

|#


