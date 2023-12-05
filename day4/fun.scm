
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
	 )
    lines-no))


(define (score card)
  (let ((have (third card))
	(win (second card))
	(score 0))    
    (do-list (h have)
	     (cond
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
#;1431> (total-score (input))
(512 512 1 1 2 4 512 512 16 2 1 8 16 2 8 0 2 0 0 0 512 512 512 512 32 512 512 8 16 1 512 512 1 4 4 32 32 8 4 8 1 1 1 0 4 4 1 128 256 4 0 8 2 2 2 2 1 1 0 0 512 64 512 512 512 512 128 512 512 32 2 32 64 1 32 0 4 4 1 0 1 0 128 512 1 16 64 32 4 8 32 8 4 2 2 2 1 0 512 512 512 16 32 256 4 4 2 8 1 8 2 2 1 0 0 256 64 512 512 16 512 512 256 512 2 512 32 512 0 512 2 32 64 1 4 8 2 2 1 0 512 512 512 2 512 512 0 32 8 1 4 2 1 1 1 0 2 1 0 0 4 512 0 4 512 0 256 256 1 64 64 2 0 8 4 1 0 0 512 256 2 128 256 512 64 128 256 1 4 8 16 8 1 0 1 0)
(tot-score 24706)

|#




