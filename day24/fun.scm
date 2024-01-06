
;;(import (lsp-server))

(import scheme)
(import (chicken process-context))
(import (chicken io))
(import (chicken format))
(import (chicken sort))
(import (chicken string))
(import (chicken pretty-print))
(import (chicken random))
(import (chicken time))
;;(import srfi-89)


;;(import srfi-89)
;; missing srfi-89 compatibility no egg for it ??

;;(define pp pretty-print)

;;(import (chicken doc))
;; documentation

;; debugging macro expander
;; debugger

(import procedural-macros)
(import regex)


(import simple-md5)

(import srfi-13)
;;srfi-13 for string=

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

(import simple-loops)
;; do-list

(import vector-lib)
;; vector-copy

#|
(current-directory)
(change-directory "day24")
|#

(define (1- x) (- x 1))
(define (1+ x) (+ x 1))


#|
find ourselves writing the same routines over and over again

1) read file
2) parse file - or rather not parse - then using reg expressions to make up for fact
cannot parse file
3) try grep and fail miserably

|#

(define (read-lines filename)
  (call-with-input-file filename
    (lambda (port)
      (let ((lines '()))
	(letrec ((loop (lambda ()
			 (let ((in (read-line port)))
			   (cond
			    ((eof-object? in) (reverse lines))
			    (#t (set! lines (cons in lines))
				(loop)))))))
	  (loop))))))

(define input (read-lines "input"))

#|
use a regexp
either match or empty string or err
|#

(let ((str " 2")
      (re (regexp "[^0-9]+[0-9]")))
  (string-match re str))

(let ((str "      2")
      (re (regexp "[^0-9]+[0-9]")))
  (string-match re str))

(let ((str "      2    ")
      (re (regexp "[^0-9]+[0-9][^0-9]+")))
  (string-match re str))


(let ((str "2")
      (re (regexp "[0-9]")))
  (string-match re str))

(let ((str "234567")
      (re (regexp "[0-9]+")))
  (string-match re str))

(let ((str "234567, 34568")
      (re (regexp "([0-9]+), ([0-9]+)")))
  (string-match re str))

(let ((str "234567, 34568, 45678")
      (re (regexp "([0-9]+), ([0-9]+), ([0-9]+)")))
  (string-match re str))

(let ((str "234567, 34568, 45678 @ -1")
      (re (regexp "([0-9]+), ([0-9]+), ([0-9]+) @ ([+-]?[0-9]+)")))
  (string-match re str))

(let ((str "234567, 34568, 45678 @ 1")
      (re (regexp "([0-9]+), ([0-9]+), ([0-9]+) @ ([+-]?[0-9]+)")))
  (string-match re str))

(let ((str "234567, 34568, 45678 @ +1")
      (re (regexp "([0-9]+), ([0-9]+), ([0-9]+) @ ([+-]?[0-9]+)")))
  (string-match re str))

(let ((str "234567, 34568, 45678 @ -1, -90, 145")
      (re (regexp "([0-9]+), ([0-9]+), ([0-9]+) @ ([+-]?[0-9]+), ([+-]?[0-9]+), ([+-]?[0-9]+)")))
  (string-match re str))

(define (parse-line s)
  (cond
   ((< (string-length s) 1) #f)
   (#t      
    (let ((re (regexp "([0-9]+), ([0-9]+), ([0-9]+) @ ([+-]?[0-9]+), ([+-]?[0-9]+), ([+-]?[0-9]+)")))
      (let ((me (string-match re s)))
	(cond
	 (me 
	  (let ((res  (map (lambda (x) (string->number x 10))
			   (cdr me))))
	    res))
	 (#t (list 'FAIL me s))))))))


(define parse-input parse-line)


(define (tidy-input)
  (butlast (map parse-line input)))

(define *input* (tidy-input))

(define *example1*
  (let ((data
	 "19, 13, 30 @ -2, 1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @ 1, -5, -3"))
    (map parse-line (string-split data "\n"))))




#|
now parsing problem solved , what is the problem

ignoring z
looking at x y direction
trajectory
px py pz @ vx vy vz

px py @ vx vy
y = mx + c

m = vy / vx
c = y - mx  with initial position

intersection two lines has

lines parallel - the same line
lines parallel but not same line
lines not parallel meet somewhere


y1 = m1 x1 + c1
y2 = m2 x2 + c2

if y1 = y2 then
m1 x1 + c1 = m2 x2 + c2
if x1 = x2 then
m1 x + c1 = m2 x + c2
(m1 - m2) x = c2 - c1
x = (c2 - c1) / (m1 - m2) 

|#

(define (gradient e)
  (match e
    ((px py pz vx vy vz)
     (/ vy vx))))    

(define (cross e)
  (match e
    ((px py pz vx vy vz)
     (let ((c (- py (* (gradient e) px))))
       c))))

(define (gradient-cross e)
  (match e
    ((px py pz vx vy vz)
     (let* ((grad (/ vy vx))
	    (c (- py (* grad px))))
       (values grad c)))))


;; (gradient-cross '(19 13 30 -2 1 -2))

;; (call-with-values (lambda () (gradient-cross '(19 13 30 -2 1 -2)))
;;   (lambda (m c)
;;     (list m c)))

;; (define (intersect e1 e1) ----- compiler BUG not caught e1 e1 duplicated  name as formal parameter

(define (square x) (* x x))
(define (sqroot x) (sqrt x))
(define (abs- x y)(abs (- x y)))
(define sum +)

(define (diverging? e1 e2)
  (match e1
    ((px1 py1 pz1 vx1 vy1 vz1)
     ;;(format #t "e1 match ok : ~%")
     (call-with-values (lambda () (gradient-cross e1))
       (lambda (m1 c1)
	 (match e2
	   ((px2 py2 pz2 vx2 vy2 vz2)
	    ;;(format #t "e2 match ok : ~%")
	    ;; arre these two particles diverging ? getting further apart as time goes forward
	    ;; choose some small time step delta-t 
	    (let ((delta-t 0.0001))
	      (let ((px3 (+ px1 vx1))
		    (py3 (+ py1 vy1))
		    (pz3 (+ pz1 vz1)))
		(let ((px4 (+ px2 vx2))
		      (py4 (+ py2 vy2))
		      (pz4 (+ pz2 vz2)))
		  (let ((magA (sqroot (sum (square (abs- px1 px2))
					   (square (abs- py1 py2))
					   (square (abs- pz1 pz2)))))
			(magB (sqroot (sum (square (abs- px3 px4))
					   (square (abs- py3 py4))
					   (square (abs- pz3 pz4))))))
		    (> magB magA))))))))))))


(define (intersect e1 e2)
  (match e1
    ((px1 py1 pz1 vx1 vy1 vz1)
     ;;(format #t "e1 match ok : ~%")
     (call-with-values (lambda () (gradient-cross e1))
       (lambda (m1 c1)
	 (match e2
	   ((px2 py2 pz2 vx2 vy2 vz2)
	    ;;(format #t "e2 match ok : ~%")
	    ;; arre these two particles diverging ? getting further apart as time goes forward
	    (call-with-values  (lambda () (gradient-cross e2))
	      (lambda (m2 c2)
		(let ((delta-m (- m1 m2))
		      (delta-c (- c2 c1)))
		  (cond
		   ((zero? delta-m) (values 'parallel #f #f))
		   (#t
		    (let* ((x (/ delta-c delta-m))
			   (y (+ c1 (* m1 x)))
			   (y2 (+ c2 (* m2 x))))
		      (cond
		       ;;((diverging? e1 e2) (values 'diverging x y))		       
		       ;; rather than 16403 from dividerging? test
		       ;; forward in time --- now only 13885 from *input*
		       ((and (> x px1) (< vx1 0)) (values 'diverging x y))
		       ((and (< x px1) (> vx1 0)) (values 'diverging x y))
		       ((and (> y py1) (< vy1 0)) (values 'diverging x y))
		       ((and (< y py1) (> vy1 0)) (values 'diverging x y))
		       ;; can we do this diverge on particle2
		       ((and (> x px2) (< vx2 0)) (values 'diverging x y))
		       ((and (< x px2) (> vx2 0)) (values 'diverging x y))
		       ((and (> y py2) (< vy2 0)) (values 'diverging x y))
		       ((and (< y py2) (> vy2 0)) (values 'diverging x y))
		       ;; otherwise should be good ??
		       ((= y y2) (values 'match x y ))
		       (#t (values 'mis-match-y y y2))))))))))
	   (_ (error "e2 no match" e2))))))
    (_ (error "e1 no match" e1))))



;; (format #t "intersect ~a ~a ~a ~%" ei ej out x y))))))))  ----- compiler BUG too many parameters not called

(define (iter xs low high)
  (let ((i 0)(j 0)(len (length xs))(inside 0))	
    (do-for i (0 len)
	    (do-for j ((+ i 1) len)
		    (let ((ei (list-ref xs i))
			  (ej (list-ref xs j)))
		      ;;(format #t "~%x = ~a : y = ~a  : " ei ej)
		      (call-with-values (lambda () (intersect ei ej))
			(lambda (out x y)
			  (call-with-values (lambda () (intersect ej ei))
			    (lambda (out2 x2 y2)
			      (cond
			       ((eq? out2 'diverging)
				;;(format #t "~%diverge false-pos (~a , ~a) ~%~%" (+ 0.0 x) (+ 0.0 y))
				#t
				)
			       ((eq? out2 'parallel)
				#t)
			       ((eq? out2 'mismatch)
				#f)
			       ((eq? out2 'match)
				(cond
				 ((and (number? x)(number? y) (>= x low)(<= x high)(>= y low)(<= y high))
				;;(format #t "~%intersect at (~a , ~a) ~%~%" (+ 0.0 x) (+ 0.0 y))
				
				  (set! inside (+ 1 inside))
				;; (format #t "~%intersect2 at (~a , ~a) ~%~%" (+ 0.0 x2) (+ 0.0 y2))
				  )
				 (#t #t)))
			       (#t (error "iter status unknown" out2))))))))))
    inside))



(define (test1)
  (let ((lo 7)(hi 27))
    (iter *example1* lo hi)))

(define (part1)
  (let ((lo 200000000000000)(hi 400000000000000))
    (iter *input* lo hi)))


(define (verify-input xs)
  (call-with-output-file "input.chk"
    (lambda (port)
      (do-list (e xs)
	       (match e
		 ((px py pz vx vy vz)
		  (format port "~a, ~a, ~a @ ~a, ~a, ~a~%"
			  px py pz vx vy vz))))
      (format port "~%"))))


(format #t "part 1 solution says ....... ~a ~%" (part1))


#|


[terry@terry-allseries day24]$ curl https://adventofcode.com/2023/day/24/input
Puzzle inputs differ by user.  Please log in to get your puzzle input.
[terry@terry-allseries day24]$ 

huh , since when ...

only difference in input is final newline at end of file

diff original-input input
diff input input.chk




(part-1)

16403

ugh rejected .....

attempt2 ........ simplified diverge test
applied simpler diverge tests to particle one then particle 2

#;4236> (part1)

11246

accepted answer .....
........................................................................................



so is there a bug somewhere ??
even if get it right , how know got it right ???


in a 2 d X - Y plane only ignoring Z entirely

x x x x
y y y y  parallel

know using
(filter-strictly *input*) => ()
nobody particle is travelling either vertical or horizontal only

compass
x can move to right or left X axis , increase or decrease height Y 

can have another attempt to classify direction
+x + y => north-east

             x>
         x>
    x >
x >

x >
     x>
          x>
               x>
                   x>

             x
         x
    x
<x 

<x 
     <x
          <x
               <x
                   <x


below we measured if any particle travels horizontal or vertical
no such particle was found

|#

(define (strictly-horizontal? e)
  (match e
    ((px py pz vx vy vz)
     (zero? vx))))


(define (strictly-vertical? e)
  (match e
    ((px py pz vx vy vz)
     (zero? vx))))

(define (any-strictly? e)
  (or (strictly-horizontal? e)
      (strictly-vertical? e)))

(define (filter-strictly xs)
  (filter (lambda (x) (if x x #f)) (map any-strictly? xs)))


(define (direction e)
  (match e
    ((px py pz vx vy vz)
     (cond
      ((and (> vx 0)(> vy 0)) 'north-east)
      ((and (> vx 0)(< vy 0)) 'south-east)
      ((and (< vx 0)(> vy 0)) 'north-west)
      ((and (< vx 0)(< vy 0)) 'south-west)
      (#t (error "direction" (list 'not 'known e)))))))

#|
(define (diverge? e1 e2)
  (let ((d1 (direction e1))
	(d2 (direction e2)))
    (cond
     ((eq? d1 

|#


      
      
  




  
  




	

