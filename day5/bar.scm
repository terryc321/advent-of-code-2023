#|

thinking symbolic execution ?

given a range of seeds after split into pairs and hi-lo worked out



|#

#|
(getcwd)
(chdir "day5")
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

;; --------------------- macros --------------------------
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

;; --------------------- while ----------------------------

(defmacro while (condition . body)
  (let ((lup (gensym "loop")))
    `(letrec ((,lup (lambda ()
		      (when ,condition
			,@body
			(,lup)))))
       (,lup))))

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

#|
seed -> soil
soil seed length
50 98 2       seed 98 -> soil 50 , seed 99 -> soil 51
52 50 48      seed 50 -> soil 52 , seed 51 -> soil 53 ...
|#
(define example '((seeds 79 14 55 13)
		  (seed soil 50 98 2 52 50 48 )
		  (soil fertilizer  0 15 37 37 52 2 39 0 15 )
		  (fertilizer water   49 53 8  0 11 42 42 0 7 57 7 4 )
		  (water light 88 18 7 18 25 70 )
		  (light temperature 45 77 23 81 45 19 68 64 13 )
		  (temperature humidity 0 69 1 1 0 69 )
		  (humidity location 60 56 37 56 93 4 )))


(define labels (map first input))

#|
iota z = 0 1 2 3 ... z-1
src  z+y = y y+1 y+2 y+3 ... y+z-1
dest z+x = x x+1 x+2 x+3 ... x+z-1

given triple : x y z
src limits inclusive  = y .... y + z - 1
dest limits inclusive = x .....x + z - 1

after all triples processed
src falls outside of map is an identity
src = dest

|#

(define (triple x y z)
  (let* ((iot (iota z))
	 (src (map (lambda (n) (+ n y)) iot))
	 (dest (map (lambda (n) (+ n x)) iot)))
    (map list src dest)))

(triple 50 98 2)
;;((98 50) (99 51))

(triple 52 50 48)


(define (fast-triple x y z)
  (let* ((src-lo y)
	 (src-hi (+ y z -1))
	 (dest-lo x)
	 (dest-hi (+ x z -1))
	 (diff (- src-lo dest-lo)))
    ;;(format #t "fast triple : src ~a - ~a : dest ~a -> ~a : diff ~a ~%" src-lo src-hi dest-lo dest-hi diff)
    (lambda (n)
      (cond
       ((and (>= n src-lo) (<= n src-hi))
	(- n diff))
       (#t #f)))))

;; fast triple works for a single three values
;; but we rather process entire map in one go
;; how about code generation
;; compile-time fast-triple


(assert (= 50 ((fast-triple 50 98 2) 98)))
(assert (= 52 ((fast-triple 52 50 48) 50))) ;; x > y
(assert (= 53 ((fast-triple 52 50 48) 51)))
(assert (= 54 ((fast-triple 52 50 48) 52)))
(assert (= 48 ((fast-triple 48 50 48) 50)))
(assert (= 49 ((fast-triple 48 50 48) 51)))
(assert (= 50 ((fast-triple 48 50 48) 52)))
(assert (eq? #f((fast-triple 127 26 100) 25)))
(assert (= 127 ((fast-triple 127 26 100) 26)))
;;(assert (= ((fast-triple 127 26 100) 125)))
(assert (eq? #f ((fast-triple 26 123 100) 122))) ;; y > x
(assert (= 26 ((fast-triple 26 123 100) 123)))
(assert (= 27 ((fast-triple 26 123 100) 124)))
(assert (= 49 ((fast-triple 48 50 48) 51)))
(assert (= 50 ((fast-triple 48 50 48) 52)))
(assert (= 55 ((fast-triple 52 50 48) 53)))
(assert (= 50 ((fast-triple 50 50 48) 50)))
(assert (= 51 ((fast-triple 50 50 48) 51)))
(assert (= 52 ((fast-triple 50 50 48) 52)))



(define (compile-triple x y z)
  (let* ((src-lo y)
	 (src-hi (+ y z -1))
	 (dest-lo x)
	 (dest-hi (+ x z -1))
	 (diff (- src-lo dest-lo)))
    ;;(format #t "fast triple : src ~a - ~a : dest ~a -> ~a : diff ~a ~%" src-lo src-hi dest-lo dest-hi diff)
    `((and (>= n ,src-lo) (<= n ,src-hi))
      ,(cond
	((< diff 0) `(+ n ,(abs diff)))
	(#t `(- n ,diff))))))



(compile-triple 50 50 48)
;; ((and (>= n 50) (<= n 97)) (- n 0))

;; generated procedures at runtime ... we can generate them at compile time ...
;; (pp (map (lambda (x) (apply fast-triple x)) (car (extract-triples input))))
;;

(define (r1)
  (map (lambda (z) 
	 (map (lambda (y) (apply compile-triple y))
	      z))
       (extract-triples input)))

(define (r2)
  `(begin
     ,@(map
       (lambda (name x)
	 `(define ,name (lambda (n) (cond ,@(append x `((#t n)))))))
       (u2)
       (r1))))




;; figure out the procedure names
(define (u1)
  (map (lambda (x) (take x 2)) (cdr input)))

;; u2 generates a series of procedure names reference the required translation
(define (u2)
  (map (lambda (x)
	 (match x
	   ((src dest) (string->symbol (format #f "~a->~a" src dest)))))
       (u1)))


;; know seeds ahead of time
(define seeds '(202517468 131640971 1553776977 241828580 1435322022
			  100369067 2019100043 153706556 460203450 84630899 3766866638 114261107
			  1809826083 153144153 2797169753 177517156 2494032210 235157184
			  856311572 542740109))





(define identity (lambda (x) x))
  

      
#|
(length  '((50 52) (51 53) (52 54) (53 55) (54 56) (55 57) (56 58) (57 59) (58
60) (59 61) (60 62) (61 63) (62 64) (63 65) (64 66) (65 67) (66 68)
(67 69) (68 70) (69 71) (70 72) (71 73) (72 74) (73 75) (74 76) (75
77) (76 78) (77 79) (78 80) (79 81) (80 82) (81 83) (82 84) (83 85)
(84 86) (85 87) (86 88) (87 89) (88 90) (89 91) (90 92) (91 93) (92
94) (93 95) (94 96) (95 97) (96 98) (97 99)))

;; 48 entries long
|#

(define (list->triples xs)
  (define (list->triples2 xs ys)
    (cond
     ((null? xs) ys)
     (#t (list->triples2 (drop xs 3) (cons (take xs 3) ys)))))
  (list->triples2 xs '()))


;;(list->triples (cddr (car (cdr example))))

(define (extract-triples dat)
  (let* ((maps (cdr dat))
	 (the-triples (map list->triples (map cddr maps))))
    the-triples))


;; --------- solutions is just output of code generator -------------
;; plus a few fixups to call required procedures in reverse order
(define (part-1)
  (define (code-gen)
    (pp (r2)))
  (code-gen))


(define (seed-pairs xs)
  (define (helper xs ys)
    (cond
     ((null? xs) ys)
     (#t (helper (drop xs 2) (cons (take xs 2) ys)))))
  (helper (cdar xs) '()))

(define (seed-ranges xs)
  (let ((pairs (seed-pairs xs)))
    (map (lambda (x)
	   (let* ((lo (car x))
		  (hi (+ lo (cadr x) -1)))
	     (list lo hi)))
	 pairs)))


#|
here are ranges of seeds

scheme@(guile-user) [2]> (pp (seed-ranges input))
((856311572 1399051680)
 (2494032210 2729189393)
 (2797169753 2974686908)
 (1809826083 1962970235)
 (3766866638 3881127744)
 (460203450 544834348)
 (2019100043 2172806598)
 (1435322022 1535691088)
 (1553776977 1795605556)
 (202517468 334158438))

(r2) is our code-generator
(begin
procedure1 
procedure2
...
procedure7


(pp (length (cdr (r2))))
7

;; procedures here ...
(pp (map second (cdr (r2))))

(
seed->soil
soil->fertilizer
fertilizer->water
water->light
light->temperature
temperature->humidity
humidity->location
)


take first in the seed ranges
856311572 1399051680
call them lo hi

lets say worse case scenario
that the seed range covers all of the map and some map does not cover
maximal complexity...


seed->soil

(define seed->soil
   (lambda (n)
     (cond ((and (>= n 3772491402) (<= n 3816852823))
            (- n 232471270))
           ((and (>= n 2850116199) (<= n 3039622537))
            (- n 393093465))
           ((and (>= n 1011676203) (<= n 1026919252))
            (+ n 1032918600))
           ((and (>= n 3411859564) (<= n 3493485542))
            (- n 382070727))
 
ranges of seed->soil for a given range -> output one or more ranges

some input values of seeds may be out of map - identities output

input larger than all map
input <------------------------->
map     <-->  <-->   <--><--><-->
overs <><--><><--><-><--><--><-->

overlaps overs the output is also a set of ranges

in the code 

(define seed->soil
   (lambda (n)
     (cond ((and (>= n 3772491402) (<= n 3816852823))
            (- n 232471270))

the input ranges are inclusive , so the output ranges are also inclusive

in-  [3772491402 <--> 3816852823]
out- [3540020132 <--> 3584381553]

input ranges could do something like 
starts partially in certain range
ends partially in a different range
covers several other ranges in between
some shades of identity when outside map ..
or may completely match one single range ....

input range is then mapped to one or more output ranges

each output ranges then passed to next procedure as single range to generate possibly
several output ranges
eventually the last procedure called and
end up with a set of output ranges

then it just wants the lowest number in the lowest surviving output range.
sort based on first number
range = (lo hi)
....(sort output-ranges (lambda (x y)(< (car x)(car y))))


range adjustment only two types of expression
(+ n XXX)
(- n XXX)

example

(- n 116548703)
(+ n 818523728)

Given .....

(define seed->soil
   (lambda (n)
     (cond ((and (>= n 3772491402) (<= n 3816852823))
            (- n 232471270))

convert this into
seed->soil
input-range output-range
input-range output-range
input-range output-range
input-range output-range
input-range output-range


given a range for input which may cover several input-ranges ...
in = (lo hi)
take lo range 
find matching input-range
if hi is inside input range then done - simply output range curtailed

input-range output-range


(pp (cdr (caddr (caddr (car (cdr (r2)))))))
                        ^^----- seed->soil 

... gets me to
(((and (>= n 3772491402) (<= n 3816852823))
  (- n 232471270))


|#

(define (seed->soil-ranges)
  (let ((res '()))
    (dolist (x  (cdr (caddr (caddr (car (cdr (r2)))))))
	    (match x
	      ((('and ('>= 'n lo) ('<= 'n hi))
		('- 'n diff))
	       (let ((olo (- lo diff))
		     (ohi (- hi diff)))
		 (set! res (cons `(transform (,lo ,hi) (,olo ,ohi)) res))))
	      ((('and ('>= 'n lo) ('<= 'n hi))
		('+ 'n diff))
	       (let ((olo (+ lo diff))
		     (ohi (+ hi diff)))
		 (set! res (cons `(transform (,lo ,hi) (,olo ,ohi)) res))))
	      (('#t 'n) #f)
	      (_ (error "seed->soil no match" (list x)))))
    res))


       
(define (make-ranges-for n)
  (let ((res '()))
    (dolist (x  (cdr (caddr (caddr (list-ref (cdr (r2)) n)))))
	    (match x
	      ((('and ('>= 'n lo) ('<= 'n hi))
		('- 'n diff))
	       (let ((olo (- lo diff))
		     (ohi (- hi diff)))
		 (set! res (cons `(transform (,lo ,hi) (,olo ,ohi)) res))))
	      ((('and ('>= 'n lo) ('<= 'n hi))
		('+ 'n diff))
	       (let ((olo (+ lo diff))
		     (ohi (+ hi diff)))
		 (set! res (cons `(transform (,lo ,hi) (,olo ,ohi)) res))))
	      (('#t 'n) #f)
	      (_ (error "seed->soil no match" (list x)))))
    res))



(define (name-transforms)
  (let ((procs '(seed->soil soil->fertilizer fertilizer->water water->light light->temperature
			    temperature->humidity humidity->location))
	(res '()))
    (dolist (n (iota 7))
	    (let ((ranges (make-ranges-for n))
		  (name (list-ref procs n)))
	      (set! res (cons (cons name ranges)
			      res))))
    (reverse res)))

#|
scheme@(guile-user) [15]> (pp (name-transforms))
((seed->soil
   (transform
     (644938450 804624156)
     (1393363309 1553049015))
   (transform
     (1844060172 1863372373)
     (2025282601 2044594802))
   (transform
     (1026919253 1059790344)
     (1233103806 1265974897))
   (transform
     (1933428941 2019959931)
     (1086566452 1173097442))
   (transform (0 21589658) (1265974898 1287564556))
...
...
(soil->fertilizer ...)
(fertilizer->water ...)
...
(humidity->location ...)


given a seed-range in form (lo hi)
search all transforms in procedure 0 to see which if any match 


scheme@(guile-user) [15]> (seed-ranges input)
$36 = ((856311572 1399051680) (2494032210 2729189393) (2797169753 2974686908) (1809826083 1962970235) (3766866638 3881127744) (460203450 544834348) (2019100043 2172806598) (1435322022 1535691088) (1553776977 1795605556) (202517468 334158438))


(dolist (sr (seed-ranges input))
     ...  sr: (856311572 1399051680) initially
  
transformer is one entry in (name-transforms)
(list-ref (name-transforms) 0) =>

(seed->soil
(transform
(644938450 804624156)
(1393363309  1553049015))
(transform ..)
(transform ..)
(transform ...)
(transform ...)
)


as search goes on 

(squash (first (seed-ranges input)) (list-ref (name-transforms) 0))
             <sr>                        <transformer>
                                      named list of transforms

sort the transforms before we start search so transforms all in order
          <--><--><--><><---><><--->
                  ?----------?
       

two ranges - what ways can they overlap ?

sr : lo  ...... hi
tr : in2  ..... in3


FOUND 1 gaps 
gaps : ((2064392707 2343571959)) 

FOUND 1 gaps 
gaps : ((2592542425 2644420891)) 

FOUND 1 gaps 
gaps : ((2292426181 2369332990)) 

FOUND 1 gaps 
gaps : ((660822731 992982308)) 

FOUND 1 gaps 
gaps : ((1134212936 1656447493)) 

FOUND 1 gaps 
gaps : ((1069388363 1941982136)) 

FOUND 1 gaps 
gaps : ((867861704 1087300933)) 

find a way to 

|#

(define (find-gaps trs)
  (let ((gaps '()))
    (letrec ((helper (lambda (n xs)
		       (cond
			((null? xs) #f)
			((null? (cdr xs)) #f)
			(#t (let ((tr (first xs))
				  (tr2 (second xs)))
			      (match tr
				(('transform (in2 in3)(out2 out3))
				 (match tr2
				   (('transform (sin2 sin3)(sout2 sout3))
				    (cond
				     ((> sin2 (+ in3 1)) (set! gaps (cons
								     (list (+ in3 1) (- sin2 1))
								     gaps)))
				     ((< sin2 (+ in3 1)) (error "find-gaps: underflow"))
				     ((= sin2 (+ in3 1)) #t)))
				   (_ (error "find-gaps:tr2 no match"))))
				(_ (error "find-gaps:tr2 no match")))))))))
      (helper 0 trs)
      (format #t "FOUND ~a gaps ~%gaps : ~a ~%~%" (length gaps) gaps))
    gaps))






(define (incorporate-gaps trs)
  (let ((ranges '()))
    (letrec ((helper (lambda (n xs)
		       (cond
			((null? xs) #f)
			(#t (let ((tr (car xs)))
			      (match tr
				(('transform (in2 in3)(out2 out3))
				 (cond
				  ((> in2 n) ;; next range is above 
				   ;;(set! gaps (cons (list n (- in2 1)) gaps))
				   ;; include identity transform for missing gap 
				   (set! ranges (cons `(transform (,n ,(- in2 1)) (,n ,(- in2 1)))
						      ranges))				   
				   (helper (+ in3 1) (cdr xs)))
				  ((= in2 n) ;; next range bumped up against end of previous range
				   (set! ranges (cons tr ranges))				   
				   (helper (+ in3 1) (cdr xs)))
				  ((< in2 n)
				   (error "find-gaps" (list 'underflow 'end-previous n 'current-from-> in2 'current-to-> in3)))))
				(_ (error "find-gaps no match")))))))))
      (helper 0 trs))
    (reverse ranges)))






(define (squash sr transformer)
  (letrec ((my-sorter (lambda (x y)
			(< (car (cadr x))(car (cadr y))))))
    (let ((transforms (sort (cdr transformer) my-sorter))
	  (transforms2 '())
	  (res '()))

      ;; see if there are any gaps in the ranges of the transformers
      ;; (set! transforms2 (incorporate-gaps transforms))
q
      (format #t "********** transforms =>  ************* ~%")
      (pp transforms)

      
      (format #t "****** transforms's gaps ~%")
      (find-gaps transforms)

      
      (format #t "***** transforms2's gaps ~%")
      (find-gaps transforms2)
      
      
      (letrec ((search (lambda (lo hi)
			 (dolist (tr transforms)
				 (match tr
				   (('transform (in2 in3)(out2 out3))
				    (cond
				     ;; transform is 
				     ((< hi in2) #t)
				     ;;transform below our range interest
				     ((> lo in3) #t)))				    
				   (_ (error "squash no match"))))))
	       )
	(let ((lo (first sr))
	      (hi (second sr)))
	  (search lo hi)
	  'done)))))







;; (squash (first (seed-ranges input)) (list-ref (name-transforms) 0))

#|
scheme@(guile-user) [22]> (map (lambda (i) (squash (first (seed-ranges input)) (list-ref (name-transforms) i))) (iota 7))
FOUND 1 gaps 
gaps : ((2064392707 2343571959)) 

FOUND 1 gaps 
gaps : ((2592542425 2644420891)) 

FOUND 1 gaps 
gaps : ((2292426181 2369332990)) 

FOUND 1 gaps 
gaps : ((660822731 992982308)) 

FOUND 1 gaps 
gaps : ((1134212936 1656447493)) 

FOUND 1 gaps 
gaps : ((1069388363 1941982136)) 

FOUND 1 gaps 
gaps : ((867861704 1087300933)) 

$108 = (done done done done done done done)

in each "MAP" seed->soil ... etc there is a range where it falls through to identity 


|#





	
    










