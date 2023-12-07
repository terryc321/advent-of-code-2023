
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
(define (code-gen)
  (pp (r2)))







