#|
(getcwd)
(chdir "../day9")
(chdir "day9")
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

(use-modules (ice-9 hash-table)) 


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

;; -----------------------------------------------------------------


(define example
  '(
    (0 3 6 9 12 15)
(1 3 6 10 15 21)
(10 13 16 21 30 45)
    ))


(define (diff2 xs ys)
  (cond
   ((or (null? ys) (null? xs)) '())
   (#t (let* ((a (car xs))
	      (b (car ys))
	      (c (- b a)))
	 (cons c (diff2 (cdr xs)(cdr ys)))))))


(define (diff xs)
  (diff2 xs (cdr xs)))

(define (all-zero? xs)
  (cond
   ((null? xs) #t)
   ((zero? (car xs)) (all-zero? (cdr xs)))
   (#t #f)))

(define (my-all dat)
  (letrec ((foo (lambda (xs)
		  (let ((ys (diff xs)))
		    (cond
		     ((all-zero? ys) (list ys))
		     (#t (cons ys (foo ys))))))))
    (cons dat (foo dat))))

(format #t "~a ~%" (map my-all example))

(define (aprox xs)
  (apply + (map last (my-all xs))))

(define (fib xs n)
  (cond
   ((null? xs) n)
   (#t
    (let* ((a n)
	   (b (car xs))
	   (c (- b a)))
      ;;(format #t "fib c: ~a = ~a - ~a ~%" c b a) 
      (fib (cdr xs) c)))))

(define (fuz xs)
  (fib (cdr xs) (car xs)))

(define (rev-aprox xs)
  (fuz (reverse (map first (my-all xs)))))




(define (part-1)
  (apply + (map aprox input)))

(define (part-2)
  (apply + (map rev-aprox input)))


#|

975
accepted answer


|#
