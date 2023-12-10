
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


#|

(+
0 3 18
0 1 7 28
0 2 8 23 68
)

(+ 68 33)

10  13  16  21  30  45  68   101
   3   3   5   9  15  23    33
     0   2   4   6   8   10
       2   2   2   2   2
         0   0   0   0

114 ?
i get 101 ??

(+ 18 3 )  21
(+ 28 8 )  36

(+ 21 36 101 )  158




|#








