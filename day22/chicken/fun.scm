
#|
ground is flat
lowest z value of object is 1

(x y z) to (x2 y2 z2)


|#

;; importing bindings gives us bind
;; - destructuring-bind (bind (a b c) (list 1 2 3) ... a is 1 , b is 2
(import (bindings))
(import (chicken format))
(import (chicken pretty-print))
(import (procedural-macros))
(import (matchable))
(import (chicken syntax))


;; (import (simple-loops)) ;; do-for dont work for (do-for (10 0 -1) ...)

(define (test-m vlis)
  (match vlis
    [(sym from to) (pp (list 'from-to from to))]
    [(sym from to step) (pp (list 'from-to-step from to step))]
    [_ (error "not matched")]))

(test-m '(x 0 10))
(test-m '(x 0 10 1))
;;(test-m '(x 0 ))


(let loop ()
  (format #t "no loops"))



(import-for-syntax matchable)

(define-macro (down-for var range . body)
  (match range
    [(from to) ;;assume from >= to , step -1
     (let ((loop (gensym "loop")))
       `(let ,loop ((,var ,from))
	     ,@body
	     (when (> ,var ,to)
	       (,loop (- ,var 1)))))
     ]
    [(from to step) (pp (list 'from-to-step from to step))]
    [_ down-for-bad-expression-not-matched]))


(define-macro (up-for var range . body)
  (match range
    [(from to) ;;assume from <= to , step 1
     (let ((loop (gensym "loop")))
       `(let ,loop ((,var ,from))
	     ,@body
	     (when (< ,var ,to)
	       (,loop (+ ,var 1)))))
     ]
    [(from to step) (pp (list 'from-to-step from to step))]
    [_ down-for-bad-expression-not-matched]))


(down-for x (10 0) (format #t "x = ~a~%" x))

(up-for y (0 10) (format #t "y = ~a~%" y))

(down-for y (9 0)
	  (up-for x (0 2)
		  (format #t "x,y = ~a,~a~%" x y)))










;;              identifier  point-1  point-2 
(define points '((A (1 0 1) (1 2 1))
		 (B (0 0 2) (2 0 2))
		 (C (0 2 3) (2 2 3))
		 (D (0 0 4) (0 2 4))
		 (E (2 0 5) (2 2 5))
		 (F (0 1 6) (2 1 6))
		 (G (1 1 8) (1 1 9))))


(define evald
  (lambda (s)
    (pp "you want me to evaluate this >> ")
    (pp s)
    (eval s (interaction-environment))))

;; declare vars set all to #false #f
(define vars '(xmin ymin zmin xmax ymax zmax))
(map (lambda (sym) (eval `(define ,sym #f))) vars)

;; if x < xmin or xmin not defined ie not a number , then
;; set xmin = x
;; gensym hold evaluation of val once , then val never evaluated again
;; gold is symbol we will change if either gold is #f or value given is here < than gold
(define-macro (less-or-undef! val gold)
  (let ((gs (gensym "val")))
    `(let ((,gs ,val))
       (when (or (not ,gold) (< ,gs ,gold))
	 (set! ,gold ,gs)))))

(define-macro (more-or-undef! val gold)
  (let ((gs (gensym "val")))
    `(let ((,gs ,val))
       (when (or (not ,gold) (> ,gs ,gold))
	 (set! ,gold ,gs)))))

(define-macro (assert! test error-message)
  `(let ((con ,test))
     (when (not con) (error error-message))))


;; (let ((xmin 0))
;;   (less-or-undef! -1 xmin)
;;   xmin)

;; (let ((xmin #f))
;;   (less-or-undef! -1 xmin)
;;   xmin)

;; (let ((xmax 0))
;;   (more-or-undef! -1 xmax)
;;   xmax)

;; (let ((xmax 0))
;;   (more-or-undef! 5 xmax)
;;   xmax)

(define *points* (list))

;; BUG IN make-object , 
(define make-object
  (lambda (obj)
    (let* ((id (list-ref obj 0))
	   (sym (format #f "~a" id))
	   (a (list-ref obj 1))
	   (b (list-ref obj 2)))
      (format #t "a b id = ~a ~a ~a ~%" a b id)			    
      (evald
       `(define ,id
	  (bind ((x y z) (x2 y2 z2)) (list ',a ',b)

		;; determine min xyz
		(less-or-undef! x xmin)
		(less-or-undef! y ymin)
		(less-or-undef! z zmin)

		;; determine max xyz
		(more-or-undef! x2 xmax)
		(more-or-undef! y2 ymax)
		(more-or-undef! z2 zmax)
		
		(assert! (<= x x2) "x <= x2 violated")
		(assert! (<= y y2) "y <= y2 violated")
		(assert! (<= z z2) "z <= z2 violated")
		
		(format #t "x y z = ~a ~a ~a ~%" x y z)			    
		(lambda (op . args)
		  (cond
		   ((eq? op 'id) ,sym)
		   ((eq? op 'foo) ,sym)
		   ((eq? op 'down)
		    #f)		   
		   ((eq? op 'in)
		    ;; is point provided (px py pz) inside region of this point
		    (bind (px py pz) (car args)
			  (and (>= px x) (<= px x2)
			       (>= py y) (<= py y2)
			       (>= pz z) (<= pz z2))))
		   (#t (error "unknown op" op)))))))
      (set! *points* (append *points* (list (eval id))))
      )))

      ;;(set! *points* (append *points* (list (eval id)))))))







;; keep running into bad expressions , chicken compiler complains , ive no idea
;; what on earth is being executed , or where to start to find coding mistake


;; make the points themselves
(map make-object points)


;; visually check by making the diagrams
;; x - z
;; (do-for x (10 0 -1) (format #t "x = ~a~%" x))
;; (do-for x (10 0 1) (format #t "x = ~a~%" x))
;; ((lambda ()
;;    (down-for y (3 1)
;; 	     (format #t "~%")	     
;; 	     (up-for x (1 2)
;; 		     (format #t "(Y ~a,X : ~a) " y x)))))


(define (show-xz)
  (down-for z (zmax zmin)
	    (format #t "~%")		   
	    (up-for x (xmin xmax)
		    (call/cc (lambda (exit)
			       (up-for y (0 ymax)
				       (map (lambda (ob)
					      (when (ob 'in (list x y z))
						;;(format #t "(~a,~a,~a)~a~%" x y z (ob 'id))
						;;(format #t "~a" (ob 'id))
						;;(format #t "(Y:~a,X:~a)~a" y x (ob 'id))
						(format #t "~a" (ob 'id))
						(exit #t)))
					    *points*))
			       (format #t ".")))))
  (format #t "~%~%"))


(define (show-yz)
  (down-for z (zmax zmin)
	    (format #t "~%")		   
	    (up-for y (ymin ymax)
		    (call/cc (lambda (exit)
			       (up-for x (0 xmax)
				       (map (lambda (ob)
					      (when (ob 'in (list x y z))
						;;(format #t "(~a,~a,~a)~a~%" x y z (ob 'id))
						;;(format #t "~a" (ob 'id))
						;;(format #t "(Y:~a,X:~a)~a" y x (ob 'id))
						(format #t "~a" (ob 'id))
						(exit #t)))
					    *points*))
			       (format #t ".")))))
  (format #t "~%~%"))


;; determine which bricks can be safely disintegrated

































