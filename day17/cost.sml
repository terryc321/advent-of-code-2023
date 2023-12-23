
(*
sml basis library recap
mutable 2 dimensional array Array2 structure 
    
    Array2.fromList( [
		       [1,2,3],
		       [4,5,6],
		       [7,8,9]
		   ]) ;

Array2.sub (arr2,y,x) ;

	   Array2.update (arr2,y,x,new_val);

    Array2.fromList ([1 ; 2 ; 3] )

someone fix the indentation problem with this editor please


*)		    


		    
(define (initial-state)
  '((0 0 0 '())))

;; breadth first search ?
;; may need to apply append to flatten them out
(define (next-states states)
  (map next-state! states))

;; where state is current x ,y  cost and previous squares path to reach 
(define (next-state state)
  #t)


(define (reset)
  (flatten-cost cost-example)
  (flatten-cost cost-input))
  







(define (puzzle vv filename)
  (define id 0)
  (define width (vector-length (vector-ref vv 0)))
  (define height (vector-length vv))
  (define (onboard? x y)
    (and (>= x 0)(< x width)(>= y 0)(< y height)))
  (define (awol? x y)
    (not (onboard? x y)))
  (define (finish-line? x y)
    (and (= x (- width 1))
	 (= y (- height 1))))
  (define (visited? x y path)
    (member `(,x ,y) path))

  (define output-port #f)

  
  (define (solution! path cost step) #f)  
    ;; (cond
    ;;  ((< cost best-cost)
    ;;   (format #t "~%~%***best solution so far *** ~% *** path ~%~a~%***cost ~a~%~%" path cost)
    ;;   (set! best-cost cost)
    ;;   (set! best-solution path)
    ;;   (set! best-step step))))
  
  (define (report x y path cost step) #f)
  
    ;; (format output-port "~a~%" `(path ,path id ,id cost ,cost))
    ;; (set! id (1+ id)))


  
  ;; if path is first ever to reach square then record that as best known
  ;; if path is same as another path , may be equally optimal
  ;; if path is higher cost than previous path , suboptimal , definitely poor path
  ;; return #t means sub optimal - do not continue search
  ;; return #f means first found, equal to best search so far
  (define (poor? x y path cost step)
    (let ((known (get-xy cost-vector x y)))
      (cond
       ((null? known)
	(set-xy! cost-vector x y `(,cost ,path))
	;;(format #t "found first for ~a , ~a  at cost ~a ~%" x y cost)
	#f)
       (#t
	;;(format #t "known = [~a]~%" known)
	(let ((known-cost (car known)))
	  (cond
	   ((< cost known-cost)
	    (set-xy! cost-vector x y `(,cost ,path))
	    (format #t "found new best for for ~a , ~a  at cost ~a ~%" x y cost)	    
	    #f)
	   ((= cost known-cost)
	    ;;(set-xy! cost-vector x y (cons `(,cost ,path) known))
	    #f)
	   (#t #t)))))))


  
       

  
  ;; going to try guess how long optiml path is
  ;; no real idea how to solve this yet ...
  (define (dead-end? p) #f)
;;    (> (length p) 27))
  
  ;; how to prevent a walled off search pattern ??
  
  (define (move-left x y n path cost step)    
    (cond
     ((awol? x y) #f)
     ((dead-end? path) #f)
     (#t
      (let ((cost2 (+ cost (get-xy vv x y))))	
	(cond
	 ((poor? x y path cost2 step) #f)
	 ;; ((finish-line? x y) (solution! (cons `(,x ,y) path) cost2 step))
	 ;; ((visited? x y path) #f)
	 (#t
	  (when (not (= n 0)) (move-left (- x 1) y (- n 1) (cons `(,x ,y) path) cost2 (1+ step)))
	  (move-down x (+ y 1) 2 (cons `(,x ,y) path) cost2 (1+ step))
	  (move-up x (- y 1) 2 (cons `(,x ,y) path) cost2 (1+ step))
	  ))))))
  
  (define (move-right x y n path cost step) 
    (cond
     ((awol? x y) #f)
     ((dead-end? path) #f)
     (#t
      (let ((cost2 (+ cost (get-xy vv x y))))	
	(cond
	 ((poor? x y path cost2 step) #f)
	 ;; ((finish-line? x y) (solution! (cons `(,x ,y) path) cost2 step))
	 ;; ((visited? x y path) #f)
	 (#t
	  (move-down x (+ y 1) 2 (cons `(,x ,y) path) cost2 (1+ step))
	  (when (not (= n 0)) (move-right (+ x 1) y (- n 1) (cons `(,x ,y) path) cost2 (1+ step)))
	  (move-up x (- y 1) 2 (cons `(,x ,y) path) cost2 (1+ step))
	  ))))))
  
  (define (move-up x y n path cost step) 
    (cond
     ((awol? x y) #f)
     ((dead-end? path) #f)
     (#t
      (let ((cost2 (+ cost (get-xy vv x y))))
	(cond
	 ((poor? x y path cost2 step) #f)
	 ;; ((finish-line? x y) (solution! (cons `(,x ,y) path) cost2 step))
	 ;; ((visited? x y path) #f)
	 (#t
	  (move-right (+ x 1) y 2 (cons `(,x ,y) path) cost2 (1+ step))
	  (move-left (- x 1) y 2 (cons `(,x ,y) path) cost2 (1+ step))
	  (when (not (= n 0)) (move-up x (- y 1) (- n 1) (cons `(,x ,y) path) cost2 (1+ step)))))))))
  
  (define (move-down x y n path cost step) 
    (cond
     ((awol? x y) #f)
     ((dead-end? path) #f)
     (#t
      (let ((cost2 (+ cost (get-xy vv x y))))
	(cond
	 ((poor? x y path cost2 step) #f)
	 ;; ((finish-line? x y) (solution! (cons `(,x ,y) path) cost2 step))
	 ;; ((visited? x y path) #f)
	 (#t	
	  (when (not (= n 0)) (move-down x (+ y 1) (- n 1) (cons `(,x ,y) path) cost2 (1+ step)))
	  (move-right (+ x 1) y 2 (cons `(,x ,y) path) cost2 (1+ step))
	  (move-left (- x 1) y 2 (cons `(,x ,y) path) cost2 (1+ step))
	  ))))))

  (define (start)
    (let ((x 0)(y 0)(n 3)(step 0)(path '())(cost 0))
      (move-right x y n path cost step)
      (move-down x y n path cost step)))

  
  #|
  (call-with-output-file filename (lambda (port)
				    (set! output-port port)
  (start)))
  |#
  (start)

  )




;; example1.out is now a fifo
(define (example-1)
  (set! cost-vector cost-example)
  (flatten-cost cost-vector)
  
  (puzzle example "example1.out")
  (pp cost-vector))

;; part1.out is now a fifo
(define (input-1)
  (set! cost-vector cost-input)
  (flatten-cost cost-vector)
  (puzzle input "part1.out")
  (pp cost-vector))

;;(example-1)

;;(input-1)
















    
