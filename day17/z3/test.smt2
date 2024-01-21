
;; (echo "starting z3...")
;; (declare-const a Int)
;; (declare-const x Int)
;; (declare-const y Int)
;; (declare-const z Int)
;; ;; (declare-const arr Array)
;; (declare-fun f (Int Bool) Int)
;; (declare-fun g (Int Int) Int)

;; (assert (< a 10))
;; (assert (< (f a true) 100))
;; (assert (= (+ x y) 10))
;; ;; (assert (= (select arr 0) true))
;; ;; (assert (= (select arr 1) false))
;; (assert (= (g a 30) 5))
;; (assert (= (g a 40) 25))
;; (check-sat)
;; (get-model)

;; (push)
;; (pop)
;; (reset) ;; erase all assertions and declarations
;; (display t)
;; (simplify t)
;; (define-sort IList () (List Int))
;; (define-sort List-Set (T) (Array (List T) Bool))
;; (define-sort I () Int)

;; propositional logic
;; (declare-const p Bool)
;; (declare-const q Bool)
;; (declare-const r Bool)
;; (define-fun conjecture () Bool
;; 	(=> (and (=> p q) (=> q r))
;; 		(=> p r)))
;; (assert (not conjecture))
;; (check-sat)
;; (get-model)

;; demorgan -- unsat
;; (declare-const a Bool)
;; (declare-const b Bool)
;; (define-fun demorgan () Bool
;;     (= (and a b) (not (or (not a) (not b)))))
;; (assert (not demorgan))
;; (check-sat)

;; cardinality constraint
;; atleast 2 true of p q r
;; (declare-const p Bool)
;; (declare-const q Bool)
;; (declare-const r Bool)
;; (assert ((_ at-least 2) p q r))
;; (check-sat)
;; (get-model)

;; at most 1 can be true
;; (declare-const p Bool)
;; (declare-const q Bool)
;; (declare-const r Bool)
;; (assert ((_ at-most 1) p q r))
;; (check-sat)
;; (get-model)

;; pseudo-boolean  greater or equal p-b-g-e pbge
;; (declare-const p Bool)
;; (declare-const q Bool)
;; (declare-const r Bool)
;; (declare-const s Bool)
;; (declare-const t Bool)
;; (assert ((_ pbge 4 2 1 3 3 2) p q r s t))
;; ;; pbge sum is atleast 4 = 2p + q + 3r + 3s + 2t
;; (check-sat)
;; (get-model)

;; similar one to 

;; ;; pseduo-boolean less than or equal p-b-l-e
;; (declare-const p Bool)
;; (declare-const q Bool)
;; (declare-const r Bool)
;; (declare-const s Bool)
;; (declare-const t Bool)
;; ;; 2p + q + 3r + 3s + 2t <= 5 
;; (assert ((_ pble 5 2 1 3 3 2) p q r s t)) ;; s true t true
;; (check-sat)
;; (get-model)
;; how get all solutions?

;; ;; pseduo boolean equal to 
;; (declare-const p Bool)
;; (declare-const q Bool)
;; (declare-const r Bool)
;; (declare-const s Bool)
;; (declare-const t Bool)
;; ;; 2p + q + 3r + 3s + 2t = 5
;; (assert ((_ pbeq 5 2 1 3 3 2) p q r s t))
;; ;;              ^^^ 5 is sum equation has to equal to
;; (check-sat)
;; (get-model)

;; The basic building blocks of SMT formulas are constants and functions. Constants are just functions that take no arguments. So everything is really just a function.

;; (declare-fun f (Int) Int)
;; (declare-fun a () Int) ; a is a constant
;; (declare-const b Int) ; syntax sugar for (declare-fun b () Int)
;; (assert (< a 20))
;; (assert (< b a))
;; (assert (= (f 10) 1))
;; (check-sat)
;; (get-model)


;; classical first order logic functions are total and have no side effects
;; uninterpreted functions
;; f takes an A and returns an A
;; apply f twice to x returns x
;; apply f once to x returns y
;; if f is identity then (f x) = x (f (f x)) = x  (= (f x) y) so (= x y) from (f x) = x
;; not sure quite what this means ...
;; (declare-sort A)
;; (declare-const x A)
;; (declare-const y A)
;; (declare-fun f (A) A)
;; (assert (= (f (f x)) x))
;; (assert (= (f x) y))
;; (assert (not (= x y)))
;; (check-sat)
;; (get-model)


;; (declare-sort A)
;; (declare-fun f (A) A)
;; (declare-const x A)
;; (assert (= (f (f x)) x))
;; (assert (= (f (f (f x))) x))
;; (check-sat)
;; (get-model)
  
;; (assert (not (= (f x) x)))
;; (check-sat)

;; ;; a = 3 
;; (declare-fun a () Int)  
;; (assert (= a 3))
;; (check-sat)
;; (get-model)


;; ;; and or not
;; ;; ite means if then else 
;; ;; grid : int -> int -> int
;; (declare-fun g (Int Int) Int)
;; (assert (= (g 0 0) 0))
;; (check-sat)
;; (get-model)
;; like something of a g grid at x = 1 y = 0 then cost is 3 
;; (assert (= (g 1 0) 3))
;; (assert (= (g 0 1) 3))
;; (check-sat)
;; (get-model)

;; if want some sort of time step to get around issue of
;; at time t = 0 cost of square 0 0 is 0
;; but at any other time cost of square 0 0 is 2

;; how express in logic cannot go forward more than 3 spaces ?

;; constants are just functions that take no arguments
;; x1,y1,cost1
;; (declare-fun x1 () Int)  
;; (declare-fun y1 () Int)

;; (declare-fun x2 () Int)  
;; (declare-fun y2 () Int)

;; (declare-fun x3 () Int)  
;; (declare-fun y3 () Int)

;; (declare-fun x4 () Int)  
;; (declare-fun y4 () Int)

;; path a b if path a b
;; path a c if path from a to b /\ path from b to c
;; path from a to c if path a 

;; path(x,y,x2,y2,cost) either is or is not ??
;; move(x,y,x2,y2,cost)  
;; (declare-fun path (Int Int Int Int Int) Bool)
;; (declare-fun uz () Int)

;; ;; at step T we are at square X Y
;; (declare-fun at (Int Int Int) Bool)

;; ;; like to advance along a path rather than be static so we disallow path 
;; (assert (forall ((x Int)(y Int)(z Int)) (= false (path x y x y z))))

;; (assert (forall ((x Int)(y Int)(x2 Int)(y2 Int)(x3 Int)(y3 Int)(z Int)(z2 Int)(z3 Int))
;; 		(implies (path x y x3 y3 z) (or (path x y x3 y3 z)
;; 						(and (path x y x2 y2 z2)
;; 						     (path x2 y2 x3 y3 z3)
;; 						     (= z (+ z2 z3)))))))
;; (assert (at 0 0 0))
;; (assert (or (at 1 0 0) (at 0 1 0)))
;; position at a square
;; for our puzzle think about moving right 1 2 3 spaces initially
;; then move up or down 1 2 3 spaces
;;
;; (assert (path 1 1 2 2 3))
;; (assert (path 2 2 3 3 5))
;; (assert (path 1 1 3 3 uz))

;; at all times must stay on the grid ?
;; move1
;; move2

;; (declare-fun uc () Int)
;; (declare-fun move (Int Int Int Int Int) Bool)
;; ;; (assert (forall ((x Int)(y Int)(x2 Int)(y2 Int)(x3 Int)(y3 Int)(c Int)(c2 Int)(c3 Int))
;; ;; 		(implies (and (move x y x2 y2 c) (move x2 y2 x3 y3 c2))
;; ;; 			 (and (= c3 (+ c c2))
;; ;; 			      (move x y x3 y3 c3)))))

;; (assert (forall ((x Int)(y Int)(x2 Int)(y2 Int)(c Int)(c2 Int))
;; 		(implies (move x y x2 y2 c)
;; 			 (not (and (= x x2)(= y y2))))))

;; ;; (assert (forall ((x Int)(y Int)(x2 Int)(y2 Int)(x3 Int)(y3 Int)(c Int)(c2 Int)(c3 Int))
;; ;; 		(implies (and (move x y x2 y2 c) (move x2 y2 x3 y3 c2))
;; ;; 			 (move x y x3 y3 (+ c c2)))))


;; (assert (move 0 0 1 0 3))

;; (assert (move 0 0 1 0 3))
;; (assert (move 1 0 2 0 4))
;; (assert (move 0 0 2 0 uc))

;; x1 means go right
;; depending on how we compute cost we can either ignore x1 meaning we go down directly
;; or we can include x1 and thiink of moving right immediately

;; ;; constants are functions of no arguments
;; (declare-fun width () Int)  
;; (declare-fun height () Int)

;; (assert (= width 141))
;; (assert (= height 141))

;; (declare-fun x1 () Int)  
;; (declare-fun y1 () Int)

;; (declare-fun x2 () Int)  
;; (declare-fun y2 () Int)

;; (declare-fun x3 () Int)  
;; (declare-fun y3 () Int)

;; (declare-fun x4 () Int)  
;; (declare-fun y4 () Int)

;; (maximize (+ x y))
;; (minimize (+ x y))

;; ;; stay with zero indexing
;; ;; p1x = 0 or 1 however want indices to work
;; (declare-fun p1x () Int)  
;; (declare-fun p1y () Int)

;; (declare-fun p2x () Int)  
;; (declare-fun p2y () Int)

;; (declare-fun p3x () Int)  
;; (declare-fun p3y () Int)

;; (declare-fun p4x () Int)  
;; (declare-fun p4y () Int)

;; ;; x1 cannot go left as already far left as can go
;; ;; y1 cannot go up as already high as can go
;; (assert (or (= x1 1)(= x1 2)(= x1 3)))
;; (assert (or (= y1 1)(= y1 2)(= y1 3)))
;; (assert (> x1 0))
;; (assert (> y1 0))

;; (assert (or (= x2 -3)(= x2 -2)(= x2 -1)(= x2 1)(= x2 2)(= x2 3)))
;; (assert (or (= y2 -3)(= y2 -2)(= y2 -1)(= y2 1)(= y2 2)(= y2 3)))
;; (assert (> (+ x1 x2) 0))
;; (assert (> (+ y1 y2) 0))

;; (assert (or (= x3 -3)(= x3 -2)(= x3 -1)(= x3 1)(= x3 2)(= x3 3)))
;; (assert (or (= y3 -3)(= y3 -2)(= y3 -1)(= y3 1)(= y3 2)(= y3 3)))
;; (assert (> (+ x1 x2 x3) 0))
;; (assert (> (+ y1 y2 y3) 0))

;; (assert (or (= x4 -3)(= x4 -2)(= x4 -1)(= x4 1)(= x4 2)(= x4 3)))
;; (assert (or (= y4 -3)(= y4 -2)(= y4 -1)(= y4 1)(= y4 2)(= y4 3)))
;; (assert (and (< (+ x1 x2 x3 x4) width) (> (+ x1 x2 x3 x4) 0)))
;; (assert (and (< (+ y1 y2 y3 y4) height) (> (+ y1 y2 y3 y4) 0)))


;; (0 0)
;; (x1 0)
;; (x1 y1)
;; (x1 + x2,y1)
;; (x1 + x2, y1 + y2)
;; (x1 + x2 + x3,y1 + y2 + y3)
;; (x1 + x2 + x3 + x4, y1 + y2 + y3)
;; (x1 + x2 + x3 + x4, y1 + y2 + y3 + y4)
;; --- ask if we can get to a certain square
;; (assert (and (= 5 (+ x1 x2 x3 x4))
;; 	     (= 5 (+ y1 y2 y3 y4))))

;; ;; (x1 + x2 + x3 + x4 + x5, y1 + y2 + y3 + y4)
;; ;; (x1 + x2 + x3 + x4 + x5, y1 + y2 + y3 + y4 + y5)

;; (declare-fun f (Int) Int)
;; (assert (= (f 5) 3))
;; (assert (= (f 3) 2))
;; ;;(assert (= (f 3) (f 5)))



;;(assert (= p2x x

;; (assert (and (= p1x 0) (= p1y 0)))
;; (assert (and (= p2x (+ p1x x1) (= p2y 0))))
;; (assert (and (= p3x  (= p2y 0))))


;; an x can be -3 -2 -1 1 2 3
;; x1 x2 x3 x4
;; y1 y2 y3 y4
;;(assert false)

;; (declare-fun x () Int)
;; (assert (> (+ x) 3))

;; check (+ 1) is just 1
;; (declare-fun x () Int)
;; (assert (= x (+ 1)))

;; check total-cost is a valid constant name
(declare-fun total-cost () Int)
(assert (= total-cost (+ 1)))


;;(assert (= ( 1 2) (3 4)))
(check-sat)
(get-model)







