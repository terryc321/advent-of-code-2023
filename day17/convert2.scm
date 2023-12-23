


#|

start at x y 0 0
cost c is 0

start taking one step to right
right( 1 , 0 , (cost 1 0) , [(1,0),(0,0)] , 1)
down( 0 , 1 , (cost 0 1) , [(0,1),(0,0)] , 1)


val width = 13;
val height = 13;

fun cost x y = x + y;

fun left x y c p n =
    let val _ = let val x2 = x     (* up = neg Y *)
		    val y2 = y - 1
		in if y2 >= 0 then   (* keeps y2 on board ? *)
		       let val c2 = c + (cost x2 y2)
		       in up x2 y2 c2 ((x2,y2)::p) (n+1)  (* go up *)
		       end
		   else
		       ()
		end		    
	val _ = let val x2 = x     (* down = pos Y *)
		    val y2 = y + 1
		in if y2 < height then   (* keeps y2 on board ? *)
		       let val c2 = c + (cost x2 y2)
		       in down x2 y2 c2 ((x2,y2)::p) (n+1)  (* go down *)
		       end
		   else
		       ()
		end		    
        val _ = if n < 3 then
		    let val x2 = x - 1   (* left = neg X *)
			val y2 = y 
		    in if x2 >= 0 then   (* keeps x2 on board ? *)
			   let val c2 = c + (cost x2 y2)
			   in left x2 y2 c2 ((x2,y2)::p) (n+1)  (* go left *)
			   end
		       else
			   ()
		    end		    
		else ()
    in ()
    end		       
and
right x y c p n =
if n >= 3 then
    let val _ = up (x + 1) y 1
	val _ = down x y 1
    in ()
    end		       
else
    let val _ = right x y (n + 1)
	val _ = up x y 1
	val _ = down x y 1
    in ()
    end 
and
up x y c p n =
if n >= 3 then
    let val _ = left (x + 1) y 1
	val _ = right x y 1
    in ()
    end		       
else
    let val _ = up x y (n + 1)
	val _ = left x y 1
	val _ = right x y 1
    in ()
    end 
and
down x y c p n =
if n >= 3 then
    let val _ = left (x + 1) y 1
	val _ = right x y 1
    in ()
    end		       
else
    let val _ = down x y (n + 1)
	val _ = left x y 1
	val _ = right x y 1
    in ()
    end ;

|#


(import scheme)

(import simple-exceptions)

(import (chicken string))
(import (chicken pretty-print))
(define pp pretty-print)

(import (chicken io))
(import (chicken format))
(import (chicken sort))
(import (chicken file))
(import (chicken process-context))
;; (change-directory "day15")
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

;;--------------------------------------

;; change input file ! 
(define (get-input) (call-with-input-file "input"
		      (lambda (port)
			(read port))))

(define input (get-input))

(define example #f)

;;(set! input example)

(define (1+ x) (+ x 1))
(define (1- x) (- x 1))

(define (10+ x) (+ x 10))
(define (10- x) (- x 10))

(define rest cdr)

;;------------------------------------------

(define (read-all f)
  (call-with-input-file f
    (lambda (port)
      (let ((lines '()))
	(letrec ((foo (lambda ()
			(let ((line (read-line port)))
			  (format #t "~a ~%" line)
			  (cond
			   ((eof-object? line) (reverse lines))
			   (#t (set! lines (cons line lines))
			       (foo)))))))
	  (foo))))))


(define (char->int ch)
  (let ((n (- (char->integer ch) (char->integer #\0))))
    (cond
     ((and (>= n 0) (<= n 9)) #t)
     (#t (set! fails (cons `((char->int ,ch) ,n expected 0 to 9) fails))))
    n))


(define (list->grid v)
  (list->vector (map (lambda (x) (list->vector (map char->int (string->list x))))  v)))

(define input  (list->grid (read-all "input")))
(define cost-input  (list->grid (read-all "input")))


(define example (list->grid (read-all "example")))
(define cost-example (list->grid (read-all "example")))

;;(define example2 (list->grid (read-all "example2")))

(define (get-xy vv x y)
  (vector-ref (vector-ref vv y) x))

(define (grid-width vv)
  (vector-length (vector-ref vv 0)))

(define (grid-height vv)
  (vector-length vv))

(define (set-xy! vv x y z)
  (vector-set! (vector-ref vv y) x z))

  
;; BUG set-xy! vv j #f
(define (flatten-cost vv)
  (do-list (i (iota (grid-width vv)))
	   (do-list (j (iota (grid-height vv)))
		    (set-xy! vv i j #f))))



(define (grid->sml vv var)
  (format #t "~%val ~a = [~%" var)
  (do-list (j (iota (grid-height vv)))
	   (when (not (= j 0))
	     (format #t ",~%"))
	   (format #t "[")
	   (do-list (i (iota (grid-width vv)))
		    (when (not (= i 0))
		      (format #t ","))
	   	    (format #t "~a" (get-xy vv i j)))	     
	   (format #t "]"))	   
  (format #t "];~%"))


(define (grid->sml0 vv var)
  (format #t "~%val ~a = [~%" var)
  (do-list (j (iota (grid-height vv)))
	   (when (not (= j 0))
	     (format #t ",~%"))
	   (format #t "[")
	   (do-list (i (iota (grid-width vv)))
		    (when (not (= i 0))
		      (format #t ","))
	   	    (format #t "~a" 0))
	   (format #t "]"))	   
  (format #t "];~%"))


#|
	val _ = let val x2 = x     (* down = pos Y *)
		    val y2 = y + 1
		in if y2 < height then   (* keeps y2 on board ? *)
		       let val c2 = c + (cost x2 y2)
		       in down x2 y2 c2 ((x2,y2)::p) (n+1)  (* go down *)
		       end
		   else
		       ()
		end		    

|#
(define (fexp title a com-a b com-b c d e com-cde f)
  (format #t "val _ = let val x2 = x + ~a (* ~a *)  (* ~a *) ~%" a com-a title)
  (format #t "            val y2 = y + ~a (* ~a *)~%" b com-b)
  (format #t "        in if ~a ~a ~a then (* ~a *) ~%" c d e com-cde)
  (format #t "              let val c2 = c + (cost x2 y2)~%")
  (format #t "                in ~a x2 y2 c2 ((x2,y2)::p) (n+1)~%" f)
  (format #t "              end~%")
  (format #t "           else ()~%")
  (format #t "         end~%"))


;; note  minus 1 is written squiggle 1 in standard ml

(define (down)
  (fexp "down" 0 "x unchanged " 1 " down y2 = y + 1 " 'y2 '< 'height " y2 < height " 'down)
  )

(define (up)
  (fexp "up" 0 "x unchanged " '~1 " up y2 = y + ~1 " 'y2 '> 0 " y2 > 0 " 'up)
  ) 

(define (left)
  (fexp "left" '~1 "x + ~1 " 0 " left y unchanged " 'x2 '> 0 " x2 > 0 still onboard " 'left)  
  )

(define (right)
  (fexp "right" '1 "x + 1 " 0 " right y unchanged " 'x2 '< 'width " x2 < width still onboard " 'right)  
  )


#|
val _ = if n < 3 then
		    let val x2 = x - 1   (* left = neg X *)
			val y2 = y 
		    in if x2 >= 0 then   (* keeps x2 on board ? *)
			   let val c2 = c + (cost x2 y2)
			   in left x2 y2 c2 ((x2,y2)::p) (n+1)  (* go left *)
			   end
		       else
			   ()
		    end		    
else ()
|#
(define (gexp title a com-a b com-b c d e com-cde f com-f)
  (format #t "val _ = if n < 3 then  (* ~a *) ~%" title)
  (format #t "                 let val x2 = x + ~a (* ~a *) ~%" a com-a)
  (format #t "                     val y2 = y + ~a (* ~a *) ~%" b com-b)
  (format #t "                 in if ~a ~a ~a then (* ~a *)  ~%" c d e com-cde)
  (format #t "                    let val c2 = c + (cost x2 y2)              ~%")
  (format #t "                        in ~a x2 y2 c2 ((x2,y2)::p) (n + 1) (* ~a *) ~%" f com-f)
  (format #t "                        end          ~%")
  (format #t "                    else ()             ~%")
  (format #t "                 end                 ~%")
  (format #t "         else ()                  ~%")  
  )


(define (right-right)
  (gexp "right-right" 1  "x + 1 go right" 0 "no change y" 'x2 '< 'width  " x2 < width  ok " 'right "go right")
  )

(define (left-left)
  (gexp "left-left" '~1  "x + ~1 go left" 0 "no change y" 'x2 '> 0  " x2 > 0  ok " 'left "go left")
  )

(define (up-up)
  (gexp "up-up" 0  "x2 no change" '~1 "y + ~1" 'y2 '> 0  " y2 > 0  ok " 'up "go up")
  )

(define (down-down)
  (gexp "down-down" 0  "x2 no change" 1 "y + 1" 'y2 '< 'height  " y2 < height  ok " 'down "go down")
  )

;; up up
  
(define (go-up)
  (left)
  (right)
  (up-up)
  )

(define (go-down)
  (left)
  (right)
  (down-down)
  )

(define (go-right)
  (up)
  (down)
  (right-right)
  )
  
(define (go-left)
  (up)
  (down)
  (left-left)
  )

(define (intro)
  (format #t "val width = 13;~%")
  (format #t "val height = 13;~%")
  (format #t "fun cost x y = x + y;~%~%")
  )



#|
  (format #t "if c > (sumcost x y) then ~%")
  (format #t "    report  ~%")
  |#

(define (finish-line?)
  (format #t "if finish x y c then () else ~%")
  ;; (format #t " print \"reached \" ^ (Int.toString x) ^ \" , \" (Int.toString y) ^ \" with cost \" ^ ~%")
  ;; (format #t " (Int.toString c) ~%")  
  ;; (format #t "else ")
  )

(define (report-cost?)
  (format #t "if reportCost x y c then () else ~%")
  )


;; (define (finish-def)
;;   (format #t "fun finish x y = if x = width then ~%")
;;   (format #t "                     if y = height then true ~%")
;;   (format #t "                     else false  ~%")
;;   (format #t "                  else false;  ~%~%"))

  

(define (all)

  (intro)
  ;;(finish-def)
  
  (format #t "fun up x y c p n =  (* ============= up procedure ================ *) ~%")
  (finish-line?)
  (report-cost?)
  (format #t " let ")
  (go-up)
  (format #t "in () end~%")
  (format #t "and ")

  (format #t "  down x y c p n =  (* ============ down procedure =============== *)~%")
  (finish-line?)
  (report-cost?)
  (format #t " let ")
  (go-down)
  (format #t "in () end~%")
  (format #t "and ")

  (format #t "   left x y c p n = (* ========== left procedure ============= *) ~%")  
  (finish-line?)
  (report-cost?)
  (format #t " let ")
  (go-left)
  (format #t "in () end~%")
  (format #t "and ")

  
  (format #t "  right x y c p n =  (* ========= right procedure ========= *)~%")  
  (finish-line?)
  (report-cost?)
  (format #t " let ")
  (go-right)
  (format #t "in () end~%")

  (format #t ";~%~%")
  )




  

