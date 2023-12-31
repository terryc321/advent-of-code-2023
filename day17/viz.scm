
#|

day17

sadly today guile and the sdl2 library do not play nice ... here it dies ...
jit.c:5960: fatal: assertion failed


|#
(use-modules (system base compile))

;;(default-optimization-level 9)
(default-optimization-level 9)


(use-modules (ice-9 textual-ports))
(use-modules (ice-9 format)) ;; format common lisp 
(use-modules (ice-9 pretty-print)) ;; pretty-print
(define pp pretty-print)
;;(use-modules (rnrs)) ;; assert 
(use-modules (srfi srfi-1)) ;; first second third ...
(use-modules (srfi srfi-2)) ;; first second third ...
(use-modules (statprof)) ;; statistical profiler

(use-modules ((rnrs)
	      ;;#:select open-pipe close-pipe)
	      #:renamer (symbol-prefix-proc 'rnrs:)))
;; assert

;; regular expression
(use-modules (ice-9 regex)) 

;; pattern matcher ?
(use-modules (ice-9 match))

;; binary io -- dont know if this helped any as read-u8 is for reading ints no??
(use-modules (ice-9 binary-ports))

;; r7rs 
(use-modules ((scheme base)
	      #:renamer (symbol-prefix-proc 'base:)))

;; ---------------- sdl specific stuff ---------------------------


(use-modules (sdl2)
	     (sdl2 rect)
	     (sdl2 events)
             (sdl2 render)
             ((sdl2 surface)
	      ;;#:select open-pipe close-pipe)
	      #:renamer (symbol-prefix-proc 'surface:))
             (sdl2 video)
	     (sdl2 ttf) ;; fonts
	     (sdl2 input keyboard))


(use-modules (ice-9 optargs))


;; --------------------- macros --------------------------
(define-macro (do-list varls . body)
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

;; (chdir "day16")
;; (getcwd)

#|
some io
read-expr : read expression from file

read-all : read all lines of file and slurp into a string list

|#
;; -------------------- sdl specific stuff ---------------
(define win #f)
(define mouse-x 0)
(define mouse-y 0)
(define hash #f)
(define hot-vector #f)
(define hot-count 0)

(define mouse-messages 0)

(define small-font #f)
(define small-font-size 12)

(define large-font #f)
(define large-font-size 50)

(define box-size 50)

(define out-x 0)
(define out-y 0)

(define window-width 0)
(define window-height 0)

;; -------------- model stuff -------------------------------------
(define code #f)
(define code-width #f)
(define code-height #f)

(define my-port #f)
(define data-port #f)

(define path-data #f)
(define cost-data #f)
(define id-data #f)
(define width-data #f)
(define height-data #f)
(define grid-data #f)

(define input #f)
(define example #f)
(define example2 #f)
(define output #f)
(define data #f)
(define data-filename "example1.out")
(define step 0)

;; ----------------------------------------------------

(define (read-next-expr)
  (read my-port))

(define (read-some-expr)
  (let ((i 2000)
	(xs '()))
    (while (> i 0)
      (set! xs (cons (read my-port) xs))
      (set! i (1- i)))
    (reverse xs)))


(define (read-expr f)
  (call-with-input-file f
    (lambda (port)
      (read port))))


(define (read-all-lists f)
  (call-with-input-file f
    (lambda (port)
      (let ((xs '()))
	(letrec ((foo (lambda ()
			(let ((y (read port)))
			  ;;(format #t "~a ~%" line)
			  (cond
			   ((eof-object? y) (reverse xs))
			   (#t (set! xs (cons y xs))
			       (foo)))))))
	  (foo))))))



(define (read-all f)
  (call-with-input-file f
    (lambda (port)
      (let ((lines '()))
	(letrec ((foo (lambda ()
			(let ((line (get-line port)))
			  (format #t "~a ~%" line)
			  (cond
			   ((eof-object? line) (reverse lines))
			   (#t (set! lines (cons line lines))
			       (foo)))))))
	  (foo))))))



(define (char->int ch)
  (let ((n (- (char->integer ch) (char->integer #\0))))
    (when (not (and (>= n 0) (<= n 9)))
      (error "char->int" (list ch 'expected "0 to 9")))
    n))



(define (list->grid v)
  (list->vector (map (lambda (x) (list->vector (map char->int (string->list x))))  v)))


(define (onboard? x y)
  (and (>= x 0)(< x width-data)
       (>= y 0)(< y height-data)))


(define (get-xy vv x y)
  (vector-ref (vector-ref vv y) x))

(define (grid-width vv)
  (vector-length (vector-ref vv 0)))

(define (grid-height vv)
  (vector-length vv))

(define (hash-size h)
  (hash-count (const #t) h))


;; ---------------------------------------------------------------------------------
#|

visualize solution provided by one of the fun-* scheme files

(sdl-ticks)
like ticks milliseconds since sdl started


|#

(define (draw-text ren font str x y)
  (let* ((surface (render-font-solid font str (make-color 255 0 0 0)))
	 (wid (surface:surface-width surface))
	 (hgt (surface:surface-height surface))
	 (texture (surface->texture ren surface)))
    (render-copy ren texture #:srcrect `(0 0 ,wid ,hgt)#:dstrect `(,x ,y ,wid ,hgt))
    ;;(format #t "surface = ~a ~%" surface))
    ))


(define (draw-large-text ren str x y)
  (draw-text ren large-font str x y))

(define (draw-small-text ren str x y)
  (draw-text ren small-font str x y))


#|
draw some sort of grid
1920 x 1080
|#
(define (draw-empty-grid ren)
  ;;(fill-rect ren (make-rect 0 0 window-width window-height) (make-color (list->vector (list 255 0 0))))
  ;; (set-renderer-draw-color! ren 255 255 255 0)
  ;; (fill-rect ren (make-rect 0 0 window-width window-height))
  (let ((x 0)
	(y 0)
	(dx box-size)
	(dy box-size)
	(on 0))
    (set! y dy)
    (while (< y (- window-height (* 2 box-size)))
      (set! x dx)
      (while (< x (- window-width (* 2 box-size)))
	(set-renderer-draw-color! ren 75 0 0 0)  
	(draw-rect ren (make-rect x y dx dy))
	#|
	(set! on (1+ on))
	(when (zero? (modulo on 3))
	(set-renderer-draw-color! ren 0 125 125 0)
	;;(fill-rect ren (make-rect x y (+ x dx) (+ y dy)) (make-color (list->vector (list 0 255 0))))
	(fill-rect ren (make-rect x y dx dy))
	(set! on 0)
	)
	|#
	(set! x (+ x dx))
	)
      (set! y (+ y dy)))))





(define (draw-number-grid ren)
  (let ((x 0) (y 0) ;; x y pixels 
	(dx box-size)
	(dy box-size)
	(on 0)
	(j 0)(k 0)) ;; j k indices into grid data
    (set! y dy)
    (set! j 0)
    (while (< y (- window-height (* 2 box-size)))
      (set! j 0)
      (set! x dx)
      (while (< x (- window-width (* 2 box-size)))
	(set-renderer-draw-color! ren 0 0 0 0)  
	(draw-rect ren (make-rect x y dx dy))
	(draw-small-text ren (format #f "(~a,~a)" x y) x y)
	(when (onboard? j k)
	  (let ((val (get-xy grid-data j k)))
	    ;;(format #t "grid data ; ~a ~a => ~a ~%" j k val)
	  (draw-large-text ren (format #f "~a" val) (+ x (third dx))  y)))
	(set! x (+ x dx))
	(set! j (+ j 1))) ;; while < x
      (set! k (+ k 1))
      (set! y (+ y dy))) ;; while < y
    ))










#|
draw some sort of grid
1920 x 1080
|#
(define (draw-grid ren)
  ;;(fill-rect ren (make-rect 0 0 window-width window-height) (make-color (list->vector (list 255 0 0))))
  (set-renderer-draw-color! ren 255 255 255 0) ;; blanche white
  (set-renderer-draw-color! ren 30 50 70 0) ;; ??   
  (fill-rect ren (make-rect 0 0 2000 1200))
  (let ((x 0)
	(y 0)
	(dx box-size)
	(dy box-size)
	(on 0))
    (set! y dy)
    (while (< y (- window-height (* 2 box-size)))
      (set! x dx)
      (while (< x (- window-width (* 2 box-size)))
	(set-renderer-draw-color! ren 75 0 0 0)  
	(draw-rect ren (make-rect x y dx dy))
	(set! on (1+ on))
	(when (zero? (modulo on 3))
	  (set-renderer-draw-color! ren 0 125 125 0)
	  ;;(fill-rect ren (make-rect x y (+ x dx) (+ y dy)) (make-color (list->vector (list 0 255 0))))
	  (fill-rect ren (make-rect x y dx dy))
	  (set! on 0)
	  )
	(set! x (+ x dx))
	)
      (set! y (+ y dy)))))





#|
draw something ?

/ ne mirror north east
\ nw mirror north-west
| vertical
- horz
. empty

first try draw a star pattern 

|#

(define (half x)
  (floor (/ x 2)))

(define (third x)
  (floor (/ x 3)))


(define (draw-single-mirror ren ch rx ry )
  ;;(fill-rect ren (make-rect 0 0 window-width window-height) (make-color (list->vector (list 255 0 0))))
  (set-renderer-draw-color! ren 255 0 0 0)
  (let ((x 0)
	(y 0)
	(dx box-size)
	(dy box-size))
    (set! x (+ dx (* (- rx out-x) box-size)))
    (set! y (+ dy (* (- ry out-y) box-size)))
    (cond
     ((and
       (>= y dy)
       (>= x dx)
       (< y (- window-height (* 2 box-size)))
       (< x (- window-width (* 2 box-size))))
      (cond
       ((char=? ch #\\)
	(do-list (i '(1 2 3 4 5 6 7 8 9 10));; nw diagonal
		 (draw-line ren x (+ i y) (+ dx x) (+ dy (+ i y)))))
       ((char=? ch #\-)  ;; horz
	(do-list (i '(1 2 3 4 5 6 7 8 9 10))
		 (draw-line ren x (+ i y (half dy)) (+ x dx) (+ i y (half dy)))))
       ((char=? ch #\|) ;; vert
	(do-list (i '(1 2 3 4 5 6 7 8 9 10))
		 (draw-line ren (+ i x (half dx)) y  (+ i x (half dx)) (+ y dy))))
       ((char=? ch #\/) ;; ne diagonal
	(do-list (i '(1 2 3 4 5 6 7 8 9 10));; nw diagonal
		 (draw-line ren x (+ i dy y) (+ dx x) (+ i y))))
       ((char=? ch #\.) #t))))))


;; ----------- dump all mirrors onto screen using line drawing ---- sticks stones--------
(define (draw-all-mirrors ren)
  (cond
   (ren
    (let ((r 0)
	  (s 0))
      (while (< s code-height)
	(set! r 0)
	(while (< r code-width)
	  (cond
	   ((and (>= r 0)(< r code-width)(>= s 0)(< s code-height))
	    (let ((ch (get-xy code r s)))
	    ;;;(format #t "r ~a : s ~a : ch = ~a ~%" r s ch)
	      (draw-single-mirror ren ch r s))))
	  (set! r (+ r 1)))
	(set! s (+ s 1)))))))


#|

|#

(define (smaller-box-size!)
  (set! box-size (- box-size 1))
  (when (< box-size 1)
    (set! box-size 1)))

(define (larger-box-size!)
  (set! box-size (1+ box-size)))


(define (get-expr-and-tally)
  (let ((e (read-next-expr)))
    (match e
      ((x y d t)
       (vector-set! (vector-ref hot-vector y) x t)))))




(define (draw-single-hot-spot ren rx ry )
  (set-renderer-draw-color! ren 0 125 0 0)
  (let ((x 0)
	(y 0)
	(dx box-size)
	(dy box-size))
    (set! x (+ dx (* (- rx out-x) box-size)))
    (set! y (+ dy (* (- ry out-y) box-size)))
    (cond
     ((and
       (>= y dy)
       (>= x dx)
       (< y (- window-height (* 2 box-size)))
       (< x (- window-width (* 2 box-size))))
      (fill-rect ren (make-rect x y dx dy))))))


(define (draw-all-hot-spots ren)
  (cond
   (ren
    (do-list (e path-data)
	     (match e
	       ((x y )
		(draw-single-hot-spot ren x y)))))))


(define (draw-single-board-spot ren rx ry )
  (set-renderer-draw-color! ren 0 0 125 0)
  (let ((x 0)
	(y 0)
	(dx box-size)
	(dy box-size))
    (set! x (+ dx (* (- rx out-x) box-size)))
    (set! y (+ dy (* (- ry out-y) box-size)))
    (cond
     ((and
       (>= y dy)
       (>= x dx)
       (< y (- window-height (* 2 box-size)))
       (< x (- window-width (* 2 box-size))))
      (fill-rect ren (make-rect x y dx dy))))))

(define (draw-all-board-spots ren)
  (cond
   (ren
    (do-list (y (iota height-data))
	     (do-list (x (iota width-data))
		      (draw-single-board-spot ren x y))))))



(define (extract-data s)
  (cond
   ((eq? s 'path) path-data)
   ((eq? s 'id) id-data)
   ((eq? s 'cost) cost-data)
   
   
   (#t #f)))

(define (next-data)
  (let ((d (read data-port)))
    (match d
      (('path path 'id id 'cost cost)
       (set! path-data path)
       (set! id-data id)
       (set! cost-data cost)))))





(define (draw ren)
  (call/cc (lambda (abort)
	     (let* (;;(surface (surface:load-bmp "hello-world.bmp"))
		    (running? #t))  
	       (define (re-render)
		 (let* (
			(dummy 0)
			;;(texture (surface->texture ren surface))
			)
		   
		   (set-renderer-draw-color! ren 255 255 255 0) ;; blacnhe white
		   ;; (set-renderer-draw-color! ren 30 50 70 0)
		   (clear-renderer ren)
		   (set-renderer-draw-color! ren 0 0 0 0)
		   
		   ;;
		   
		   ;;(render-copy ren texture)
		   ;;(draw-line ren 0 0 mouse-x mouse-y)
		   ;;(draw-rect ren (make-rect 0 0 mouse-x mouse-y))
		   
		   ;;(draw-grid ren)
		   ;;(draw-empty-grid ren)

		   
		   ;; (draw-single-mirror ren #\/ 0 0)
		   ;; (draw-single-mirror ren #\/ 1 1)
		   ;; (draw-single-mirror ren #\/ 2 2)

		   ;; (set-renderer-draw-color! ren 0 0 125 0)
		   ;;(draw-line ren box-size box-size (* 2 box-size)(* 2 box-size))
		   #|
		   (draw-single-mirror ren #\/ 0 0)
		   (draw-single-mirror ren #\\ 1 0)
		   (draw-single-mirror ren #\| 2 0)
		   (draw-single-mirror ren #\- 3 0)
		   |#

		   (draw-all-board-spots ren)
		   (draw-all-hot-spots ren)
		   (draw-number-grid ren)
		   
		   ;; (draw-all-mirrors ren)
		   
		   (set-renderer-draw-color! ren 0 0 0 0)
		   ;; (draw-large-text ren (format #f "mouse [~a,~a] : out [~a,~a] : step [~a] : hits [~a]"
		   ;; 			     mouse-x mouse-y out-x out-y step hot-count)
		   ;; 		 25 20)
		   ;;(draw-large-text ren (format #f "path ~a" (extract-data 'path)) 25 20)
		   (draw-small-text ren (format #f "path ~a"
						(extract-data 'path))
				    25 5)
		   (draw-small-text ren (format #f "id ~a : cost ~a"
						(extract-data 'id)
						(extract-data 'cost))
				    25 20)
		   
		   
		   
		   ;; ----- display 
		   (present-renderer ren)))
	       ;; render
	       (re-render)  
	       ;; game loop
	       (while running?
		 (let ((event (poll-event)))	
		   (when event
		     (cond
		      ((quit-event? event)
		       (display "bye!\n")
		       (set! running? #f))
		      ((keyboard-event? event)
		       (let ((key (keyboard-event-key event))
			     (scan (keyboard-event-scancode event))
			     (mod (keyboard-event-modifiers event)))
			 #t
			 (cond
			  (#f ;;(keyboard-event-repeat? event)
			   ;;(format #t "keyboard repeat key ~a : scan ~a : mods ~a ~%"   key scan mod)
			   )
			  ((or (keyboard-event-repeat? event) (keyboard-down-event? event))		    
			   ;;(format #t "keyboard down/press key ~a : scan ~a : mods ~a ~%"   key scan mod)
			   #t
			   (cond
			    ((key-pressed? 'escape) (abort #t))
			    ((key-pressed? 'w)
			     (set! step (1+ step))
			     (next-data) ;; read another path		  
			     )
			    ((key-pressed? 's) (set! step (1- step)) (when (< step 0) (set! step 0)))		 
			    ((key-pressed? 'down) (set! out-y (1+ out-y)))
			    ((key-pressed? 'up) (set! out-y (1- out-y)))
			    ((key-pressed? 'left) (set! out-x (1- out-x)))
			    ((key-pressed? 'right) (set! out-x (1+ out-x)))
			    ((key-pressed? 'minus) (smaller-box-size!) #t)
			    ((key-pressed? 'equals) (larger-box-size!) #t))		
			   (re-render))
			  ((keyboard-up-event? event)
			   ;;(format #t "keyboard up/release key ~a : scan ~a : mods ~a ~%"   key scan mod)
			   #t
			   )
			  )))
		      ((mouse-motion-event? event)
		       (let ((x (mouse-motion-event-x event))
			     (y (mouse-motion-event-y event))
			     (rel-x (mouse-motion-event-x-rel event))
			     (rel-y (mouse-motion-event-y-rel event)))
			 (set! mouse-x x)
			 (set! mouse-y y)
			 #t
			 ;;(format #t "mouse motion ~a ~a : ~a ~a ~%" x y rel-x rel-y)
			 ;;(re-render)
			 ))
		      ((window-shown-event? event)
		       (set! window-width (window-size win))
		       (let ((v (window-event-vector event)))
			 (set! window-width (car v))
			 (set! window-height (cadr v))
			 (format #t "window-shown-event ~a ~%" v)))
		      
		      ((window-exposed-event? event)
		       #t
		       #|
		       (format #t "window exposed~%")
		       (let ((v (window-event-vector event)))
		       (set! window-width (car v))
		       (set! window-height (cadr v))
		       (format #t "window-exposed-event ~a ~%" v))	    
		       (re-render)
		       |#
		       )
		      ((window-resized-event? event)
		       (set! window-width (window-size win))
		       (let ((v (window-event-vector event)))
			 (set! window-width (car v))
			 (set! window-height (cadr v))
			 ;;(format #t "window-event-vector ~a ~%" v)
			 )
		       ;; (call-with-values  (window-size win) (lambda (w h)
		       ;; 					   (format #t "window width ~a ~%" w)
		       ;; 					   (format #t "window height ~a ~%" h)))	      
		       ;;(format #t "window resized event ~a ~%" event)
		       (re-render)
		       )
		      
		      ;; (call-with-values (window-size win)
		      ;;   (lambda (w h)
		      ;; 	(format #t "window size changed to ~a by ~a ~%" w h))))
		      (#t #f)))))))))



(define (setup)

  (set! input  (list->grid (read-all "input")))
  (set! example (list->grid (read-all "example")))
  ;;(set! example2 (list->grid (read-all "example2")))

  (format #t "press a key to see next ~%")
  (set! data-port (open-input-file "example1.out"))
  (next-data)
  (set! width-data (grid-width example))
  (set! height-data (grid-height example))
  (set! grid-data example)
  )






;;(format #t "file portmyport = ~a ~%" my-port)

(define (viz)

  (sdl-init)
  (ttf-init)
  (set! small-font (load-font "/home/terry/advent-of-code/2023/day16/ProggyClean.ttf" small-font-size))
  (format #t "small font = ~a ~%" small-font)

  (set! large-font (load-font "/home/terry/advent-of-code/2023/day16/ProggyClean.ttf" large-font-size))
  (format #t "large font = ~a ~%" large-font)

  (format #t "input ~a ~%" input)
  (format #t "first input ~%~a ~%" (vector-ref input 0))


  (call-with-window (make-window #:title "hello world"
				 #:position '(0 0)
				 #:maximize? #t
				 #:minimize? #t
				 #:resizable? #t
				 #:border? #t
				 #:high-dpi? #t)
		    (lambda (window)
		      (set! win window)
		      (call-with-renderer (make-renderer window) draw)))

  (sdl-quit)

  )


(define (run)
  (setup)
  (viz))

