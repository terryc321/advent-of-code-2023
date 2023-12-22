
#|

because guile jit just in time compiler ate the bed and crashed using sdl2
had to port code to use chicken schme instead

in the notes
https://gitlab.com/chicken-sdl2/chicken-sdl2/-/blob/main/docs/enums.md#window-flags

fullscreen                    SDL_WINDOW_FULLSCREEN
fullscreen-desktop            SDL_WINDOW_FULLSCREEN_DESKTOP
opengl                        SDL_WINDOW_OPENGL
shown                         SDL_WINDOW_SHOWN
hidden                        SDL_WINDOW_HIDDEN
borderless                    SDL_WINDOW_BORDERLESS
resizable                     SDL_WINDOW_RESIZABLE
minimized                     SDL_WINDOW_MINIMIZED
maximized                     SDL_WINDOW_MAXIMIZED
input-grabbed                 SDL_WINDOW_INPUT_GRABBED
input-focus                   SDL_WINDOW_INPUT_FOCUS
mouse-focus                   SDL_WINDOW_MOUSE_FOCUS
foreign                       SDL_WINDOW_FOREIGN
allow-high-dpi                SDL_WINDOW_ALLOW_HIGHDPI           (SDL 2.0.1+)
mouse-capture                 SDL_WINDOW_MOUSE_CAPTURE           (SDL 2.0.4+)
always-on-top                 SDL_WINDOW_ALWAYS_ON_TOP           (SDL 2.0.5+)

^^^
these on the left are the symbols to use for chcken scheme !!!


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

;; Compatible with both CHICKEN 4 and CHICKEN 5.
;; sdl2 egg
;; sdl2-image egg
;; sdl2-ttf egg
(cond-expand
  (chicken-4 (use (prefix sdl2 "sdl2:")
                  (prefix sdl2-image "img:")))
  (chicken-5 (import (prefix sdl2 "sdl2:")
                     (prefix sdl2-image "img:")
		     (prefix sdl2-ttf "ttf:"))))




;; -------------------- sdl specific stuff ---------------
(define win #f)
(define mouse-x 0)
(define mouse-y 0)
(define hash #f)
(define hot-vector #f)
(define hot-count 0)

(define mouse-messages 0)

(define small-font #f)
(define small-font-size 30)

(define large-font #f)
(define large-font-size 50)

(define box-size 50)

(define out-x 0)
(define out-y 0)

(define window-width 800)
(define window-height 800)

(define font #f)
(define window #f)
(define renderer #f)
(define text #f)
(define surface #f)
(define texture #f)

(define text-texture #f)
(define text-surface #f)


;;----- 2nd window


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
(define (1+ x)(+ x 1))
(define (1- x)(- x 1))


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
			(let ((line (read-line port)))
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



#|
------------------ draw hot spots ----------------
|#
(define (draw-single-hot-spot ren rx ry )
  (sdl2:render-draw-color-set! renderer (sdl2:make-color 0 125 0 0))
  (let ((x 0)
	(y 0)
	(dx box-size)
	(dy box-size)
	(border 3)
	)
    (set! x (+ dx (* (- rx out-x) box-size)))
    (set! y (+ dy (* (- ry out-y) box-size)))
    (cond
     ((and
       (>= y dy)
       (>= x dx)
       (< y (- window-height (* 2 box-size)))
       (< x (- window-width (* 2 box-size))))
      (sdl2:render-fill-rect! renderer (sdl2:make-rect (+ x border) (+ y border)
						       (- dx (* border 2))
						       (- dy (* border 2))
						       ))))))



(define (draw-all-hot-spots ren)
  (cond
   (ren
    (do-list (e path-data)
	     (match e
	       ((x y )
		(draw-single-hot-spot ren x y)))))))



#|
------------ draws the board -------------------
|#
(define (draw-single-board-spot ren rx ry )
  (sdl2:render-draw-color-set! renderer (sdl2:make-color 0 0 125 255))
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
      (sdl2:render-fill-rect! renderer (sdl2:make-rect x y dx dy))))))

(define (draw-all-board-spots ren)
  (cond
   (ren
    (do-list (y (iota height-data))
	     (do-list (x (iota width-data))
		      (draw-single-board-spot renderer x y))))))
;;------------------------------------------------------------------




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
       (set! cost-data cost)
       ;;(format #t "path id ~a~%path ~a ~%" id path-data)
       )
      )))





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

;;; from chicken eggsweeper demo ----- mainly for main loop 
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; MAIN LOOP

;; (define (main-loop)
;;   ;; An event struct that will be overwritten by sdl2:wait-event!.
;;   ;; Passing an existing event struct to sdl2:wait-event! is optional,
;;   ;; but it reduces the amount of garbage, because sdl2:wait-event!
;;   ;; allocates a new event if none is passed. Since there may be many
;;   ;; events handled per second, that small bit of garbage can add up.
;;   (define event (sdl2:make-event))

;;   (restart!)

;;   ;; Create a continuation that can be called to exit the main loop.
;;   (let/cc exit-main-loop!
;;     ;; Loop forever (until exit-main-loop! is called).
;;     (while #t
;;       (when (any-dirty-tiles?)
;;         (redraw-dirty-tiles! window-surf *game-over* *focused-tile*)
;;         (sdl2:update-window-surface! window))

;;       ;; Wait for the next event, then handle it.
;;       (handle-event (sdl2:wait-event! event) exit-main-loop!))))


;; ;;; Disable various irrelevant event types, to avoid wasted time and
;; ;;; memory garbage from handling them.
;; (set! (sdl2:event-state 'text-editing) #f)
;; (set! (sdl2:event-state 'text-input) #f)
;; (set! (sdl2:event-state 'mouse-wheel) #f)
;; (set! (sdl2:event-state 'finger-down) #f)
;; (set! (sdl2:event-state 'finger-up) #f)
;; (set! (sdl2:event-state 'finger-motion) #f)
;; (set! (sdl2:event-state 'multi-gesture) #f)


;; (define (handle-event ev exit-main-loop!)
;;   (case (sdl2:event-type ev)
;;     ;; Window exposed, etc.
;;     ((window)
;;      (sdl2:update-window-surface! window))

;;     ;; User requested app quit (e.g. clicked the close button).
;;     ((quit)
;;      (exit-main-loop! #t))

;;     ;; Keyboard key pressed
;;     ((key-down)
;;      (case (sdl2:keyboard-event-sym ev)
;;        ;; Escape quits the program
;;        ((escape)
;;         (exit-main-loop! #t))

;;        ;; R restarts the game with the current difficulty level
;;        ((r)
;;         (restart!))

;;        ;; Number row keys switch difficulty level
;;        ((n-1)  (select-level! 1) (restart!))
;;        ((n-2)  (select-level! 2) (restart!))
;;        ((n-3)  (select-level! 3) (restart!))
;;        ((n-4)  (select-level! 4) (restart!))

;;        ;; Arrow keys or keypad numbers try to move focus
;;        ((kp-1)         (try-move-focus! -1  1))
;;        ((kp-2   down)  (try-move-focus!  0  1))
;;        ((kp-3)         (try-move-focus!  1  1))
;;        ((kp-4   left)  (try-move-focus! -1  0))
;;        ((kp-6  right)  (try-move-focus!  1  0))
;;        ((kp-7)         (try-move-focus! -1 -1))
;;        ((kp-8     up)  (try-move-focus!  0 -1))
;;        ((kp-9)         (try-move-focus!  1 -1))

;;        ;; Return or keypad enter tries to open focused tile
;;        ((return kp-enter)
;;         (when (and *focused-tile* (not *game-over*))
;;           (try-open-tile! *focused-tile*)))

;;        ;; Space or keypad 0 tries to toggle flag focused tile
;;        ((space kp-0)
;;         (when (and *focused-tile* (not *game-over*))
;;           (try-flag-tile! *focused-tile*)))))

;;     ;; Mouse button pressed
;;     ((mouse-button-down)
;;      (let* ((mouse-x (sdl2:mouse-button-event-x ev))
;;             (mouse-y (sdl2:mouse-button-event-y ev))
;;             (tile (screen-point->tile mouse-x mouse-y *board*)))
;;        (case (sdl2:mouse-button-event-button ev)
;;          ;; If Ctrl is not being held, left mouse button tries to open
;;          ;; the tile. But if Ctrl is being held, left mouse button
;;          ;; tries to toggle flag, acting like right mouse button.
;;          ((left)
;;           (when (and tile (not *game-over*))
;;             (if (not (member 'ctrl (sdl2:mod-state)))
;;                 (try-open-tile! tile)
;;                 (try-flag-tile! tile))))

;;          ;; Right mouse button tries to toggle flag on clicked tile
;;          ((right)
;;           (when (and tile (not *game-over*))
;;             (try-flag-tile! tile))))))

;;     ;; Mouse cursor moved
;;     ((mouse-motion)
;;      (let* ((mouse-x (sdl2:mouse-motion-event-x ev))
;;             (mouse-y (sdl2:mouse-motion-event-y ev))
;;             (tile (screen-point->tile mouse-x mouse-y *board*)))
;;        ;; Focus on the tile that the mouse is pointing at.
;;        (when (not *game-over*)
;;          (focus-on-tile! tile))))))


;; (sdl2:fill-rect! (sdl2:window-surface window)
;;                  #f
;;                  (sdl2:make-color 255 255 255))

;; (sdl2:fill-rect! (sdl2:window-surface window)
;;                  (sdl2:make-rect 250 250 50 50)
;;                  (sdl2:make-color 0 0 255))


;; (main-loop)

#|
  
    
  ;;(sdl2:update-window-surface! window)
  ;;(sdl2:delay! 5000)

  ;; (sdl2:sdl-init)
  ;; (ttf-init)
  ;; (set! small-font (load-font "/home/terry/advent-of-code/2023/day16/ProggyClean.ttf" small-font-size))
  ;; (format #t "small font = ~a ~%" small-font)

  ;; (set! large-font (load-font "/home/terry/advent-of-code/2023/day16/ProggyClean.ttf" large-font-size))
  ;; (format #t "large font = ~a ~%" large-font)

  ;; (format #t "input ~a ~%" input)
  ;; (format #t "first input ~%~a ~%" (vector-ref input 0))


  ;; (call-with-window (make-window #:title "hello world"
  ;; 				 #:position '(0 0)
  ;; 				 #:maximize? #t
  ;; 				 #:minimize? #t
  ;; 				 #:resizable? #t
  ;; 				 #:border? #t
  ;; 				 #:high-dpi? #t)
  ;; 		    (lambda (window)
  ;; 		      (set! win window)
  ;; 		      (call-with-renderer (make-renderer window) draw)))

  ;; (sdl-quit)
  
  

|#


#|

mouse stuff

      ;; Mouse button pressed
      ((mouse-button-down)
       (let* ((mouse-x (sdl2:mouse-button-event-x ev))
              (mouse-y (sdl2:mouse-button-event-y ev))
	      )
              ;;(tile (screen-point->tile mouse-x mouse-y *board*)))
	 (case (sdl2:mouse-button-event-button ev)
           ;; If Ctrl is not being held, left mouse button tries to open
           ;; the tile. But if Ctrl is being held, left mouse button
           ;; tries to toggle flag, acting like right mouse button.
           ((left) #f)
	   
            ;; (when (and tile (not *game-over*))
            ;;   (if (not (member 'ctrl (sdl2:mod-state)))
            ;;       (try-open-tile! tile)
            ;;       (try-flag-tile! tile))))

           ;; Right mouse button tries to toggle flag on clicked tile
           ((right) #f)
	   )))
            ;; (when (and tile (not *game-over*))
            ;;   (try-flag-tile! tile))))))

      ;; Mouse cursor moved
      ((mouse-motion)
       (let* ((mouse-x (sdl2:mouse-motion-event-x ev))
              (mouse-y (sdl2:mouse-motion-event-y ev))
	      )
	 #f))
      
|#

#|
what was the need to feel like putting all routines inside another routine ?

(format #t "file portmyport = ~a ~%" my-port)
complete re-write based on how chicken scheme implements sdl2 ffi

|#



(define (handle-event ev exit-main-loop!)
  (case (sdl2:event-type ev)
    ;; Window exposed, etc.
    ((window)
     (case (sdl2:window-event-event ev)
       ((size-changed)
	(call-with-values
	    (lambda () (sdl2:window-size window))
	  (lambda (w h) (set! window-height h)
		  (set! window-width w)))))
     
     (sdl2:render-present! renderer) ;; try this instead
     ;;(sdl2:update-window-surface! window)
     )
    ;; window resize
    ;; set window-height window-width

    
    ;; User requested app quit (e.g. clicked the close button).
    ((quit)   (exit-main-loop! #t))
    ;; Keyboard key pressed
    ((key-down)
     (format #t "key-down : sym ~a ~%" (sdl2:keyboard-event-sym ev))
     (case (sdl2:keyboard-event-sym ev)
       ;; Escape quits the program
       ((escape)
        (exit-main-loop! #t))

       ((w)
	(set! step (1+ step))
	(next-data) ;; read another path
	
	)
       ((s) (set! step (1- step)) (when (< step 0) (set! step 0)))		 
       ((down) (set! out-y (1+ out-y)))
       ((up) (set! out-y (1- out-y)))
       ((left) (set! out-x (1- out-x)))
       ((right) (set! out-x (1+ out-x)))
       ((minus) (smaller-box-size!) #t)
       ((equals) (larger-box-size!) #t)		
       
       ;; R restarts the game with the current difficulty level
       ((r) (redraw!))
       ;;(restart!))

       ((a) (format #t "user pressed a !~%"))
       ;; Number row keys switch difficulty level
       ((n-1)  #f) ;;(select-level! 1) (restart!))
       ((n-2)  #f) ;;(select-level! 2) (restart!))
       ((n-3)  #f) ;;(select-level! 3) (restart!))
       ((n-4)  #f) ;;(select-level! 4) (restart!))

       ;; Arrow keys or keypad numbers try to move focus
       ((kp-1)         #f) ;;(try-move-focus! -1  1))
       ((kp-2   down)  #f) ;;(try-move-focus!  0  1))
       ((kp-3)         #f) ;;(try-move-focus!  1  1))
       ((kp-4   left)  #f) ;;(try-move-focus! -1  0))
       ((kp-6  right)  #f) ;;(try-move-focus!  1  0))
       ((kp-7)         #f) ;;(try-move-focus! -1 -1))
       ((kp-8     up)  #f) ;;(try-move-focus!  0 -1))
       ((kp-9)         #f) ;;(try-move-focus!  1 -1))

       ;; Return or keypad enter tries to open focused tile
       ((return kp-enter) #f) ;;
       ;; (when (and *focused-tile* (not *game-over*))
       ;;   (try-open-tile! *focused-tile*))

       ;; Space or keypad 0 tries to toggle flag focused tile
       ((space kp-0) #f) ;;
       ;; (when (and *focused-tile* (not *game-over*))
       ;;   (try-flag-tile! *focused-tile*))
       
       
       ) ;; end of case key-down
     (redraw!)
     );; key-down

    ;; mouse-stuff-goes-here
    
    ))
;; ---------- handle event ------------


(define (draw-text ren font str x y)
  (let* ((text-surface (ttf:render-text-solid font str (sdl2:make-color 255 0 0 255)))
	 (texture (sdl2:create-texture-from-surface ren text-surface)))
    (call-with-values (lambda () (ttf:size-text font text))
      (lambda (w h)
	(sdl2:render-copy! ren texture #f (sdl2:make-rect x y w h))))))

;; optional args ??
;; #:srcrect #f ;;(sdl2:make-rect 0 0 w h)
;; #:dstrect (sdl2:make-rect x y w h))))))


(define (draw-large-text ren str x y)
  (draw-text ren large-font str x y))

(define (draw-small-text ren str x y)
  (draw-text ren small-font str x y))



(define (redraw!)
  
  ;; all white
  (sdl2:render-draw-color-set! renderer (sdl2:make-color 255 255 255))
  (sdl2:render-clear! renderer)
  
  ;; black square
  (sdl2:render-draw-color-set! renderer (sdl2:make-color 0 0 0))
  (sdl2:render-fill-rect! renderer (sdl2:make-rect 0 0 50 50))
  ;; blue square 50 x 50 wide 
  (sdl2:render-draw-color-set! renderer (sdl2:make-color 0 0 255))
  (sdl2:render-fill-rect! renderer (sdl2:make-rect 250 250 50 50))
  ;; red square
  (sdl2:render-draw-color-set! renderer (sdl2:make-color 255 0 0))
  ;; red square outlines
  (sdl2:render-draw-rect! renderer (sdl2:make-rect 350 350 50 50))
  (sdl2:render-draw-rect! renderer (sdl2:make-rect 400 350 50 50))
  (sdl2:render-draw-rect! renderer (sdl2:make-rect 450 350 50 50))
  ;; draw the board
  (draw-all-board-spots renderer)
  ;; draw hot spots
  (draw-all-hot-spots renderer)


  
  ;; call with values  takes two thunks - procedures no args
  ;;   (call-with-values (lambda () ..) (lambda () ...))
  ;;
  #|
  (call-with-values (lambda () (ttf:size-text large-font text))
    (lambda (w h)
      ;;(format #t "size width ~a : height ~a ~%" w h )
      (sdl2:render-copy! renderer text-texture (sdl2:make-rect 0 0 w h)
			 (sdl2:make-rect 20 25 w h))))
|#
  (draw-small-text  renderer (format #f "hi-i am small text ~a" (if (null? path-data)
								    '()
								    (car path-data)))
		    60 10)
   
        

  ;; text-texture is hello world writ large
  ;;(sdl2:render-copy! renderer text-texture #f #f) 
  
  ;; show on window
  (sdl2:render-present! renderer)
  )  ;; ------ render! -------------


;; just loop 
(define (main-loop)
  (define event (sdl2:make-event))
  (call/cc (lambda (exit-main-loop!)
	     (redraw!)
	     ;; Loop forever (until exit-main-loop! is called).
	     (do-while #t
		       (next-data)
		       (redraw!)

		       ;; Wait for the next event, then handle it.
		       (handle-event (sdl2:wait-event! event) exit-main-loop!)
		       #|
		       (let ((events (sdl2:peek-events 1)))
			 (cond
			  ((null? events) #f)
			  (#t (set! event (car(sdl2:get-events! 1)))
			      (handle-event event exit-main-loop!))))
		       |#
		       ))))



#|

ttf fonts to do


|#
(define (viz)

  ;; ---------- entry point -----------

  (sdl2:set-main-ready!)
  (sdl2:init!)
  (ttf:init!)

  ;;(define font (ttf:open-font "ComicNeue-Regular.otf" 40))
  (set! large-font (ttf:open-font "/home/terry/advent-of-code/2023/day16/ProggyClean.ttf" large-font-size))  
  (set! small-font (ttf:open-font "/home/terry/advent-of-code/2023/day16/ProggyClean.ttf" small-font-size))  
  
  ;;(define-values (w h) (ttf:size-utf8 font text))
  ;;(define window (sdl2:create-window! text 'centered 'centered w h))
  (set! window (sdl2:create-window! "Hello, World!"
				    0 0
				    window-width window-height
				    '(resizable ) ;; fullscreen fullscreen-desktop 
				    ))
  ;;(set! surface (sdl2:window-surface window))
  (set! renderer (sdl2:create-renderer! window))

  (set! text "Hello, World!")
  (set! text-surface (ttf:render-text-solid large-font text (sdl2:make-color 255 0 0 255)))
  (set! text-texture (sdl2:create-texture-from-surface renderer text-surface))
  
  
  ;;(set! renderer (sdl2:create-software-renderer! surface))
  ;;(set! texture (create-texture-from-surface renderer surface))
  ;;;(set! texture (create-texture renderer format?? access?? 600 400))

  #|
  (let ((text-surface (ttf:render-utf8-shaded large-font text (sdl2:make-color 0 0 0) (sdl2:make-color 255 255 255))))
    (sdl2:blit-surface! text-surface #f surface #f))

  (sdl2:blit-surface! surface #f (sdl2:window-surface window) #f)
  (sdl2:delay! 3000)
  |#

  
	      ;;(sdl2:window-surface window) #f))
  
  
  ;;; Disable various irrelevant event types, to avoid wasted time and
;;; memory garbage from handling them.
  (set! (sdl2:event-state 'text-editing) #f)
  (set! (sdl2:event-state 'text-input) #f)
  (set! (sdl2:event-state 'mouse-wheel) #f)
  (set! (sdl2:event-state 'finger-down) #f)
  (set! (sdl2:event-state 'finger-up) #f)
  (set! (sdl2:event-state 'finger-motion) #f)
  (set! (sdl2:event-state 'multi-gesture) #f)

  ;;      ;;(tile (screen-point->tile mouse-x mouse-y *board*)))
  ;; ;; Focus on the tile that the mouse is pointing at.
  ;; (when (not *game-over*)
  ;;   (focus-on-tile! tile))))))

  (main-loop)
  (sdl2:quit!)
  )


(define (run)
  (setup)
  (viz))


(run)
