

#|

in this version we can walk on slopes 
  < > ^ v 
so to indicate we have already been here 
we changed to a letter 
  l = left <
  r = right >
  u = up ^
  d = down 

when we walk over the letter , we capitalise the letter 
so we then do not walk over our path again and again

use letters because symbols < > ^ do not have an uppercase version !

  left l becomes L 
  right r becomes R 
  up u becomes U 
  down d becomes D 


|#





(defpackage :aoc
  (:use :cl))

(in-package :aoc)

(declaim (optimize (speed 0)(debug 0)(space 3)(safety 1)(compilation-speed 0)))


(defun read-grid (filename) 
  (with-open-file (in filename :direction :input)
    (format t "opened file !~%")
    (let ((width 0)
          (height 0)
          (x 1)
          (y 1)
          (hash (make-hash-table :test #'equalp)))
      (tagbody 
          start 
          (let ;;((ch (read-char in :eof-error-p nil)))
              ((ch (read-char in nil)))
            (cond
              ((characterp ch) 
               (cond ; newline ascii 10
                 ((char= ch #\newline) (setq x 1)(setq y (+ y 1)) 
                  (when (> x width) (setq width x))
                  (when (> y height) (setq height y))
                  (go start))
                 (t ; add char to grid at x y , increment x 
                  (when (char= ch #\>) (setq ch #\r))
                  (when (char= ch #\<) (setq ch #\l))
                  (when (char= ch #\^) (setq ch #\u))
                  (when (char= ch #\v) (setq ch #\d))

                  (setf (gethash (list x y) hash) ch)
                  (format t "setting ~a ~a -> ~a : ~a ~%" x y ch (char-code ch)) 
                  (when (> x width) (setq width x))
                  (when (> y height) (setq height y))
                  (setq x (+ x 1))
                  (go start))))
              (t ; end of file 
                (go end))))
       end)
      ;; adjust height ??
      (setq height (- height 1))
      (setf (gethash 'width hash) width)
      (setf (gethash 'height hash) height)
      hash)))


(defparameter g (read-grid "../problem-spec/example"))
;; copy grid 

(defun copy (h)
  (let ((hash (make-hash-table :test #'equalp)))
    (labels ((copy-hash-entry (key value)
               ;;(format t "key ~a -> value ~a ~%" key value)
               (setf (gethash key hash) value)))
      (maphash #'copy-hash-entry g)
      hash)))

(defparameter h (copy g))

(defun xy (g x y)
  (gethash (list x y) g))

(defun xy! (g x y v)
  ;; (setf (xy g x y) v)
  (setf (gethash (list x y) g) v)
)

(defun width (g) (gethash 'width g))
(defun height (g) (gethash 'height g))
(defun size (g) (list (width g)( height g)))

(defparameter g2 (read-grid "../problem-spec/input"))

;; check copies working 
;; (xy! g 1 1 'hello)
;; (xy! h 1 1 'goodbye)
;; 
;; (xy g 1 1)
;; (xy h 1 1)

(defun draw (g) 
  (let ((width (width g))(height (height g)))
    (loop for y from 1 to height do
      (when (> y 1) (format t "~%"))
      (loop for x from 1 to width do
        (format t "~a" (xy g x y))))
    (format t "~%")))


(draw h)
;;(draw g)
;;(draw g2)

#|
got a grid in memory , really a hash (list x y) as key  returns char 
puzzle input start at 2 , 1 in both example and input problems 
where at in terms x y ... and where been ... 
if we get to (width-1,height) we have reached end of maze in these puzzles
leave a breadcrumb trail on the grid itself , 
 when we undo - restore ?

lets try without copying entire board ...

;;off board ? offp
|#


(defun walk (g)
  (let ((g (copy g)))
  (let* ((width (width g))
         (height (height g))
         (target-x (- width 1))
         (target-y height)
         (longest 0)
         (solutions nil))
    (labels ((offp (x y) (or (< x 1) (< y 1) (> x width) (> y height)))
             (rec (x y step)               
               

               (labels 
                   ((clear (x y) (let ((ch (gethash (list x y) g)))
                                   (cond
                                     ((not (characterp ch)) nil)
                                     (t (or (char= ch #\.) (char= ch #\X) 
                                            (char= ch #\d) (char= ch #\u) 
                                            (char= ch #\l) (char= ch #\r) )))))
                    (target-reached (x y) (and (= x target-x) (= y target-y))))
               (cond
                 ((target-reached x y) 
                  (when (> step longest) 
                    (setq longest step))
                  (format t "target reached after ~a steps .... longest ~a ~%" step longest)
                  (setq solutions (cons (list step (copy g)) solutions)))
                 ;; off board ?
                 ((offp x y) nil)
                 ;; is it a forest # 
                 ;; cannot walk through forest
                 (t (let ((ch (xy g x y)))
                      (cond
                        ((char= ch #\#) nil) 
                        (t
                         (let ((ch2 ch))                           
                           (when (clear (+ x 1) y) 
                             (xy! g x y #\O)
                             (rec (+ x 1) y (+ step 1))
                             (xy! g x y ch2))
                           
                           (when (clear (- x 1) y) 
                             (xy! g x y #\O)
                             (rec (- x 1) y (+ step 1))
                             (xy! g x y ch2))
                           
                           (when (clear x (+ y 1)) 
                             (xy! g x y #\O)
                             (rec x (+ y 1) (+ step 1))
                             (xy! g x y ch2))

                           (when (clear x (- y 1))
                             (xy! g x y #\O)
                             (rec x (- y 1) (+ step 1))
                             (xy! g x y ch2))
                           ))))))))) ;; labels ... offp
                         
      ;;(xy! g 2 1 #\O)
      (let ((initial-x 2)
            (initial-y 1)
            (initial-step 0))
        (rec initial-x
             initial-y
             initial-step)
        (let ((results (sort solutions (lambda (x y)
                                         (let ((step-x (car x))
                                               (step-y (car y)))
                                           (< step-x step-y))))))
          (dolist (result results)
            (let ((step (car result))
                  (grid (cadr result)))
              (format t "solution took ~a steps ~%" step)
              (draw grid)
              (format t "~%~%")))
          (format t "~%")
          (format t "recap ~%")
          (mapcar (lambda (result) (car result)) results)))))))





#|

> (walk h) 

solution took 154 steps 
#O#####################
#OOOOOOO#########OOO###
#######O#########O#O###
###OOOOO#.rOOO###O#O###
###O#####.#O#O###O#O###
###Or...#.#O#OOOOO#OOO#
###O###.#.#O#########O#
###OOO#.#.#OOOOOOO#OOO#
#####O#.#.#######O#O###
#OOOOO#.#.#OOOOOOO#OOO#
#O#####.#.#O#########O#
#O#OOO#...#OOO###...rO#
#O#O#O#######O###.###O#
#OOO#Or.#...rOr.#.###O#
#####O#.#.###O#.#.###O#
#OOOOO#...#OOO#.#.#OOO#
#O#########O###.#.#O###
#OOO###OOO#OOO#...#O###
###O###O#O###O#####O###
#OOO#OOO#O#OOOr.#.rO###
#O###O###O#O###.#.#O###
#OOOOO###OOO###...#OOO#
#####################.#

recap 
(74 82 82 86 90 94 110 118 118 126 150 154)

> (walk g2)

... time passes ...


|#















         
        





