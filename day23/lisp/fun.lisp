
(defpackage :aoc
  (:use :cl))

(in-package :aoc)

;;(declaim (optimize (speed 3)(debug 0)(space 0)(safety 1)(compilation-speed 0)))


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

|#


(defun walk (g)
  (let ((g (copy g)))
  (let* ((width (width g))(height (height g))(target-x (- width 1))(target-y height)(solutions nil))
    (labels ((offp (x y) ;;off board ?
               (or (< x 1) (< y 1) (> x width) (> y height)))
             (rec (g x y step)               
               ;; show board 
               
               ;; (xy! g x y #\X) ;; mark X where we are 
               ;; (format t "at ~a ~a -> ~a ~%" x y (xy g x y))
               ;; (draw g)               
               ;; (format t "~%")

               (labels 
                   (
                    ;; (visited (x y) (char= (gethash (list x y) g) #\O )) 
                    ;; (not-visited (x y) (not (visited x y)))
                    ;; may not be a character at x y !!
                    (clear (x y) (let ((ch (gethash (list x y) g)))
                                   (cond
                                     ((not (characterp ch)) nil)
                                     (t (or (char= ch #\.) (char= ch #\X) 
                                            (char= ch #\v) (char= ch #\^) 
                                            (char= ch #\<) (char= ch #\>) )))))
                    (target-reached (x y) (and (= x target-x) (= y target-y))))
               (cond
                 ((target-reached x y) 
                  ;; leave remarks till after found all solutions
                  ;;(format t "target reached in ~a steps ~%" step)
                  (setq solutions (cons (list step (copy g)) solutions))
                  ;; (draw g)               
                  ;; (format t "~%")
                  ) 
                 ;; off board ?
                 ((offp x y) nil)
                 ;; is it a forest # 
                 ;; cannot walk through forest
                 (t (let ((ch (xy g x y)))
                      (cond
                        ((char= ch #\#) nil) 
                        ((and (char= ch #\>) (clear (+ x 1) y))
                         ;;(xy! g (+ x 1) y #\O)
                         (rec g (+ x 1) y (+ step 1))
                         ;;(xy! g (+ x 1) y #\.) ; undo
                         ) 
                        ((and (char= ch #\<) (clear (- x 1) y))
                         ;;(xy! g (- x 1) y #\O)                         
                         (rec g (- x 1) y (+ step 1))
                         ;;(xy! g (- x 1) y #\.) ;undo                          
                         )
                        ((and (char= ch #\^) (clear x (- y 1)))
                         ;;(xy! g x (- y 1) #\O)                         
                         (rec g x (- y 1) (+ step 1))
                         ;;(xy! g x (- y 1) #\.) ; undo                         
                         )
                        ((and (char= ch #\v) (clear x (+ y 1)))
                         ;;(xy! g x (+ y 1) #\O)                                                  
                         (rec g x (+ y 1) (+ step 1))
                         ;;(xy! g x (+ y 1) #\.) ; undo                         
                         )
                        ((or (char= ch #\.) (char= ch #\X)) 
                         (when (clear (+ x 1) y) 
                           (xy! g x y #\O)
                           (rec g (+ x 1) y (+ step 1))
                           (xy! g x y #\.))
                           
                         (when (clear (- x 1) y) 
                           (xy! g x y #\O)
                           (rec g (- x 1) y (+ step 1))
                           (xy! g x y #\.))
                           
                         (when (clear x (+ y 1)) 
                           (xy! g x y #\O)
                           (rec g x (+ y 1) (+ step 1))
                           (xy! g x y #\.))

                         (when (clear x (- y 1))
                           (xy! g x y #\O)
                           (rec g x (- y 1) (+ step 1))
                           (xy! g x y #\.))))))))))
                         
      ;;(xy! g 2 1 #\O)
      (let ((initial-x 2)
            (initial-y 1)
            (initial-step 0))
        (rec g initial-x
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

solution took 94 steps 
#O#####################
#OOOOOOO#########...###
#######O#########.#.###
###OOOOO#O>O>.###.#.###
###v#####O#v#.###.#.###
###O>OOO#O#O#.....#...#
###v###O#O#O#########.#
###...#O#O#OOOOOOO#...#
#####.#O#O#######O#.###
#.....#O#O#OOOOOOO#...#
#.#####O#O#O#########v#
#.#...#OOO#OOO###OOO>O#
#.#.#v#######v###O###v#
#...#.>.#...>O>O#O###O#
#####v#.#.###v#O#O###O#
#.....#...#...#O#O#OOO#
#.#########.###O#O#O###
#...###...#...#OOO#O###
###.###.#.###v#####v###
#...#...#.#.>.>.#.>O###
#.###.###.#.###.#.#v###
#.....###...###...#OOO#
#####################.#

recap 
(74 82 82 86 90 94)

> (walk g2)

recap 
(1310 1322 1326 1354 1358 1362 1366 1370 1370 1374 1374 1378 1418 1426 1434
 1438 1442 1470 1478 1482 1486 1486 1490 1490 1494 1502 1506 1514 1518 1518
 1522 1526 1530 1530 1530 1530 1534 1534 1538 1538 1542 1542 1542 1546 1546
 1546 1546 1550 1550 1554 1554 1558 1558 1558 1562 1562 1566 1566 1570 1570
 1574 1574 1578 1578 1578 1578 1582 1582 1582 1586 1586 1590 1590 1590 1590
 1594 1594 1594 1598 1598 1602 1602 1602 1606 1606 1610 1614 1618 1618 1622
 1626 1626 1630 1634 1634 1638 1638 1642 1642 1646 1646 1650 1650 1654 1658
 1658 1662 1662 1662 1662 1666 1666 1670 1670 1674 1674 1674 1674 1678 1678
 1678 1682 1686 1686 1686 1690 1690 1694 1694 1698 1698 1698 1702 1702 1702
 1706 1706 1710 1710 1710 1710 1714 1714 1714 1714 1714 1714 1718 1718 1718
 1722 1722 1722 1726 1726 1726 1726 1726 1730 1730 1730 1734 1738 1738 1742
 1746 1746 1750 1750 1750 1750 1754 1754 1758 1758 1762 1762 1762 1762 1766
 1766 1766 1766 1766 1770 1770 1770 1774 1774 1774 1778 1778 1778 1782 1782
 1782 1786 1790 1790 1794 1794 1794 1794 1798 1798 1806 1810 1810 1822 1822
 1826 1830 1834 1834 1838 1838 1838 1842 1846 1846 1854 1858 1862 1874 1882
 1886 1886 1890 1898 1902 1902 1906 1914 1922 1922 1934 1938 1942 1942 1946
 1954 1966 1990 1998 2006 2034 2054 2062 2082 2130 2170 2174)
AOC> 


longest walk suggested is 2174 



|#















         
        





