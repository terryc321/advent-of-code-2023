
;;(import (lsp-server))

(import scheme)
(import (chicken process-context))
(import (chicken io))
(import (chicken format))
(import (chicken sort))
(import (chicken string))
(import (chicken pretty-print))
(import (chicken random))
(import (chicken time))
;;(import srfi-89)


;;(import srfi-89)
;; missing srfi-89 compatibility no egg for it ??

;;(define pp pretty-print)

;;(import (chicken doc))
;; documentation

;; debugging macro expander
;; debugger

(import procedural-macros)
(import regex)


(import simple-md5)

(import srfi-13)
;;srfi-13 for string=

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

(import simple-loops)
;; do-list

(import vector-lib)
;; vector-copy

#|
(current-directory)
(change-directory "day24")
|#

(define (1- x) (- x 1))
(define (1+ x) (+ x 1))

(define *input* (call-with-input-file "input.scm" (lambda (port) (read port))))
(define input *input*)


#|

px py pz vx vy vz  = our thrown particle

my particle will be at time t
px + vx * t , py + vy * t , pz + vz * t
t increments in ones
until all 300 odd hailstones have been collided
collisions do not have to occur every time step

input contains 300 particles 

px1 py1 pz1 vx1 vy1 vz1
px2 py2 pz2 vx2 vy2 vz2
px3 py3 pz3 vx3 vy3 vz3  

collisions occur after time t=0
first collision on t = 1

dependign on where px,py,pz start initial collision may be at time t or some other time ??maybe?? maybe not??
is there multiple convergences ??for a particular set data??
be awkward as question looking for one single answer

suppose first collision is with one of the hailstones in input at time t unknown
initial position of thrown hailstone is also unknown , probably found out after solution found

collided with first hailstone somewhere , next time step

idea of colliding , becoming closer , so if all hailstones are getting closer then thats ok ,
if any are escaping then
if any are parallel then not a solution , since cannot collide with parallel unless

take any two particles see if can find at what time they collide ,

whole integer solution for positions so thats useful too

------------------------------------------------------------------------------------------------------

after time t hits some particle

px1 py1 pz1 vx1 vy1 vz1

IF at time t1 = 1 impact with particle 1 
px1' = px1 + vx1 * t1 = px' = px + vx * t1
py1' = py1 + vy1 * t1 = py' = py + vy * t1
pz1' = pz1 + vz1 * t1 = pz' = pz + vz * t1

IF at time t2 = 2 impact with particle2
;; not quite right as px' is position impact at time t = 1 nanosecond
;; we abuse notation let px' here be postion at time t = 2 nanosecond
px2' = px2 + vx2 * t2 = px' = px + vx * t2
py2' = py2 + vy2 * t2 = py' = py + vy * t2
pz2' = pz2 + vz2 * t2 = pz' = pz + vz * t2

then
vx = px2' - px1'
vy = py2' - py1'
vz = pz2' - pz1'

since now know velocities can go back to first equation
px = px1' - vx
py = py1' - vy
pz = pz1' - vz

provided the impact occurs at time t = 1 nanosecond

----------------------------------------------------------------------------------------------------------


|#



	


