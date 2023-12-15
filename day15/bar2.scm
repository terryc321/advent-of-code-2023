#|

BAR2.SCM


somewhere in the code uses symbols like qt pc ot

list containing ((ot 9)(ab 5)(pc 6))

BUG :
symbol is not EQ with itself

data structure was using strings that looked like symbols but were not , simply strings

list actually contained (("ot" 9)("ab" 5)("pc" 6))
but presented as ((ot 9)(ab 5)(pc 6))



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

#|

Determine the ASCII code for the current character of the string.
Increase the current value by the ASCII code you just determined.
Set the current value to itself multiplied by 17.
Set the current value to the remainder of dividing itself by 256.

|#

(define (chash s)
  (let ((slen (string-length s))
	(hash 0))
    (do-for i (0 slen 1)
	    (let ((asc (char->integer (string-ref s i))))
	      (set! hash (modulo (* 17 (+ hash asc)) 256))
	      ;;(format #t "s[~a] = ~a : ascii ~a : hash ~a~%" i (string-ref s i) asc hash)
	      )
	    )
    hash))

(define (part-1)
  (apply + (map chash (string-split input ","))))


(define (chop s)
  (let ((slen (string-length s)))
    (call/cc (lambda (escape)
	       (do-for i (0 slen 1)
		       (let ((ch (string-ref s i)))
			 (cond
			  ((char=? ch #\=) (escape (list '=
							 (string->symbol (substring s 0 i))
							 (chash (substring s 0 i))
							 (string->number
							  (substring s (+ i 1) slen)))))
			  ((char=? ch #\-) (escape (list '-
							 (string->symbol (substring s 0 i))
							 (chash (substring s 0 i))
							 ))))))))))

	      
(define example "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")

(define (replace-or-append xs lab key)
  (cond
   ((null? xs) (list (list lab key)))
   ((eq? (car (car xs)) lab) (cons (list lab key) (cdr xs))) ;; replace EQUAL? with EQ?
   (#t (cons (car xs) (replace-or-append (cdr xs) lab key)))))

(replace-or-append '((a 1)(b 2)(c 3)) 'a 4)
(replace-or-append '((a 1)(b 2)(c 3)) 'b 5)
(replace-or-append '((a 1)(b 2)(c 3)) 'c 6)
(replace-or-append '((a 1)(b 2)(c 3)) 'd 7)


#|
BUG :
symbol is not EQ with itself

data structure was using strings that looked like symbols but were not , simply strings

|#

(define (remove-lab2 xs lab)
  (cond
   ((null? xs) xs)
   ((eq? (car (car xs)) lab) (cdr xs))  ;; <<<<<<< replace equal with eq?  fails .....
   (#t (cons (car xs) (remove-lab2 (cdr xs) lab)))))

(define (remove-lab xs lab)
  ;;(format #t "removing label ~a from ~a ~%" lab xs)
  (let ((res (remove-lab2 xs lab)))
    res))



(remove-lab '((a 1)(b 2)(c 3)) 'a)
(remove-lab '((a 1)(b 2)(c 3)) 'b)
(remove-lab '((a 1)(b 2)(c 3)) 'c)
(remove-lab '((a 1)(b 2)(c 3)) 'd)

(define (show-boxes v)
  (format #t "~%<boxes>~%")
  (let ((vlen (vector-length v)))
    (do-for i (0 vlen 1)
	    (let ((vref (vector-ref v i)))
	      (cond
	       ((null? vref) #f)
	       (#t (format #t "~a : ~a ~%" i vref)))))
    (format #t "~%")))

  

(define (chop-xs s)
  (define boxes (make-vector 256 '()))
  (do-list (inst (map chop (string-split s ",")))
	   (match inst
	     (('= lab box key) (vector-set! boxes box (replace-or-append (vector-ref boxes box) lab key)))
	     (('- lab box)     (vector-set! boxes box (remove-lab (vector-ref boxes box) lab)))
	     (_ (error "chop-xs no match.")))
	   ;;(format #t "after ~a : ~%" inst)
	   ;;(show-boxes boxes)
	   )
  (show-boxes boxes)
  (focusing-power boxes))


(define (focusing-power v)
  (let ((vlen (vector-length v))
	(tot 0))
    (do-for box (0 vlen 1)
	    (let ((vref (vector-ref v box)))
	      (let ((slen (length vref)))
		(do-for slot (0 slen 1)
			(let ((slot-no (+ 1 slot))
			      (lens (second (list-ref vref slot))))
			  (let ((prod (* (+ 1 box)
					 slot-no
					 lens)))
			    (set! tot (+ tot prod))
			    (format #t "box ~a : slot-no ~a : lens ~a : prod ~a ~%" box slot-no lens prod)))))))
    tot))




(define (example-2)
  (chop-xs example))


(define (part-2)
  (chop-xs input))

#|

<boxes>
0 : ((rn 9)) 
1 : ((vzmd 5) (qp 3)) 
4 : ((qhnm 8)) 
6 : ((lz 4) (cvkb 2)) 
8 : ((smx 6) (frhh 6)) 
10 : ((pcg 6)) 
11 : ((bfhkp 4) (hqr 7) (xc 9)) 
12 : ((vv 6)) 
13 : ((kjh 3) (gf 6)) 
14 : ((hthj 3)) 
15 : ((rxgqm 6)) 
16 : ((sm 8)) 
17 : ((cn 2)) 
19 : ((qb 3) (glp 1) (jfs 4)) 
21 : ((phm 9) (zdg 2) (nrsb 4) (dpq 3)) 
23 : ((mj 2)) 
25 : ((gvl 8)) 
26 : ((nnfx 9) (bph 6)) 
28 : ((xd 4) (zxnfv 9) (skpfx 9) (pqk 1)) 
29 : ((cnff 5)) 
30 : ((vh 1)) 
31 : ((vcf 3) (rzc 4)) 
33 : ((cvx 8) (pmd 7) (fptg 8) (dm 5)) 
34 : ((rp 9)) 
35 : ((mbmsqc 3)) 
36 : ((mmrh 3) (pt 3)) 
37 : ((txfc 9) (nmrllp 1)) 
38 : ((lpj 5)) 
39 : ((xpkrr 1) (nkpn 3)) 
40 : ((mk 8)) 
41 : ((lm 1) (pfc 8)) 
43 : ((hsnr 1) (fhm 9) (smk 2)) 
44 : ((pzrbn 4) (slm 3)) 
49 : ((xmdx 2) (gpj 2)) 
50 : ((lxqm 3) (bjf 8)) 
53 : ((dcck 8)) 
56 : ((nj 4) (zck 7) (bxjt 8)) 
57 : ((ml 9)) 
59 : ((tvknh 3)) 
60 : ((jr 2) (knc 9)) 
61 : ((rnm 2)) 
62 : ((xf 3) (hv 8) (fvb 4)) 
64 : ((thd 7)) 
65 : ((fk 9)) 
67 : ((mdb 4) (sp 8)) 
71 : ((vmd 1)) 
73 : ((rct 9)) 
74 : ((mm 9)) 
75 : ((mks 3)) 
76 : ((kq 4)) 
77 : ((js 1) (gznpjd 5) (kb 8)) 
78 : ((spch 3) (rpff 6) (ptgkh 4)) 
79 : ((xg 2)) 
80 : ((ntn 6) (ljbx 7) (tdjsk 1) (vz 9)) 
82 : ((hdhfph 8)) 
86 : ((cc 7)) 
88 : ((hxx 3) (tgsfpd 7)) 
89 : ((rst 5)) 
90 : ((vhfqbc 3) (nl 9)) 
91 : ((bcv 5) (bdcr 4)) 
92 : ((hzcg 9)) 
94 : ((kc 3) (zd 7)) 
95 : ((xfhbct 7)) 
96 : ((xh 5)) 
98 : ((vl 1)) 
99 : ((shx 9)) 
101 : ((dq 3)) 
103 : ((cd 6)) 
104 : ((fhbx 8)) 
105 : ((qvr 8) (nxhk 2) (qgq 8) (ssrq 2)) 
106 : ((zntrht 7)) 
108 : ((hgm 4)) 
110 : ((zt 7) (ks 5) (jbb 9)) 
111 : ((kd 1)) 
112 : ((jf 3) (xx 7)) 
115 : ((gl 7) (ppc 6)) 
116 : ((rjx 4) (fn 3)) 
117 : ((pxvg 7) (ncd 7) (tq 4)) 
119 : ((ct 8) (kvf 9) (tbfk 9) (dtbm 8) (sd 8)) 
120 : ((bv 1)) 
121 : ((fpqr 7)) 
123 : ((qft 3) (pxc 7)) 
125 : ((mp 8)) 
126 : ((ddzdhp 6) (lr 1)) 
127 : ((lc 8)) 
128 : ((jv 7) (zf 5)) 
129 : ((hsf 7)) 
132 : ((lqct 7) (vn 6) (grk 4)) 
135 : ((st 8) (tc 9) (ds 8)) 
136 : ((dd 2)) 
137 : ((rg 7) (xrgx 9) (cf 5)) 
138 : ((tjrz 5) (ddb 4) (jvj 6) (lck 4)) 
139 : ((pxs 4)) 
140 : ((gjszks 8) (cfzxq 4)) 
141 : ((gfbjd 5)) 
142 : ((jrr 5)) 
143 : ((tlhst 8)) 
144 : ((ld 8)) 
145 : ((zg 7) (psn 6) (cxss 1)) 
146 : ((xbx 9)) 
147 : ((xk 6)) 
148 : ((hl 6)) 
149 : ((mfr 9)) 
150 : ((rskv 4) (kxvm 6)) 
152 : ((mcx 3) (tbgk 9) (ttp 1)) 
153 : ((bmgc 9)) 
156 : ((pl 4)) 
158 : ((xxhf 3) (mnc 9) (zfvh 6)) 
159 : ((qdz 9) (mr 4)) 
161 : ((zpg 3) (bxbdq 6)) 
162 : ((tks 3) (jph 2)) 
163 : ((tnsn 1)) 
166 : ((zzr 4) (vsm 8)) 
168 : ((bzl 8) (tt 6)) 
169 : ((msxq 5) (sv 6)) 
170 : ((rx 2)) 
171 : ((mqm 8) (nhfrm 8) (czvx 3)) 
172 : ((bj 3) (qk 3)) 
174 : ((qzknxr 7)) 
175 : ((xkbz 4) (hsrr 8)) 
176 : ((tsml 1) (tnn 1) (dggn 7) (zrd 6)) 
180 : ((lzn 7) (jj 1)) 
181 : ((nmhb 3)) 
182 : ((qpgn 3) (hn 4) (ckx 1) (npph 6)) 
183 : ((vq 1) (pkdh 2)) 
186 : ((dv 2) (nhlx 3) (skvv 6)) 
188 : ((zvgtq 2) (bxb 2) (fpf 3)) 
192 : ((mtlqr 3)) 
193 : ((nc 1)) 
195 : ((xsx 1)) 
196 : ((hdx 3) (dmc 3)) 
199 : ((lhdtk 4)) 
200 : ((gq 8)) 
201 : ((fs 1) (vrq 2) (jgpbjl 3)) 
202 : ((vrb 7)) 
203 : ((sx 2) (zjst 7)) 
204 : ((cvs 3)) 
206 : ((qm 6)) 
209 : ((nlg 9)) 
212 : ((vxf 2) (xth 8)) 
213 : ((mvcqks 8) (lfhbbg 4)) 
214 : ((tlhn 8)) 
215 : ((jnqn 7)) 
217 : ((grp 1)) 
218 : ((nrz 7) (gc 2) (hpb 7)) 
219 : ((vcb 1)) 
220 : ((dx 5) (sgr 8) (vpv 6)) 
222 : ((gbzxc 7)) 
223 : ((bm 3) (bckgx 4)) 
225 : ((rfklb 6)) 
226 : ((pntp 6) (qfk 6)) 
227 : ((djqd 8)) 
228 : ((fhnh 1) (mg 4)) 
229 : ((gvx 9) (cmkz 8) (nqxbjr 2) (plgb 6)) 
230 : ((zl 2)) 
231 : ((hsl 5)) 
233 : ((mhd 7)) 
234 : ((vt 9)) 
235 : ((gd 7)) 
244 : ((nf 7)) 
245 : ((mh 1)) 
246 : ((cmlj 2)) 
247 : ((zm 8) (qjfv 9)) 
248 : ((vqdcj 3)) 
250 : ((nff 4)) 
252 : ((fv 3)) 
253 : ((zcsm 6) (fct 7) (tfs 5)) 
254 : ((dgc 7) (xrjj 7) (tj 5)) 
255 : ((kvn 1)) 

box 0 : slot-no 1 : lens 9 : prod 9 
box 1 : slot-no 1 : lens 5 : prod 10 
box 1 : slot-no 2 : lens 3 : prod 12 
box 4 : slot-no 1 : lens 8 : prod 40 
box 6 : slot-no 1 : lens 4 : prod 28 
box 6 : slot-no 2 : lens 2 : prod 28 
box 8 : slot-no 1 : lens 6 : prod 54 
box 8 : slot-no 2 : lens 6 : prod 108 
box 10 : slot-no 1 : lens 6 : prod 66 
box 11 : slot-no 1 : lens 4 : prod 48 
box 11 : slot-no 2 : lens 7 : prod 168 
box 11 : slot-no 3 : lens 9 : prod 324 
box 12 : slot-no 1 : lens 6 : prod 78 
box 13 : slot-no 1 : lens 3 : prod 42 
box 13 : slot-no 2 : lens 6 : prod 168 
box 14 : slot-no 1 : lens 3 : prod 45 
box 15 : slot-no 1 : lens 6 : prod 96 
box 16 : slot-no 1 : lens 8 : prod 136 
box 17 : slot-no 1 : lens 2 : prod 36 
box 19 : slot-no 1 : lens 3 : prod 60 
box 19 : slot-no 2 : lens 1 : prod 40 
box 19 : slot-no 3 : lens 4 : prod 240 
box 21 : slot-no 1 : lens 9 : prod 198 
box 21 : slot-no 2 : lens 2 : prod 88 
box 21 : slot-no 3 : lens 4 : prod 264 
box 21 : slot-no 4 : lens 3 : prod 264 
box 23 : slot-no 1 : lens 2 : prod 48 
box 25 : slot-no 1 : lens 8 : prod 208 
box 26 : slot-no 1 : lens 9 : prod 243 
box 26 : slot-no 2 : lens 6 : prod 324 
box 28 : slot-no 1 : lens 4 : prod 116 
box 28 : slot-no 2 : lens 9 : prod 522 
box 28 : slot-no 3 : lens 9 : prod 783 
box 28 : slot-no 4 : lens 1 : prod 116 
box 29 : slot-no 1 : lens 5 : prod 150 
box 30 : slot-no 1 : lens 1 : prod 31 
box 31 : slot-no 1 : lens 3 : prod 96 
box 31 : slot-no 2 : lens 4 : prod 256 
box 33 : slot-no 1 : lens 8 : prod 272 
box 33 : slot-no 2 : lens 7 : prod 476 
box 33 : slot-no 3 : lens 8 : prod 816 
box 33 : slot-no 4 : lens 5 : prod 680 
box 34 : slot-no 1 : lens 9 : prod 315 
box 35 : slot-no 1 : lens 3 : prod 108 
box 36 : slot-no 1 : lens 3 : prod 111 
box 36 : slot-no 2 : lens 3 : prod 222 
box 37 : slot-no 1 : lens 9 : prod 342 
box 37 : slot-no 2 : lens 1 : prod 76 
box 38 : slot-no 1 : lens 5 : prod 195 
box 39 : slot-no 1 : lens 1 : prod 40 
box 39 : slot-no 2 : lens 3 : prod 240 
box 40 : slot-no 1 : lens 8 : prod 328 
box 41 : slot-no 1 : lens 1 : prod 42 
box 41 : slot-no 2 : lens 8 : prod 672 
box 43 : slot-no 1 : lens 1 : prod 44 
box 43 : slot-no 2 : lens 9 : prod 792 
box 43 : slot-no 3 : lens 2 : prod 264 
box 44 : slot-no 1 : lens 4 : prod 180 
box 44 : slot-no 2 : lens 3 : prod 270 
box 49 : slot-no 1 : lens 2 : prod 100 
box 49 : slot-no 2 : lens 2 : prod 200 
box 50 : slot-no 1 : lens 3 : prod 153 
box 50 : slot-no 2 : lens 8 : prod 816 
box 53 : slot-no 1 : lens 8 : prod 432 
box 56 : slot-no 1 : lens 4 : prod 228 
box 56 : slot-no 2 : lens 7 : prod 798 
box 56 : slot-no 3 : lens 8 : prod 1368 
box 57 : slot-no 1 : lens 9 : prod 522 
box 59 : slot-no 1 : lens 3 : prod 180 
box 60 : slot-no 1 : lens 2 : prod 122 
box 60 : slot-no 2 : lens 9 : prod 1098 
box 61 : slot-no 1 : lens 2 : prod 124 
box 62 : slot-no 1 : lens 3 : prod 189 
box 62 : slot-no 2 : lens 8 : prod 1008 
box 62 : slot-no 3 : lens 4 : prod 756 
box 64 : slot-no 1 : lens 7 : prod 455 
box 65 : slot-no 1 : lens 9 : prod 594 
box 67 : slot-no 1 : lens 4 : prod 272 
box 67 : slot-no 2 : lens 8 : prod 1088 
box 71 : slot-no 1 : lens 1 : prod 72 
box 73 : slot-no 1 : lens 9 : prod 666 
box 74 : slot-no 1 : lens 9 : prod 675 
box 75 : slot-no 1 : lens 3 : prod 228 
box 76 : slot-no 1 : lens 4 : prod 308 
box 77 : slot-no 1 : lens 1 : prod 78 
box 77 : slot-no 2 : lens 5 : prod 780 
box 77 : slot-no 3 : lens 8 : prod 1872 
box 78 : slot-no 1 : lens 3 : prod 237 
box 78 : slot-no 2 : lens 6 : prod 948 
box 78 : slot-no 3 : lens 4 : prod 948 
box 79 : slot-no 1 : lens 2 : prod 160 
box 80 : slot-no 1 : lens 6 : prod 486 
box 80 : slot-no 2 : lens 7 : prod 1134 
box 80 : slot-no 3 : lens 1 : prod 243 
box 80 : slot-no 4 : lens 9 : prod 2916 
box 82 : slot-no 1 : lens 8 : prod 664 
box 86 : slot-no 1 : lens 7 : prod 609 
box 88 : slot-no 1 : lens 3 : prod 267 
box 88 : slot-no 2 : lens 7 : prod 1246 
box 89 : slot-no 1 : lens 5 : prod 450 
box 90 : slot-no 1 : lens 3 : prod 273 
box 90 : slot-no 2 : lens 9 : prod 1638 
box 91 : slot-no 1 : lens 5 : prod 460 
box 91 : slot-no 2 : lens 4 : prod 736 
box 92 : slot-no 1 : lens 9 : prod 837 
box 94 : slot-no 1 : lens 3 : prod 285 
box 94 : slot-no 2 : lens 7 : prod 1330 
box 95 : slot-no 1 : lens 7 : prod 672 
box 96 : slot-no 1 : lens 5 : prod 485 
box 98 : slot-no 1 : lens 1 : prod 99 
box 99 : slot-no 1 : lens 9 : prod 900 
box 101 : slot-no 1 : lens 3 : prod 306 
box 103 : slot-no 1 : lens 6 : prod 624 
box 104 : slot-no 1 : lens 8 : prod 840 
box 105 : slot-no 1 : lens 8 : prod 848 
box 105 : slot-no 2 : lens 2 : prod 424 
box 105 : slot-no 3 : lens 8 : prod 2544 
box 105 : slot-no 4 : lens 2 : prod 848 
box 106 : slot-no 1 : lens 7 : prod 749 
box 108 : slot-no 1 : lens 4 : prod 436 
box 110 : slot-no 1 : lens 7 : prod 777 
box 110 : slot-no 2 : lens 5 : prod 1110 
box 110 : slot-no 3 : lens 9 : prod 2997 
box 111 : slot-no 1 : lens 1 : prod 112 
box 112 : slot-no 1 : lens 3 : prod 339 
box 112 : slot-no 2 : lens 7 : prod 1582 
box 115 : slot-no 1 : lens 7 : prod 812 
box 115 : slot-no 2 : lens 6 : prod 1392 
box 116 : slot-no 1 : lens 4 : prod 468 
box 116 : slot-no 2 : lens 3 : prod 702 
box 117 : slot-no 1 : lens 7 : prod 826 
box 117 : slot-no 2 : lens 7 : prod 1652 
box 117 : slot-no 3 : lens 4 : prod 1416 
box 119 : slot-no 1 : lens 8 : prod 960 
box 119 : slot-no 2 : lens 9 : prod 2160 
box 119 : slot-no 3 : lens 9 : prod 3240 
box 119 : slot-no 4 : lens 8 : prod 3840 
box 119 : slot-no 5 : lens 8 : prod 4800 
box 120 : slot-no 1 : lens 1 : prod 121 
box 121 : slot-no 1 : lens 7 : prod 854 
box 123 : slot-no 1 : lens 3 : prod 372 
box 123 : slot-no 2 : lens 7 : prod 1736 
box 125 : slot-no 1 : lens 8 : prod 1008 
box 126 : slot-no 1 : lens 6 : prod 762 
box 126 : slot-no 2 : lens 1 : prod 254 
box 127 : slot-no 1 : lens 8 : prod 1024 
box 128 : slot-no 1 : lens 7 : prod 903 
box 128 : slot-no 2 : lens 5 : prod 1290 
box 129 : slot-no 1 : lens 7 : prod 910 
box 132 : slot-no 1 : lens 7 : prod 931 
box 132 : slot-no 2 : lens 6 : prod 1596 
box 132 : slot-no 3 : lens 4 : prod 1596 
box 135 : slot-no 1 : lens 8 : prod 1088 
box 135 : slot-no 2 : lens 9 : prod 2448 
box 135 : slot-no 3 : lens 8 : prod 3264 
box 136 : slot-no 1 : lens 2 : prod 274 
box 137 : slot-no 1 : lens 7 : prod 966 
box 137 : slot-no 2 : lens 9 : prod 2484 
box 137 : slot-no 3 : lens 5 : prod 2070 
box 138 : slot-no 1 : lens 5 : prod 695 
box 138 : slot-no 2 : lens 4 : prod 1112 
box 138 : slot-no 3 : lens 6 : prod 2502 
box 138 : slot-no 4 : lens 4 : prod 2224 
box 139 : slot-no 1 : lens 4 : prod 560 
box 140 : slot-no 1 : lens 8 : prod 1128 
box 140 : slot-no 2 : lens 4 : prod 1128 
box 141 : slot-no 1 : lens 5 : prod 710 
box 142 : slot-no 1 : lens 5 : prod 715 
box 143 : slot-no 1 : lens 8 : prod 1152 
box 144 : slot-no 1 : lens 8 : prod 1160 
box 145 : slot-no 1 : lens 7 : prod 1022 
box 145 : slot-no 2 : lens 6 : prod 1752 
box 145 : slot-no 3 : lens 1 : prod 438 
box 146 : slot-no 1 : lens 9 : prod 1323 
box 147 : slot-no 1 : lens 6 : prod 888 
box 148 : slot-no 1 : lens 6 : prod 894 
box 149 : slot-no 1 : lens 9 : prod 1350 
box 150 : slot-no 1 : lens 4 : prod 604 
box 150 : slot-no 2 : lens 6 : prod 1812 
box 152 : slot-no 1 : lens 3 : prod 459 
box 152 : slot-no 2 : lens 9 : prod 2754 
box 152 : slot-no 3 : lens 1 : prod 459 
box 153 : slot-no 1 : lens 9 : prod 1386 
box 156 : slot-no 1 : lens 4 : prod 628 
box 158 : slot-no 1 : lens 3 : prod 477 
box 158 : slot-no 2 : lens 9 : prod 2862 
box 158 : slot-no 3 : lens 6 : prod 2862 
box 159 : slot-no 1 : lens 9 : prod 1440 
box 159 : slot-no 2 : lens 4 : prod 1280 
box 161 : slot-no 1 : lens 3 : prod 486 
box 161 : slot-no 2 : lens 6 : prod 1944 
box 162 : slot-no 1 : lens 3 : prod 489 
box 162 : slot-no 2 : lens 2 : prod 652 
box 163 : slot-no 1 : lens 1 : prod 164 
box 166 : slot-no 1 : lens 4 : prod 668 
box 166 : slot-no 2 : lens 8 : prod 2672 
box 168 : slot-no 1 : lens 8 : prod 1352 
box 168 : slot-no 2 : lens 6 : prod 2028 
box 169 : slot-no 1 : lens 5 : prod 850 
box 169 : slot-no 2 : lens 6 : prod 2040 
box 170 : slot-no 1 : lens 2 : prod 342 
box 171 : slot-no 1 : lens 8 : prod 1376 
box 171 : slot-no 2 : lens 8 : prod 2752 
box 171 : slot-no 3 : lens 3 : prod 1548 
box 172 : slot-no 1 : lens 3 : prod 519 
box 172 : slot-no 2 : lens 3 : prod 1038 
box 174 : slot-no 1 : lens 7 : prod 1225 
box 175 : slot-no 1 : lens 4 : prod 704 
box 175 : slot-no 2 : lens 8 : prod 2816 
box 176 : slot-no 1 : lens 1 : prod 177 
box 176 : slot-no 2 : lens 1 : prod 354 
box 176 : slot-no 3 : lens 7 : prod 3717 
box 176 : slot-no 4 : lens 6 : prod 4248 
box 180 : slot-no 1 : lens 7 : prod 1267 
box 180 : slot-no 2 : lens 1 : prod 362 
box 181 : slot-no 1 : lens 3 : prod 546 
box 182 : slot-no 1 : lens 3 : prod 549 
box 182 : slot-no 2 : lens 4 : prod 1464 
box 182 : slot-no 3 : lens 1 : prod 549 
box 182 : slot-no 4 : lens 6 : prod 4392 
box 183 : slot-no 1 : lens 1 : prod 184 
box 183 : slot-no 2 : lens 2 : prod 736 
box 186 : slot-no 1 : lens 2 : prod 374 
box 186 : slot-no 2 : lens 3 : prod 1122 
box 186 : slot-no 3 : lens 6 : prod 3366 
box 188 : slot-no 1 : lens 2 : prod 378 
box 188 : slot-no 2 : lens 2 : prod 756 
box 188 : slot-no 3 : lens 3 : prod 1701 
box 192 : slot-no 1 : lens 3 : prod 579 
box 193 : slot-no 1 : lens 1 : prod 194 
box 195 : slot-no 1 : lens 1 : prod 196 
box 196 : slot-no 1 : lens 3 : prod 591 
box 196 : slot-no 2 : lens 3 : prod 1182 
box 199 : slot-no 1 : lens 4 : prod 800 
box 200 : slot-no 1 : lens 8 : prod 1608 
box 201 : slot-no 1 : lens 1 : prod 202 
box 201 : slot-no 2 : lens 2 : prod 808 
box 201 : slot-no 3 : lens 3 : prod 1818 
box 202 : slot-no 1 : lens 7 : prod 1421 
box 203 : slot-no 1 : lens 2 : prod 408 
box 203 : slot-no 2 : lens 7 : prod 2856 
box 204 : slot-no 1 : lens 3 : prod 615 
box 206 : slot-no 1 : lens 6 : prod 1242 
box 209 : slot-no 1 : lens 9 : prod 1890 
box 212 : slot-no 1 : lens 2 : prod 426 
box 212 : slot-no 2 : lens 8 : prod 3408 
box 213 : slot-no 1 : lens 8 : prod 1712 
box 213 : slot-no 2 : lens 4 : prod 1712 
box 214 : slot-no 1 : lens 8 : prod 1720 
box 215 : slot-no 1 : lens 7 : prod 1512 
box 217 : slot-no 1 : lens 1 : prod 218 
box 218 : slot-no 1 : lens 7 : prod 1533 
box 218 : slot-no 2 : lens 2 : prod 876 
box 218 : slot-no 3 : lens 7 : prod 4599 
box 219 : slot-no 1 : lens 1 : prod 220 
box 220 : slot-no 1 : lens 5 : prod 1105 
box 220 : slot-no 2 : lens 8 : prod 3536 
box 220 : slot-no 3 : lens 6 : prod 3978 
box 222 : slot-no 1 : lens 7 : prod 1561 
box 223 : slot-no 1 : lens 3 : prod 672 
box 223 : slot-no 2 : lens 4 : prod 1792 
box 225 : slot-no 1 : lens 6 : prod 1356 
box 226 : slot-no 1 : lens 6 : prod 1362 
box 226 : slot-no 2 : lens 6 : prod 2724 
box 227 : slot-no 1 : lens 8 : prod 1824 
box 228 : slot-no 1 : lens 1 : prod 229 
box 228 : slot-no 2 : lens 4 : prod 1832 
box 229 : slot-no 1 : lens 9 : prod 2070 
box 229 : slot-no 2 : lens 8 : prod 3680 
box 229 : slot-no 3 : lens 2 : prod 1380 
box 229 : slot-no 4 : lens 6 : prod 5520 
box 230 : slot-no 1 : lens 2 : prod 462 
box 231 : slot-no 1 : lens 5 : prod 1160 
box 233 : slot-no 1 : lens 7 : prod 1638 
box 234 : slot-no 1 : lens 9 : prod 2115 
box 235 : slot-no 1 : lens 7 : prod 1652 
box 244 : slot-no 1 : lens 7 : prod 1715 
box 245 : slot-no 1 : lens 1 : prod 246 
box 246 : slot-no 1 : lens 2 : prod 494 
box 247 : slot-no 1 : lens 8 : prod 1984 
box 247 : slot-no 2 : lens 9 : prod 4464 
box 248 : slot-no 1 : lens 3 : prod 747 
box 250 : slot-no 1 : lens 4 : prod 1004 
box 252 : slot-no 1 : lens 3 : prod 759 
box 253 : slot-no 1 : lens 6 : prod 1524 
box 253 : slot-no 2 : lens 7 : prod 3556 
box 253 : slot-no 3 : lens 5 : prod 3810 
box 254 : slot-no 1 : lens 7 : prod 1785 
box 254 : slot-no 2 : lens 7 : prod 3570 
box 254 : slot-no 3 : lens 5 : prod 3825 
box 255 : slot-no 1 : lens 1 : prod 256 
303404


|#




;; --------------------------------------------------------------------


(define (debug-chop-xs s)
  (define boxes (make-vector 256 '()))
  (let  ((inst (map chop (string-split s ","))))
    (format #t "inst ~%~a~%" inst)
    (newline)
    (display inst)
    inst))

  



(map (lambda (x) (eq? 'pc (second x)))
     (debug-chop-xs example))

(map (lambda (x) (symbol? (second x)))
     (debug-chop-xs example))

(map (lambda (x) (second x))
     (debug-chop-xs example))



(debug-chop-xs example)



#|


#;1059> (chop-xs example)
after (= rn 0 1) : 

<boxes>
0 : ((rn 1)) 

removing label cm from ((rn 1)) 
after (- cm 0) : 

<boxes>
0 : ((rn 1)) 

after (= qp 1 3) : 

<boxes>
0 : ((rn 1)) 
1 : ((qp 3)) 

after (= cm 0 2) : 

<boxes>
0 : ((rn 1) (cm 2)) 
1 : ((qp 3)) 

removing label qp from ((qp 3)) 
after (- qp 1) : 

<boxes>
0 : ((rn 1) (cm 2)) 

after (= pc 3 4) : 

<boxes>
0 : ((rn 1) (cm 2)) 
3 : ((pc 4)) 

after (= ot 3 9) : 

<boxes>
0 : ((rn 1) (cm 2)) 
3 : ((pc 4) (ot 9)) 

after (= ab 3 5) : 

<boxes>
0 : ((rn 1) (cm 2)) 
3 : ((pc 4) (ot 9) (ab 5)) 

removing label pc from ((pc 4) (ot 9) (ab 5)) 
after (- pc 3) : 

<boxes>
0 : ((rn 1) (cm 2)) 
3 : ((ot 9) (ab 5)) 

after (= pc 3 6) : 

<boxes>
0 : ((rn 1) (cm 2)) 
3 : ((ot 9) (ab 5) (pc 6)) 

after (= ot 3 7) : 

<boxes>
0 : ((rn 1) (cm 2)) 
3 : ((ot 9) (ab 5) (pc 6) (ot 7))    <<<<<<<< using EQ? we have OT 9   

#;1069> (chop-xs example)  -----------------------------------------
after (= rn 0 1) : 

<boxes>
0 : ((rn 1)) 

removing label cm from ((rn 1)) 
after (- cm 0) : 

<boxes>
0 : ((rn 1)) 

after (= qp 1 3) : 

<boxes>
0 : ((rn 1)) 
1 : ((qp 3)) 

after (= cm 0 2) : 

<boxes>
0 : ((rn 1) (cm 2)) 
1 : ((qp 3)) 

removing label qp from ((qp 3)) 
after (- qp 1) : 

<boxes>
0 : ((rn 1) (cm 2)) 

after (= pc 3 4) : 

<boxes>
0 : ((rn 1) (cm 2)) 
3 : ((pc 4)) 

after (= ot 3 9) : 

<boxes>
0 : ((rn 1) (cm 2)) 
3 : ((pc 4) (ot 9)) 

after (= ab 3 5) : 

<boxes>
0 : ((rn 1) (cm 2)) 
3 : ((pc 4) (ot 9) (ab 5)) 

removing label pc from ((pc 4) (ot 9) (ab 5)) 
after (- pc 3) : 

<boxes>
0 : ((rn 1) (cm 2)) 
3 : ((ot 9) (ab 5)) 

after (= pc 3 6) : 

<boxes>
0 : ((rn 1) (cm 2)) 
3 : ((ot 9) (ab 5) (pc 6)) 

after (= ot 3 7) : 

<boxes>
0 : ((rn 1) (cm 2)) 
3 : ((ot 7) (ab 5) (pc 6))   ;; <<<<<<<<<<<<< using EQUAL? have OT 7

#;1106> 
|#

