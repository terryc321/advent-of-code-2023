
#|
;;try write an unhygienic macro 

(define-er-macro (macro-foo-1)
  %
  `(set! p (lambda () 3)))

(define-er-macro (macro-foo-2)
  %
  `(,%set! p (,%lambda () 3)))


(define-er-macro (macro-foo-1)
  %
  `(set! p (lambda (z) (+ z z))))

(let ((p 5))
  (macro-foo-1)
  (p 3))

(let ((p 5))
  (macro-foo-2)
  (p 'dont-care))

#|
chicken 5 explicit renaming macro
is there restrictions on what can be in macro definition ?

|#

(define-er-macro (macro-foo-1)
  %
  `(set! p (lambda (z) (+ z z))))


(define-er-macro (macro-foo-2)
  %
  `(set! p (lambda () 3)))


|#


;; 
;; (foo) => (set! p (lambda () 3))
(define-syntax foo
  (er-macro-transformer
   (lambda (exp rename compare)
     `(,(rename 'set!) p (,(rename 'lambda) () 3)))))


(let ((p 5)) ;; let p be number 5
  (foo) ;; this should set p to be (lambda () 3)
  (p)) ;; call p should return 3

;;(define-er-macro foo2


(define-er-macro (foo2)
  %
  `(,%set! p (,%lambda (z) 3)))

(pp (expand '(foo2)))

(define-er-macro (foo3)
  %
  `(,%set! p (,%lambda () 7)))


(define-er-macro (foo2)
  %
  `(,%set! p (,%lambda (z) 3)))




(define-syntax foo-1
  (er-macro-transformer
   (lambda (exp rename compare)
     (let ((vars (map car (cadr exp)))
           (inits (map cadr (cadr exp)))
           (body (cddr exp)))
       `((,(rename 'lambda) ,vars ,@body) ;; lambda renamed avoid capture
	 ,@inits)))))


#|
;; --------- mit-scheme  Release 12.1 || SF || LIAR/x86-64

|#
(define-syntax foo-2
  (er-macro-transformer
   (lambda (exp rename compare)
     `(,(rename 'set!) p (,(rename 'lambda) () 5)))))

(let ((p 3))
  (foo-2)
  (p))
