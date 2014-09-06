; Copyright 2010 Andrew Pennebaker under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html
;
; Copyright 2004 Darius Bacon
;
; A utility for checking correctness of macroexpansions, since they
; tend to use generated variable names.

(defun alpha= (x y)
  "Return true iff X and Y are equal up to renaming of bound variables.
Very crude/incomplete implementation, needs a real code-walker."
  (equal (normalize 0 nil x) 
	 (normalize 0 nil y)))

(defun normalize (n env x)
  "Return expression X in canonical form for comparison.  Bound
variables are renamed as a function of the place in X where they get
bound."
  (cond ((null x) '())
	((atom x) (lookup env x))
	((letp x)
	 (let ((vars (mapcar #'first (second x)))
	       (vals (mapcar #'second (second x)))
	       (body (cddr x)))
	   (let* ((new-vars (rename n vars))
		  (new-n (+ n (length vars)))
		  (new-env (bind vars new-vars env)))
	     `(let ,(mapcar #'list 
			    new-vars
			    (normalize-each n env vals))
		,@(normalize-each new-n new-env body)))))
	(t (cons (first x)
		 (normalize-each n env (rest x))))))

(defun normalize-each (n env xs)
  (mapcar (lambda (x) (normalize n env x)) xs))

(defpackage :alpha)

(defun rename (n vars)
  (loop for v in vars
	for i from n
	collect (concat-symbol :alpha "g" i)))

(defun lookup (env x)
  (or (and (symbolp x) (cdr (assoc x env :test #'eq)))
      x))

(defun bind (vars vals env)
  (append (mapcar #'cons vars vals) env))

(defun letp (x)
  (and (eq (first x) 'let)
       (<= 3 (length x))
       (listp (second x))
       (every #'bindingp (second x))))

(defun bindingp (x)
  (and (consp x)
       (symbolp (car x))
       (= 2 (length x))))

(defun concat-symbol (package &rest parts)
  "Intern a symbol by catenating PARTS."
  (intern (format nil "~{~a~}" parts) package))


(use-package :cl-quickcheck)

(when *testing*
  (is alpha= '(x y z) '(x y z))
  (isnt alpha= '(x y z) '(y x z))
  (is alpha= '(x (let ((x 5)) x)) '(x (let ((y 5)) y)))
  (is alpha= 
      '(let ((x 0)) (let ((y 1)) (f y)))
      '(let ((x 0)) (let ((x 1)) (f x))))
  (isnt alpha= 
	'(let ((x 0)) (let ((y 1)) (f x)))
	'(let ((x 0)) (let ((x 1)) (f x)))))
