; Copyright 2010 Andrew Pennebaker under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html
;
; Copyright 2004 Darius Bacon
;
; Examples from QuickCheck paper (incomplete)

(use-package :cl-quickcheck)

(define x-generator an-index)
(define y-generator an-index)
(define xs-generator (a-list an-index))
(define ys-generator (a-list an-index))

(defun test-reverse ()
  (for-all (x)
    (is= (list x) (reverse (list x))))
  (for-all (xs ys)
    (is= (reverse (append xs ys))
	 (append (reverse ys) (reverse xs))))
  (for-all (xs)
    (is= xs (reverse (reverse xs)))))

(defun test-max ()
  (for-all (x y)
    (only-if (<= x y)
	     (is= (max x y) y))))

(when *testing*
  (test-reverse)
  (test-max))

(defun test-insert ()
  (for-all (x xs)
    (only-if (orderedp xs)
      (classify (null xs) "trivial"
		(test (orderedp (insert x xs))))))
  (for-all (x xs)
    (only-if (orderedp xs)
      (collect (length xs)
	       (test (orderedp (insert x xs)))))))

(defun orderedp (xs)
  (or (null (rest xs))
      (and (<= (first xs) (second xs))
	   (orderedp (rest xs)))))

(defun insert (x xs)
  (cond ((null xs)
	 (list x))
	((< (first xs) x) 
	 (cons (first xs)
	       (insert x (rest xs))))
	(t (cons x xs))))
