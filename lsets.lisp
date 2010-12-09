; Copyright 2010 Andrew Pennebaker under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html
;
; Copyright 2004 Darius Bacon
;
; A partial specification of the Lisp set functions.
; This is kind of random... I need to try and write a real formal
; specification.

(use-package :cl-quickcheck)

(defun set-equal (x y)
  (and (subsetp x y) (subsetp y x)))

(when *testing*
  (let ((a-set (a-list an-index)))
    (let ((x-generator a-set)
	  (y-generator a-set)
	  (z-generator a-set))
      (for-all (x)
        (is set-equal x (reverse x))
	(is set-equal x (append x x))
	(is set-equal x (union x '()))
	(is set-equal '() (intersection x '()))
	(is set-equal x (union x x))
	(is set-equal x (intersection x x))
	(isnt set-equal x (adjoin (list x) x)))
      (for-all (x k)
        (is member k (adjoin k x))
	(isnt member k (remove k x))
	(isnt member k '())
	(is set-equal (adjoin k (remove k x)) (adjoin k x))
	(is set-equal (remove k (adjoin k x)) (remove k x))
	(is set-equal (adjoin k x) (union (list k) x))
	(is set-equal (remove k x) (set-difference x (list k))))
      (for-all (x y)
	(is set-equal (union x y) (append x y))
	(is set-equal (union x y) (union y x))
	(is set-equal (intersection x y) (intersection y x))
	(is subsetp (set-difference x y) x)
	(is null (intersection (set-difference x y) y))
	(is subsetp x (union x y))
	(is subsetp y (union x y))
	(is subsetp (intersection x y) x)
	(is subsetp (intersection x y) y))
      (for-all (x y z)		    ; associative and distributive laws
        (is set-equal
	    (union x (union y z))
	    (union (union x y) z))
	(is set-equal 
	    (intersection x (intersection y z))
	    (intersection (intersection x y) z))
	(is set-equal 
	    (union x (intersection y z))
	    (intersection (union x y) (union x z)))
	(is set-equal
	    (intersection x (union y z))
	    (union (intersection x y) (intersection x z)))))))
