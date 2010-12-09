; This code is in the public domain.
; Based on the Haskell original at
; http://www.kimbly.com/code/invidx/haskell/InvIdx.lhs

(defun list-to-idx (a-list)
  (loop for key in (reduce #'adjoin (loop for (key2 vals) in a-list
                                          unless (null vals)
                                          collect key2)
                           :from-end t :initial-value '())
        collect (list key (reduce #'union
                                  (loop for (key2 vals) in a-list
                                        if (eql key key2)
                                        collect (remove-duplicates vals))))))

(defun idx-invert (idx)
  (loop for val in (reduce #'union (mapcar #'second idx) :initial-value '())
        collect (list val (loop for (key vals) in idx
                                if (member val vals)
                                collect key))))

(defun idx-lookup (idx key)
  (second (assoc key idx)))

(defun idx-lookup-all (idx keys)
  (and keys (reduce #'intersection (lookup-each idx keys))))

(defun idx-lookup-any (idx keys)
  (and keys (reduce #'union (lookup-each idx keys))))

(defun lookup-each (idx keys)
  (loop for key in keys
        collect (idx-lookup idx key)))


(use-package :cl-quickcheck)

(defun idx-equal (idx1 idx2)
  (equal (canon idx1) (canon idx2)))

(defun canon (idx)
  (sort (loop for (key vals) in idx
              collect (list key (sort vals #'<)))
        #'<
        :key #'first))

(defmacro cute (fn &rest args)
  "This is roughly like currying in Haskell.  Example:
 (cute + <> 1) --> (let ((t1 1)) (lambda (<>) (+ <> t1)))
And CUTE really is the name they give this in the Scheme SRFI... ugh."
  (let ((vars (loop for a in args
		    collect (if (eq '<> a) '<> (gensym)))))
    `(let ,(loop for a in args
		 for v in vars
		 unless (eq '<> a)
		 collect (list v a))
       (lambda (<>) 
	 (,fn ,@vars)))))

(define (an-idx)
  (list-to-idx (generate (a-list (a-tuple an-integer 
                                          (a-list an-integer))))))

(when *testing*
  (for-all ((idx an-idx))
    (is idx-equal idx (idx-invert (idx-invert idx))))
  (for-all ((idx an-idx) 
	    (keys (a-list an-integer)))
    (test (every (cute subsetp (idx-lookup-all idx keys) <>)
		 (lookup-each idx keys)))
    (test (every (cute subsetp <> (idx-lookup-any idx keys))
		 (lookup-each idx keys)))))
