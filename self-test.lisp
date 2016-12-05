; Copyright 2010 Andrew Pennebaker under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html
;
; Copyright 2004 Darius Bacon
;
;
; Testing the tester

(in-package :cl-quickcheck)

(eval-when (:execute :compile-toplevel :load-toplevel)
  (defmacro quietly (&body body)
    `(let ((*loud* nil))
       (isolate ,@body)))
  (defmacro isolate (&body body)
    `(singletonize (collect-test-results (lambda () ,@body))))
  (defmacro output-of (&body body)
    `(capture-stdout (let ((*loud* t))
		       ,@body))))

(defun singletonize (tests)
  "Stupid hack for convenience."
  (if (null (cdr tests))
      (car tests)
      tests))

(let ((*num-trials* 20))
  (is= "." (output-of (isolate (test t))))
  (is= "X" (output-of (isolate (test nil))))
  (is= "@" (output-of (isolate (test (/ 1 0)))))
  (is= "@" (output-of (isolate (is= 3 (/ 1 0)))))
  (is= nil (test-flopped (quietly (test t))))
  (is= t   (test-flopped (quietly (test nil))))

  (is typep (test-flopped (quietly (is undefined-function)))
      'error)

  (is= nil (test-flopped
	    (quietly (should-signal 'simple-type-error (string= 1 2)))))
  (test (test-flopped (quietly (should-signal 'simple-error (string= 1 2)))))
  (test (test-flopped (quietly (should-signal 'simple-error t))))

  (is= "
0 tests submitted."
       (output-of (report (isolate))))
  (is= "..
2 tests submitted; all passed."
       (output-of (report (isolate (test (= 0 0))
				   (test (= 1 1))))))
   (is= ".X
FAIL (TEST NIL)
2 tests submitted; 1 FAILED."
	(output-of (report (isolate (test t) (test nil)))))

   (is= "X
FAIL (IS = (+ 2 3) 4)
  with values 5 4
1 test submitted; 1 FAILED."
	(output-of (report (list (isolate (is = (+ 2 3) 4))))))

   (named "*BREAK-ON-FAILURE* set to T disables error interception"
	  (should-signal 'type-error
			 (quietly (let ((*break-on-failure* t))
				    (is string= 1 2)))))
   (named "*BREAK-ON-FAILURE* set to T signals error on test failure"
	  (should-signal 'simple-error
			 (quietly (let ((*break-on-failure* t))
				    (test nil)))))

   (is= 'skipped
	(test-flopped (quietly (let ((*break-on-failure* t))
				 (only-if nil (test t))))))

   (is= "X.
FAIL \"Name\"
  1/2 counterexamples.
1 test submitted; 1 FAILED."
	(output-of (report (isolate (named "Name" (test nil))
				    (named "Name" (test t))))))

   (let ((*num-trials* 3))
     (is= ".X.X.X"
	  (output-of (isolate (for-all (k)
				(is= (* 2 k) (+ k k))
				(is= (* 2 k) (+ k k 1))))))
     (is= "X.X.X.
FAIL (IS= (+ K 1) K)
  with values 1 0
  for ((K 0))
  3/3 counterexamples.
2 tests submitted; 1 FAILED."
	  (output-of (report (isolate
			      (for-all ((k (lambda () 0)))
				(is= (+ k 1) k)
				(is= k k)))))))

   (for-all (k)
     (is= 'a (pick-weighted (20 'a))))
   (locally
       #+sbcl (declare (sb-ext:disable-package-locks cl:random))
   (flet ((random (n) 0))
     (is= 'a (pick-weighted (1 'a) (1 'b))))
   (flet ((random (n) 1))
     (is= 'b (pick-weighted (1 'a) (1 'b))))
   (flet ((random (n) 1))
     (is= 'a (pick-weighted (2 'a) (1 'b))))
   (flet ((random (n) 2))
       (is= 'b (pick-weighted (2 'a) (1 'b)))))

   (for-all ((b a-boolean))
     (is typep b 'boolean))
   (for-all (k)
     (is integerp k)
     (is <= 0 k (1- *size*)))
   (for-all (n)
     (is integerp n)
     (is <= (- *size*) n *size*))
   (for-all ((r a-real))
     (is realp r)
     (is <= (- *size*) r *size*))
   (for-all ((ns (a-list an-integer)))
     (is listp ns)
     (is <= 0 (length ns) *size*)
     (test (every #'integerp ns)))

   (is= 'skipped (test-flopped (quietly (only-if nil (test (/ 1 0))))))
   (is typep     (test-flopped (quietly (only-if t   (test (/ 1 0)))))
       'error)
   (is typep     (test-flopped (quietly (only-if (/ 1 0) (test t))))
       'error)
   (is= "-" (output-of (isolate (only-if nil (test t)))))
   (is= "
SKIP (TEST NIL)
PASS (TEST T)
  1 case checked and passed in 2 attempts.
2 tests submitted; 1 SKIPPED."
	(output-of (report (quietly (only-if nil (test nil))
				    (only-if t   (test t))
				    (only-if nil (test t))))))
   (named
    "If enough trials passed, don't bother reporting skipped cases."
    (is= "
1 test submitted; all passed."
	 (output-of (report (quietly
			     (loop repeat *num-trials*
				   collect (only-if t (test t)))
			     (only-if nil (test t)))))))

   (let ((*num-trials* 3))
     (is= "
SKIP (TEST NIL)
  for ((K 0))
  0 cases checked and passed in 12 attempts.
1 test submitted; 1 SKIPPED."
	  (output-of (report (quietly (for-all ((k (lambda () 0)))
					(only-if nil (test nil))))))))

   (named
    "Tests are reported in the order they were first encountered."
    (is= "
FAIL A
  2/2 counterexamples.
FAIL B
FAIL C
3 tests submitted; 3 FAILED."
	 (output-of (report (quietly
			     (named 'a (test nil))
			     (named 'b (test nil))
			     (named 'c (test nil))
			     (named 'a (test nil)))))))

   (is= "HEY1THERE1"
	(output-of
	 (wrap-each (let ((x 0))
		      (princ wrappee)
		      (princ (incf x)))
	   'hey
	   'there)))

   (define (a-foo)
     "Doc comment for A-FOO."
     42)
   (is= "Doc comment for A-FOO."
	(documentation 'a-foo 'variable))
   (is= 42 (funcall a-foo))

   (named "Bindings reported from FOR-ALL nest properly."
	  (is= (test-bindings (quietly
			       (for-all ((a (lambda () 1)))
				 (for-all ((a (lambda () 2)))
				   (test nil)))))
	       '((a 2) (a 1)))))
