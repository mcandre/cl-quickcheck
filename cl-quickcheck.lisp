; Copyright 2010 Andrew Pennebaker under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html
;
; Copyright 2004 Darius Bacon
;
; A library for test-driven development -- see self-test.lisp for an
; example of how to use it.
;
; This code could be simpler if we just printed test results to the
; console as each one gets computed, and always jumped right into the
; debugger on failure.  But that's not the behavior I want:
;
; First, in a test-driven style, we write the test first, then run it
; and see it fail, then fix the code, then rerun the test.  Jumping
; into the debugger on failure is a waste of time in that scenario
; (although if the debugger somehow knew to open the editor positioned
; right where you need to fix the code, that could change things).
; Normally after running a test suite we want just a quick indication
; of what's wrong, if anything, with the expectation that the problem
; is usually an obvious consequence of your last change.  You should
; set up your environment so that rerunning the tests is just a
; keystroke away.  Maybe it'd be a good thing to have separate
; keystrokes for expecting failure vs. expecting success (jumping into
; the debugger in the latter case).
;
; Second, it's convenient to write multiple tests over the same random
; test cases, like (for-all (x) (test1 x) (test2 x)).  Coherent
; reporting groups by test, not by test case.  OTOH the natural
; evaluation order groups evaluation by test case; that's the order I
; chose, though I wonder if it's not the best choice.
;
; Third, with cl-quickcheck's random test case generation, you can expect
; to get a large number of failing test cases.  We'd like to see some
; sort of summary of them before debugging.
;
; So the design here has us completing all the tests before reporting
; on any of them.  While we're still running the tests we output just
; a "progress bar" of dots.
;
; TODO:
;  rest of quickcheck
;    trivial-if 
;    histograms
;    function generation
;  check whether defaults for *num-trials* and *size* work well
;  more useful behavior in the debugger after a failure
;    a restart to retry the failed test
;    a restart to single-step through the failed test again
;  better naming?  i'm not very happy with some of the exported names.
;  output in the format expected by some cross-language test runner
;  change to use CL conditions instead of dynamic function variable?
;  use ASSERT and *BREAK-ON-SIGNALS* ?
;  use type names in place of generator names?
;  make PRINT-TEST the default printer for test structs?
;  noticeably slow in CLISP - worth tuning?
;  see farther-out stuff in ideas file

(defpackage :cl-quickcheck
  (:export :quickcheck :collect-test-results :report
           :test :is :isnt :is= :isnt= :should-signal
	   :named :wrap-each :only-if :for-all 
	   :an-index :an-integer :a-real :a-boolean :a-list :a-tuple :a-member :a-char :a-string :a-symbol
           :k-generator :m-generator :n-generator
	   :define :generate :pick-weighted
           :*testing* :*break-on-failure* :*loud* :*num-trials* :*size*
           :test-name :test-flopped :test-detail :test-bindings)
  (:use :common-lisp))
(in-package :cl-quickcheck)

; Test constructors

(eval-when (:execute :compile-toplevel :load-toplevel)
  (defmacro quickcheck (&body body)
    "Run BODY and report the results of any tests."
    `(run-quickcheck (lambda () ,@body)))

  (defmacro test (flag)
    "Test that FLAG is true."
    `(run-tester '(test ,flag) (lambda () ,flag)))

  (defmacro is (fn &rest operands)
    "Test that FN applied to OPERANDS is true, with the failure message
detailing the failing arguments."
    (is-macro `(is ,fn ,@operands) `#',fn operands))

  (defmacro isnt (fn &rest operands)
    "Test that FN applied to OPERANDS is false."
    (is-macro `(isnt ,fn ,@operands) `(complement #',fn) operands))

  (defmacro is= (x y)
    "Test that X is EQUAL to Y."
    (is-macro `(is= ,x ,y) '#'equal (list x y)))

  (defmacro isnt= (x y)
    "Test that X is not EQUAL to Y."
    (is-macro `(isnt= ,x ,y) '(complement #'equal) (list x y)))

  (defun is-macro (form fn operands)
    `(run-is-tester ',form (lambda () (list ,fn ,@operands))))

  (defmacro should-signal (condition &body body)
    "Test that evaluating BODY signals (a subtype of) CONDITION."
    `(run-should-signal '(should-signal ,condition ,@body)
			,condition
			(lambda () ,@body)))

  (defmacro let-dfv (((name params &body body1)) &body body2)
    "Bind a dynamic function variable, with CALL-NEXT-FUNCTION in its
body calling the same variable as bound in the enclosing dynamic scope."
    (let ((parent (gensym)))
      `(let ((,name (let ((,parent ,name))
		      (flet ((call-next-function ,params 
			       (funcall ,parent ,@params)))
			(lambda ,params ,@body1)))))
	 ,@body2)))

  (defmacro named (name &body tests)
    "Perform the given TESTS with all the test names set to NAME."
    `(run-named ,name (lambda () ,@tests)))

  (defvar *logger* (lambda (test) test)
    "Function to do whatever's appropriate with the result of each test
as it completes.")

  (defun run-named (name fn)
    (let-dfv ((*logger* (test)
                (call-next-function (update-name test name))))
      (funcall fn)))

  (defmacro wrap-each (wrapper &body wrappees)
    "Perform each of the WRAPPEES as if surrounded by WRAPPER (with 
the literal symbol WRAPPEE inside WRAPPER being the hole where the 
wrappee appears).  This is useful for factoring out common setup/teardown 
code for a sequence of tests without compromising their isolation."
    ;; TODO: use a macrolet for a cleaner expansion?  Make wrappee a
    ;; parameter?
    `(progn ,@(mapcar (lambda (wrappee)
			`(symbol-macrolet ((wrappee ,wrappee))
			   ,wrapper))
		      wrappees)))

  (defmacro only-if (flag test)
    "Perform the TEST only if FLAG is true, otherwise return a SKIPPED
test result whose name is TEST quoted."
    `(run-only-if ',flag (lambda () ,flag) ',test (lambda () ,test)))

  (defmacro for-all (bindings &body body)
    "Perform the test in BODY for random values of BINDINGS."
    (let ((bindings (mapcar #'normalize-binding bindings)))
      (let ((vars (mapcar #'first bindings))
	    (generators (mapcar #'second bindings)))
	`(run-for-all (lambda ,vars ,@body) ',vars ,@generators))))

  (defun normalize-binding (binding)
    "Return BINDING's pair of name and generator expression."
    (cond ((and (consp binding) (= 2 (length binding)))
	   binding)
	  ((symbolp binding) 
	   (list binding (default-generator binding)))
	  (t (error "Not a variable binding: ~S" binding))))

  (defun default-generator (name)
    "Give a generator expression for a name that's missing an explicit one.
You'll have to define the meaning of this shorthand elsewhere."
    (concat-symbol name "-GENERATOR"))

  (defun concat-symbol (&rest parts)
    "Intern a symbol by catenating PARTS."
    (intern (format nil "~{~a~}" parts))))

; Test structs

; A test case either passed or flopped; if it flopped, either it was
; skipped or it failed; if it failed, there was a false assertion or
; an unexpected error.

; A (general) test is either a test case or a list of general tests.
; TODO: be consistent about naming tests vs. cases in the code.

(defstruct test 
  name      ; What test was run.
  flopped   ; NIL if passed, 'SKIPPED if skipped, T if false assertion,
	    ; or a condition if an error occurred.
  detail    ; Function of no args to write more info to stdout.
  bindings) ; A-list of variables and values from FOR-ALL.

(defun test-failed (test)
  (and (test-flopped test) (not (test-skipped test))))

(defun test-skipped (test)
  (eq 'skipped (test-flopped test)))

(defun test-passed (test)
  (not (test-flopped test)))

(defun update-name (test name)
  (make-test :name name
             :flopped (test-flopped test)
             :detail (test-detail test)
             :bindings (test-bindings test)))

(defun update-bindings (test bindings)
  (make-test :name (test-name test)
             :flopped (test-flopped test)
             :detail (test-detail test)
             :bindings bindings))

(defun sort-out (tests)
  "Collect the test cases of TESTS into separate lists by name, and
return them in order of the first appearance of a case with that name."
  (let ((dups (collect-dups tests)))
    (loop for name in (unique-names tests)
	  collect (gethash name dups))))

(defun unique-names (tests)
  "Return the test-names of TESTS, in order of first appearance."
  (let ((names '()))
    (dolist (test tests)
      (pushnew (test-name test) names :test #'equal))
    (nreverse names)))

(defun collect-dups (tests)
  "Return a hashtable from names of elements of TESTS to lists of the
elements with the same name."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (test tests)
      (push test (gethash (test-name test) table '())))
    table))

; Performing the tests

(defvar *break-on-failure* nil
  "When true, test failures jump us immediately into the debugger.")

(eval-when (:execute :compile-toplevel :load-toplevel)
  (defmacro capture-stdout (&body body)
    `(with-output-to-string (*standard-output*)
       ,@body)))

(defvar *testing* nil
  "When true, we're in the dynamic extent of a quickcheck form.")

(defun collect-test-results (fn)
  "Call FN with *TESTING* true, and return a list of the test results."
  (let ((tests '())
	(*testing* t))
    (let-dfv ((*logger* (test) 
		(push test tests) 
		test))
      (funcall fn))
    (nreverse tests)))

(defun answer (name flopped &optional (detail (lambda () t)))
  "Log a test outcome, with appropriate interactive side effects."
  (when (and *break-on-failure* flopped (not (eq flopped 'skipped)))
    (cerror "Run the remaining tests."
	    "Test failed: ~S~a" name (capture-stdout (funcall detail))))
  (show-progress flopped)
  (funcall *logger* (make-test :name name :flopped flopped :detail detail)))

(defun run-tester (name passp-fn)
  "Return a test outcome from calling PASSP-FN."
  (answer name (call-tester passp-fn)))

(defun call-tester (passp-fn)
  "Call PASSP-FN and return whether it flopped."
  (multiple-value-bind (passed condition) (intercept-errors passp-fn)
    (or condition (not passed))))

(defun run-is-tester (name fn-fn)
  "Return a test outcome from the result of FN-FN, which returns a
list of a function to call for the actual test, plus its arguments."
  (multiple-value-bind (fn-and-arguments condition) (intercept-errors fn-fn)
    (if condition
        (answer name condition)
        (destructuring-bind (fn . arguments) fn-and-arguments
	  (answer name 
		  (call-tester (lambda () (apply fn arguments)))
		  (lambda ()
		    (format t "~%  with values~{ ~S~}" arguments)))))))

(defun run-should-signal (name expected-condition fn)
  "Test that calling FN signals (a subtype of) EXPECTED-CONDITION."
  (multiple-value-bind (value condition) 
		       (ignore-errors (prog1 (funcall fn)))
    (declare (ignore value))
    (answer name (not (typep condition expected-condition)))))

(defun run-only-if (flag-name flag-fn name test-fn)
  "Behavior of the ONLY-IF macro."
  (multiple-value-bind (flag condition) (intercept-errors flag-fn)
    (cond (condition (answer flag-name condition))
	  (flag (funcall test-fn))
	  (t (answer name 'skipped)))))

(defun intercept-errors (fn)
  (if *break-on-failure*
      (prog1 (funcall fn))
      (ignore-errors (prog1 (funcall fn)))))

; Generating random test cases

(defvar *num-trials* 100
  "Number of random trials we want to see pass in FOR-ALL tests.")

(defun run-for-all (test-fn vars &rest generators)
  "Repeatedly call TEST-FN with VARS bound to values from GENERATORS."
  (let ((status (make-hash-table :test #'equal)))
    (let-dfv ((*logger* (test)
	        (let ((name (test-name test)))
		  (setf (gethash name status) 
			(tally test (gethash name status 0)))
		  (call-next-function test))))
      (loop repeat (* 4 *num-trials*)
	    do (run-trial test-fn vars generators)
	    until (every #'judgedp (hash-table-values status))))))
; TODO: we should record at this time whether a test passed
; *num-trials* times, rather than checking afterwards when
; *num-trials* could have a different value.

(defun judgedp (opt-count)
  "OPT-COUNT describes a repeated test: NIL if it ever failed,
otherwise a count of passed trials.  Return true iff the test has
either passed *NUM-TRIALS* times or failed at least once."
  (or (not opt-count) (<= *num-trials* opt-count)))

(defun tally (test opt-count)
  (and opt-count
       (not (test-failed test))
       (+ opt-count (if (test-skipped test) 0 1))))

(defun hash-table-values (table)
  "Return a list of TABLE's values in arbitrary order."
  (let ((results '()))
    (maphash (lambda (key value) 
	       (declare (ignore key))
	       (push value results)) 
	     table)
    results))

(defun run-trial (test-fn vars generators)
  "Run one trial of a FOR-ALL test."
  (let* ((values (mapcar #'generate generators))
	 (bindings (mapcar #'list vars values)))
    (let-dfv ((*logger* (test)
	        (call-next-function
		 (update-bindings test 
				  (append (test-bindings test) bindings)))))
      (apply test-fn values))))

(defun generate (generator)
  "Ask GENERATOR to produce a value."
  (funcall generator))

; Basic generators

(defvar *size* 20
  "Bounds the size of random test cases in a generator-dependent way.")

; We need to define the value cell because names like A-BOOLEAN are
; used in the value position in expressions, not the function
; position.  Unfortunately Common Lisp won't let us make a lexically
; scoped global -- this seems about the best we can do.
(eval-when (:execute :compile-toplevel :load-toplevel)
  (defmacro define (binding &body body)
    "Like Scheme's top-level DEFINE, more or less."
    (cond ((symbolp binding)
	   `(defparameter ,binding ,@body))
	  (t (let ((fn (first binding))
		   (params (rest binding)))
	       (if (and (stringp (first body))
			(not (null (rest body))))
		   `(defparameter ,fn
		      (lambda ,params ,@body)
		      ,(first body))
		   `(defparameter ,fn
		      (lambda ,params ,@body))))))))

(define (a-boolean)
  (= 0 (random 2)))

(define (an-index)
  (random *size*))

(define (an-integer)
  (- (random (+ *size* *size* 1)) *size*))

(define (a-real) 
  (- (random (float (* 2 *size*)))
     *size*))

(defun a-list (generator)
  (lambda ()
    (loop repeat (random *size*)
          collect (generate generator))))

(defun a-tuple (&rest generators)    ; TODO - add a test for this
  (lambda ()
    (mapcar #'generate generators)))

(defun a-member (&rest generators)
  (lambda ()
    (generate (random-element generators))))

; a-char, a-string, a-symbol added in v0.4
(defun a-char ()
  (generate #'(lambda () (code-char (random 256)))))

(defun a-string ()
  (coerce (loop repeat (random *size*)
                collect (a-char))
          'string))

(defun a-symbol ()
  (intern (a-string)))

(defun random-element (list)
  (nth (random (length list)) list))

; Standard shorthands (see DEFAULT-GENERATOR)

(define k-generator an-index)
(define m-generator an-integer)
(define n-generator an-integer)

; Helper for custom test-data generators

(defmacro pick-weighted (&body choices)
  "Given CHOICES with constant weights, pick a random one at runtime."
  (let ((total (loop for (weight . body) in choices
		     sum weight))
	(var (gensym))
	(accum 0))
    `(let ((,var (random ,total)))
       (cond ,@(loop for (weight . body) in choices
		     collect `((< ,var ,(incf accum weight)) ,@body))
	     (t (error "Bug"))))))

; Reporting results

(defvar *loud* t
  "When true, we show progress as tests are run with dots to stdout.")

(defun show-progress (flopped)
  "Write a single character as a bird's-eye view of a test result."
  (when *loud*
    (write-char (case flopped
		  ((nil)     #\.)
		  ((skipped) #\-)
		  ((t)       #\X)
		  (t         #\@)))
    (force-output)))

(defun run-quickcheck (fn)
  "Call FN to run a test suite, and report the result."
  (format t "~&Starting tests with seed ~s~%" *random-state*)
  (report (collect-test-results fn)))

(defun report (test-cases)
  "Print out the interesting test results in longer form."
  (let* ((tests (sort-out test-cases))
	 (verdicts (mapcar #'verdict tests)))
    (mapc #'summarize-test tests)
    (summarize-all verdicts)
    (every #'test-passed verdicts)))

(defun summarize-test (cases)
  "Report the results of the test cases of a test, if they're interesting."
  (multiple-value-bind (num-failed num-skipped num-passed num-cases)
		       (distribution cases)
    (unless (and (= 0 num-failed) 
		 (or (= 0 num-skipped) (<= *num-trials* num-passed)))
      (print-test (verdict cases))
      (when (< 1 num-cases)
	(when (< 0 num-failed)
	  (format t "~%  ~a/~a counterexamples." num-failed num-cases))
	(when (< 0 num-skipped)
	  (format t "~%  ~a case~p checked and passed in ~a attempts."
		  num-passed num-passed num-cases))))))

(defun print-test (test)
  (let ((flopped (test-flopped test)))
    (format t "~%~a ~S" (classify flopped) (test-name test))
    (when (eq 'error (classify flopped))
      (format t "~%  ~a" flopped))
    (funcall (test-detail test))
    (when (test-bindings test)
      (format t "~%  for ~S" (test-bindings test)))))

(defun classify (flopped)
  (case flopped
    ((nil)     'pass)
    ((skipped) 'skip)
    ((t)       'fail)
    (t         'error)))

(defun summarize-all (verdicts)
  (multiple-value-bind (num-failed num-skipped num-passed total)
		       (distribution verdicts)
    (format t "~%~a test~p submitted" total total)
    (when (and (< 0 total) (= num-passed total))
      (format t "; all passed"))
    (when (< 0 num-failed)
      (format t "; ~a FAILED" num-failed))
    (when (< 0 num-skipped)
      (format t "; ~a SKIPPED" num-skipped))
    (format t ".")))

(defun distribution (tests)
  "Count test cases failed, skipped, passed, and total."
  (values (count-if #'test-failed tests)
	  (count-if #'test-skipped tests)
	  (count-if #'test-passed tests)
	  (length tests)))

(defun verdict (tests)
  "Choose the most significant result from TESTS: failed, passed, or
skipped, in that order."
  (or (find-if #'test-failed tests)
      (find-if #'test-passed tests)
      (first tests)))
