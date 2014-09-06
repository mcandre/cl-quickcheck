; Copyright 2010 Andrew Pennebaker under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html
;
; Copyright 2004 Darius Bacon
;
; This code goes with the cl-quickcheck test library -- the idea is to
; make it so easy to put together a regression test suite that there's
; no excuse not to.  Even without such a suite, you normally check out
; your code interactively at the Lisp prompt; so the idea here is you
; paste a transcript of the successful interactions right into your
; source file and it gets regression-checked automatically from then
; on.  (Python's doctest module and E's Updoc follow the same strategy
; -- this is an imitation of them.  Not a very good imitation, I'm
; afraid -- for one thing, the syntax is MCL-specific.)

; E's Updoc is designed to check the examples in documentation files,
; so we could hardly call this a Lisp version without that feature as
; well.  It has at least two benefits: keeping documentation from
; going stale, and more subtly to help prevent tests from becoming
; weaker over time as harried programmers fix them to keep passing
; after changes to the code, because the tests are in the
; documentation and you need to keep the examples in your docs up to
; date for your customers anyway (he said naively).

; The syntax in doc files is kind of unfortunate: we still need #{} 
; wrapping like the corresponding read macro in Lisp files.

; We ought to try doing something about testing examples from doc
; strings -- does anyone put many examples there?
 
; I'll bet it'd help if, on test failure, your IDE could give you the
; option of updating the test to follow the newer results.  (Instead
; of manually editing them.)

(in-package :cl-quickcheck)

(export '(test-doc-file test-doc-stream doc-test))


(defun test-doc-file (filename)
  "Scan a documentation file for embedded #{} test cases and test them."
  (with-open-file (stream filename)
    (test-doc-stream stream)))

(defun test-doc-stream (stream)
  (eval `(progn ,@(parse-doc-stream stream))))

(defmacro doc-test (expr &rest expectation)
  `(run-tester '(doc-test ,expr ,@expectation)
               (lambda () 
                 (equal ',expectation (multiple-value-list ,expr)))))

(defun parse-doc-stream (stream)
  (let ((c (read-char stream nil nil)))
    (cond ((not c) '())
          ((and (char= c #\#)
		(char= (peek-char t stream nil nil) #\{))
	   (read-char stream)
	   (cons (updoc-stream stream)
		 (parse-doc-stream stream)))
          (t (parse-doc-stream stream)))))


(set-dispatch-macro-character #\# #\{ (lambda (stream c wtf)
                                        (declare (ignore c wtf))
                                        (updoc-stream stream)))

(defun updoc-stream (stream)
  (let ((cases '()))
    (loop
     (multiple-value-bind (input expectation) (read-test-case stream)
       (if input
           (push `(doc-test ,input ,@expectation) cases)
           (return `(progn ,@(reverse cases))))))))

(defun read-test-case (stream)
  (and (read-prompt stream)
       (values (read stream) (read-expectation stream))))

(defun read-prompt (stream)
  (let ((c (read stream nil nil)))
    (cond ((member c '(nil })) nil)
          ((eq c '?) t)
          (t (error "Expected a '?' prompt")))))

(defun read-expectation (stream)
  "Return a list of s-expressions read from STREAM up to the next
occurrence of ?, }, or eof."
  (if (member (peek-char t stream nil nil) '(#\? #\} nil))
      '()
      (cons (read stream)
            (read-expectation stream))))


; Self test

(defun example-test ()
  #{
? (+ 2 3)
5
? (truncate 27 5)
5
2
  })

(when *testing*
  (example-test)
  (test-doc-stream (make-string-input-stream "

This is some sample documentation text to check.
Here are some inline tests:

#{
? (append '(a b c) '(x y z))
(A B C X Y Z)
? (length '(a b c))
3
}

And more tests:
#{
? (not nil)
T
}
")))
