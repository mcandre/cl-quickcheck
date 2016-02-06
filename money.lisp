; Copyright 2010 Andrew Pennebaker under the terms of the MIT X license
; found at http://www.opensource.org/licenses/mit-license.html
;
; Copyright 2004 Darius Bacon
;
; Example from Kent Beck, _Test-Driven Development_

(use-package :cl-quickcheck)

(defstruct money amount currency)

(defun dollars (n)
  (make-money :amount n :currency 'usd))

(defun francs (n)
  (make-money :amount n :currency 'franc))

(defun money* (n money)
  (make-money :amount (* n (money-amount money))
	      :currency (money-currency money)))

(defun money= (money1 money2)
  (and (= (money-amount money1) (money-amount money2))
       (eql (money-currency money1) (money-currency money2))))

(when *testing*
  (for-all (m)
    (isnt money= (dollars m) (francs m))   ; deliberately failing
    (is money= (dollars m) (dollars m)))
  (for-all (m n)
    (only-if (/= m n)
             (isnt money= (dollars m) (dollars n)))      ; deliberately failing
    (is money=
        (money* m (francs n))
        (francs (* m n)))))
