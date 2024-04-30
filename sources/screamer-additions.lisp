(in-package :screamer)

;;; note: The following random functions was included
;;; for experimental purposes on music constraints.

(cl:defun permut-random (input)
"Returns a random permutation of input."
 (labels
  ((takeout (i list)
    (cond ((= i 0) (subseq list 1 (length list)))
          ((= i (1- (length list))) (butlast list))
          (t (append
              (subseq list 0 i)
              (subseq list (1+ i) (length list)))))))
 (let ((list (copy-seq input))
      (r nil))
   (loop for i from 0 while (< i (length input)) do
        (unless (= 0 (length list))
          (let ((j (random (length list))))
            (push (elt list j) r)
            (setf list (takeout j list)))))
   r)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (screamer::declare-nondeterministic 'a-random-member-of))

(cl:defun a-random-member-of (sequence)
  "Nondeterministically returns an random element of SEQUENCE. The SEQUENCE must be
either a list or a vector."
  (declare (ignore sequence))
  (screamer::screamer-error
   "A-RANDOM-MEMBER-OF is a nondeterministic function. As such, it must be called~%~
   only from a nondeterministic context."))

(cl:defun a-random-member-of-nondeterministic (continuation sequence)
(let ((sequence (permut-random (value-of sequence))))
  (cond
    ((listp sequence)
     (unless (null sequence)
       (screamer::choice-point-external
        (loop (if (null (rest sequence)) (return))
          (screamer::choice-point-internal (funcall continuation (first sequence)))
          (setf sequence (value-of (rest sequence)))))
       (funcall continuation (first sequence))))
    ((vectorp sequence)
     (let ((n (length sequence)))
       (unless (zerop n)
         (let ((n (1- n)))
           (choice-point-external
            (dotimes (i n)
              (choice-point-internal (funcall continuation (aref sequence i)))))
           (funcall continuation (aref sequence n))))))
    (t (error "SEQUENCE must be a sequence")))))
	
(defmacro-compile-time n-values (n
	 		    &body forms)				
"FROM T2L-SCREAMER AND SMC(PWGL):
 Copyright (c) 2007, Kilian Sprotte. All rights reserved."
 (let ((values (gensym "VALUES-"))
       (last-value-cons  (gensym "LAST-VALUE-CONS-"))
       (value (gensym "VALUE-")))
   `(let ((,values '())
          (,last-value-cons nil)
    (number 0))
      (block n-values
  (for-effects
    (let ((,value (progn ,@forms)))
      (global (cond ((null ,values)
 		    (setf ,last-value-cons (list ,value))
 		    (setf ,values ,last-value-cons))
 		   (t (setf (rest ,last-value-cons) (list ,value))
 		      (setf ,last-value-cons (rest ,last-value-cons))))
 	     (incf number))
      (when (>= number ,n) (return-from n-values)))))
      ,values)))
	  
(defmacro-compile-time print-values (&body forms)
  "Evaluates EXPRESSIONS as an implicit PROGN and outputs
each of the nondeterministic values returned by the last EXPRESSION in
succession using a LISTENER WINDOW (Openmusic).

After each value is printed, the user is queried as to whether or not further
values are desired. These values are produced by repeatedly evaluating the
body and backtracking to produce the next value, until either the user
indicates that no further values are desired or until the body fails and
yields no further values.

Accordingly, local side effects performed by the body while producing each
value are undone after printing each value, before attempting to produce
subsequent values, and all local side effects performed by the body are undone
upon exit from PRINT-VALUES, either because there are no further values or
because the user declines to produce further values.

A PRINT-VALUES expression can appear in both deterministic and
nondeterministic contexts. Irrespective of what context the PRINT-VALUES
expression appears in, the EXPRESSIONS are always in a nondeterministic
context. A PRINT-VALUES expression itself is always deterministic and always
returns NIL.

PRINT-VALUES is analogous to the standard top-level user interface in Prolog."
`(catch 'succeed
   (for-effects
     (let ((value (progn ,@forms)))         
         (unless (om::non-determinise-listener value)
           (throw 'succeed value))))))


;; OLD VERSION FROM OM 4 (WITH GLOBAL VARIABLE -> PREFERENCES PANEL: REMOVED IN THIS VERSION)
   
;(defmacro-compile-time print-values (&body forms)
; `(catch 'succeed
;    (for-effects
;      (let ((value (progn ,@forms)))         
;        (if (= om::*screamer-valuation* 2)
;          (unless (om::non-determinise-listener value)
;            (throw 'succeed value))
;          (progn (throw 'succeed value) (print value)))))))
