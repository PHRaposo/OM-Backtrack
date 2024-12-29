(in-package :screamer)

;; note: The following random functions was included
;; for experimental purposes on music constraints.
;; FROM OPENMUSIC
  
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

(defun om-random-value (num)
  (if (= num 0) 0
  (if (< num 0)
    (- (random (- num)))
    (random num))))

(defun nth-random (list)
 (nth (om-random-value (length list)) list))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (declare-nondeterministic 'a-random-member-of))

(cl:defun a-random-member-of (sequence)
  "Nondeterministically returns an random element of SEQUENCE. The SEQUENCE must be
either a list or a vector."
  (declare (ignore sequence))
  (screamer-error
   "A-RANDOM-MEMBER-OF is a nondeterministic function. As such, it must be called~%~
   only from a nondeterministic context."))
 
(cl:defun a-random-member-of-nondeterministic (continuation sequence)
(let ((sequence (value-of sequence)))
  (cond
    ((listp sequence)
     (unless (null sequence)
       (choice-point-external
        (loop (if (null (rest sequence)) (return))
	     (let ((random-el (nth-random sequence)))
          (choice-point-internal (funcall continuation random-el))
           (setf sequence (value-of (remove random-el sequence :test #'equal :count 1))))))
       (funcall continuation (first sequence))))
    ((vectorp sequence)
     (let ((n (length sequence)))
       (unless (zerop n)
	    (let ((curr-n n)
		       (n (1- n)))
           (choice-point-external
            (dotimes (i n)
			 (decf curr-n) 
			 (let* ((random-el (aref sequence (om-random-value curr-n))))			      
              (choice-point-internal (funcall continuation random-el))
			  (setf sequence (value-of (remove random-el sequence :test #'equal :count 1))))))
           (funcall continuation (aref sequence 0))))))
    (t (error "SEQUENCE must be a sequence")))))

#| OLD VERSION BASED ON PERMUT-RANDOM
(cl:defun a-random-member-of-nondeterministic (continuation sequence)
(let ((sequence (permut-random (value-of sequence))))
  (cond
    ((listp sequence)
     (unless (null sequence)
       (choice-point-external
        (loop (if (null (rest sequence)) (return))
          (choice-point-internal (funcall continuation (first sequence)))
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
    (t (error "SEQUENCE must be a sequence")))))|#
					 	
(defmacro-compile-time n-values (n
	 		    &body forms)				
"FROM T2L-SCREAMER AND SMC(PWGL):
 Copyright (c) 2007, Kilian Sprotte. All rights reserved.
 TODO - DOC	 
"
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

; ================================================================================================ ;
;; CHANGES FROM SWAPNEILS : https://github.com/swapneils/screamer/tree/master
; ================================================================================================ ;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declare-nondeterministic 'apply-nondeterministic))

(cl:defun apply-nondeterministic (function &rest arguments)
  "Analogous to the CL:APPLY, except FUNCTION can be either a nondeterministic
function, or an ordinary deterministic function.

You must use APPLY-NONDETERMINISTIC to apply a nondeterministic function. An
error is signalled if a nondeterministic function object is used with
CL:APPLY.

You can use APPLY-NONDETERMINISTIC to apply either a deterministic or
nondeterministic function, though even if all of the ARGUMENTS are
deterministic and FUNCTION is a deterministic function object, the call
expression will still be nondeterministic \(with presumably a single value),
since it is impossible to determine at compile time that a given call to
APPLY-NONDETERMINISTIC will be passed only deterministic function objects for
function."
  (declare (ignore function arguments))
  (screamer-error
   "APPLY-NONDETERMINISTIC is a nondeterministic function. As such, it must~%~
   be called only from a nondeterministic context."))
		
(cl:defun apply-nondeterministic-nondeterministic
    (continuation function argument &rest arguments)
  (let ((function (value-of function)))
    (if (nondeterministic-function? function)
	    (apply #'apply (nondeterministic-function-function function)
	               continuation argument arguments)
	        (funcall continuation (apply #'apply function argument arguments)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declare-nondeterministic 'mapcar-nondeterministic))

(cl:defun mapcar-nondeterministic (function &rest arguments)
  "Analogous to the CL:mapcar, except FUNCTION can be either a nondeterministic
function, or an ordinary deterministic function.
You must use mapcar-NONDETERMINISTIC to mapcar a nondeterministic function. An
error is signalled if a nondeterministic function object is used with
CL:mapcar.
You can use MAPCAR-NONDETERMINISTIC to mapcar either a deterministic or
nondeterministic function, though even if all of the ARGUMENTS are
deterministic and FUNCTION is a deterministic function object, the call
expression will still be nondeterministic \(with presumably a single value),
since it is impossible to determine at compile time that a given call to
MAPCAR-NONDETERMINISTIC will be passed only deterministic function objects for
function."
  (declare (ignore function arguments))
  (screamer-error
   "mapcar-NONDETERMINISTIC is a nondeterministic function. As such, it must~%~
   be called only from a nondeterministic context."))

(cl:defun mapcar-nondeterministic-nondeterministic
    (continuation function argument &rest arguments)
  (let ((function (value-of function)))
    (if (nondeterministic-function? function)
        (funcall continuation
                 (apply #'mapcar
                        (lambda (&rest args)
                          (let (res) ;<==fix (phraposo)
                           (apply (nondeterministic-function-function function)
                                  (lambda (r) (push r res)) ;<== push
                                 args)
                            (reverse res))) ;<== reverse
                        argument arguments))
        (funcall continuation (apply #'mapcar function argument arguments)))))

(defun +-rule-down (z x y)
  ;; note: We can't assert that X and Y are integers when Z is an integer since
  ;;       Z may be an integer when X and Y are Gaussian integers. But we can
  ;;       make such an assertion if either X or Y is "an integer" (original: "is real").
  ;;	   If the Screamer type system could distinguish Gaussian integers from other
  ;;       complex numbers we could make such an assertion whenever either X or Y was
  ;;       not a Gaussian integer.
  (if (and (variable-integer? z) (or (variable-integer? x) (variable-integer? y)))
 ;(if (and (variable-integer? z) (or (variable-real? x) (variable-real? y)))
	  (restrict-integer! x))
  ;; note: Ditto.
  (if (and (variable-real? z) (or (variable-real? x) (variable-real? y)))
      (restrict-real! x))
  (if (and (variable-real? x) (variable-real? y) (variable-real? z))
      (restrict-bounds!
       x
       (infinity-- (variable-lower-bound z) (variable-upper-bound y))
       (infinity-- (variable-upper-bound z) (variable-lower-bound y))))
  (let ((x (value-of x))
        (y (value-of y))
        (z (value-of z)))
    (if (and (not (variable? x))
             (not (variable? y))
             (not (variable? z))
             (/= z (+ x y)))
        (fail))))
		
(defun *-rule-down (z x y)
  ;; note: We can't assert that X and Y are integers when Z is an integer since
  ;;       Z may be an integer when X and Y are Gaussian integers. But we can
  ;;       make such an assertion if either X or Y is "an integer" (original: "is real").
  ;;       If the Screamer type system could distinguish Gaussian integers from other
  ;;       complex numbers we could make such an assertion whenever either X or Y was
  ;;       not a Gaussian integer.
  (if (and (variable-integer? z) (or (variable-integer? x) (variable-integer? y)))
 ;(if (and (variable-integer? z) (or (variable-real? x) (variable-real? y)))
      (restrict-integer! x))
  ;; note: Ditto.
  (if (and (variable-real? z) (or (variable-real? x) (variable-real? y)))
      (restrict-real! x))
  (if (and (variable-real? x) (variable-real? y) (variable-real? z))
      (/-rule z y x))
  (let ((x (value-of x))
        (y (value-of y))
        (z (value-of z)))
    (if (and (not (variable? x))
             (not (variable? y))
             (not (variable? z))
             (/= z (* x y)))
        (fail))))

(defun assert!-notv-equalv (x y)
 (cond
   ((known?-equalv x y) (fail))
   ((not (known?-notv-equalv x y))
    (let* ((x (variablize x))
           (y (variablize y))
           (noticer #'(lambda ()
                        (cond ((and (known?-numberpv x)
                                    (known?-numberpv y))
                               (/=-rule x y))
                              ((known?-equalv x y) (fail))))))
      (attach-noticer! noticer x)
      (attach-noticer! noticer y)))))
	  
;; NOTE: the following modifitations does not work so well with Screamer Plus.
#|
;; ALLOW =V AND /=V TO USE ENUMERATED DOMAINS
(defun =-rule (x y)
  (cond
    ;; note: I forget why +-RULE *-RULE MIN-RULE and MAX-RULE must perform the
    ;;       check in the second COND clause irrespective of whether the first
    ;;       clause is executed.
    ((and (variable-real? x) (variable-real? y))
     (restrict-bounds! x (variable-lower-bound y) (variable-upper-bound y))
     (restrict-bounds! y (variable-lower-bound x) (variable-upper-bound x)))
    ((and (not (variable? x)) (not (variable? y)) (/= x y)) (fail)))
  (when (or (variable? x) (variable? y))
    (let ((xdom (cond
                  ((and (variable? x)
                        (subtypep (type-of (variable-enumerated-domain x)) 'list))
                   (variable-enumerated-domain x))
                  ((bound? x) (list (value-of x)))
                  (t nil)))
          (ydom (cond
                  ((and (variable? y)
                        (subtypep (type-of (variable-enumerated-domain y)) 'list))
                   (variable-enumerated-domain y))
                  ((bound? y) (list (value-of y)))
                  (t nil))))
      (when (and xdom ydom)
        (let ((joined (intersection xdom ydom)))
          (mapc
           (lambda (v)
             (when (variable? v)
               (restrict-enumerated-domain! v joined)))
           (list x y)))))))
		   
(defun /=-rule (x y)
  ;; note: Got rid of the nondeterministic version of /=-RULE.
  (let ((xv (value-of x))
        (yv (value-of y)))
    (cond ((and (not (variable? xv)) (not (variable? yv)) (= xv yv)) (fail))
          ((and (bound? xv)
                (variable? y))
           (if (listp (variable-enumerated-domain y))
               (when (member (value-of xv) (variable-enumerated-domain y))
                 (restrict-enumerated-domain! y (remove (value-of xv) (variable-enumerated-domain y))))
               (restrict-enumerated-antidomain! y (cons (value-of xv) (variable-enumerated-antidomain y)))))
          ((and (bound? yv)
                (variable? x))
           (if (listp (variable-enumerated-domain x))
               (when (member (value-of yv) (variable-enumerated-domain x))
                 (restrict-enumerated-domain! x (remove (value-of yv) (variable-enumerated-domain x))))
               (restrict-enumerated-antidomain! x (cons (value-of yv) (variable-enumerated-antidomain x))))))))

;; ALOW RESTRICT-BOUNDS! TO GROUND VARIABLES
;; FIX BY SWAPNEILS 
(defun restrict-bounds! (x lower-bound upper-bound)
  ;; note: X must be a variable.
  ;; note: LOWER-BOUND and UPPER-BOUND must be real constants.
  (when (variable-integer? x)
    (if lower-bound (setf lower-bound (ceiling lower-bound)))
    (if upper-bound (setf upper-bound (floor upper-bound))))
  (if (or (eq (variable-value x) x) (not (variable? (variable-value x))))
      (let ((run? nil))
        (when (and lower-bound
                   (or (not (variable-lower-bound x))
                       (> lower-bound (variable-lower-bound x))))
          (when (and (variable-upper-bound x)
                     (< (variable-upper-bound x) lower-bound))
            (fail))
          (when (or (not (variable-lower-bound x))
                    (not (variable-upper-bound x))
                    (>= (/ (- lower-bound (variable-lower-bound x))
                           (- (variable-upper-bound x) (variable-lower-bound x)))
                        *minimum-shrink-ratio*))
            (local (setf (variable-lower-bound x) lower-bound))
            (setf run? t)))
        (when (and upper-bound
                   (or (not (variable-upper-bound x))
                       (< upper-bound (variable-upper-bound x))))
          (when (and (variable-lower-bound x)
                     (> (variable-lower-bound x) upper-bound))
            (fail))
          (when (or (not (variable-lower-bound x))
                    (not (variable-upper-bound x))
                    (>= (/ (- (variable-upper-bound x) upper-bound)
                           (- (variable-upper-bound x) (variable-lower-bound x)))
                        *minimum-shrink-ratio*))
            (local (setf (variable-upper-bound x) upper-bound))
            (setf run? t)))
        (when run?
          (cond ((eq (variable-enumerated-domain x) t)
                 (if (and (variable-lower-bound x)
                          (variable-upper-bound x)
                          (variable-integer? x)
                          (or (null *maximum-discretization-range*)
                              (<= (- (variable-upper-bound x)
                                     (variable-lower-bound x))
                                  *maximum-discretization-range*)))
                     (set-enumerated-domain!
                      x (integers-between
                         (variable-lower-bound x)
                         (variable-upper-bound x)))))
                ((or (and lower-bound
                          (some #'(lambda (element) (< element lower-bound))
                                (variable-enumerated-domain x)))
                     (and upper-bound
                          (some #'(lambda (element) (> element upper-bound))
                                (variable-enumerated-domain x))))
                 ;; note: Could do less consing if had LOCAL DELETE-IF.
                 ;;       This would also allow checking list only once.
                 (set-enumerated-domain!
                  x (remove-if #'(lambda (element)
                                   (or (and lower-bound (< element lower-bound))
                                       (and upper-bound (> element upper-bound))))
                               (variable-enumerated-domain x)))))
          ;; When the range-size of x is 0, set (variable-value x)
          (let ((domain (domain-size x))
                (range (range-size x))
                (enumerated (variable-enumerated-domain x))
                (lower (variable-lower-bound x)))
            (when (or (eql domain 1)
                      (eql range 0))
              (setf (variable-value x)
                    (cond ((listp enumerated) (first enumerated))
                          (lower lower)
                          (t (variable-value x))))))
          (run-noticers x)))))
|#