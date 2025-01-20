(in-package :screamer)
			 	
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

; ================================================================================================ ;
;; CHANGES FROM SWAPNEILS : https://github.com/swapneils/screamer/tree/master
; ================================================================================================ ;
		
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
                                  (lambda (r) (push r res)) ;<== push (phraposo)
                                 args)
                            (reverse res))) ;<== reverse (phraposo)
                        argument arguments))
        (funcall continuation (apply #'mapcar function argument arguments)))))

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

;; MODIFIED VERSION OF RESTRICT-BOUNDS!	 - IN-PROGRESS

(defun restrict-lower-bound! (x lower-bound)
  ;; NOTE: X must be a variable.
  ;; NOTE: LOWER-BOUND must be a real constant.
  (if (variable-integer? x) (setf lower-bound (ceiling lower-bound)))
  (when (and (or (eq (variable-value x) x) (not (variable? (variable-value x))))
             (or (not (variable-lower-bound x))
                 (> lower-bound (variable-lower-bound x))))
    (if (and (variable-upper-bound x) (< (variable-upper-bound x) lower-bound))
        (fail))
    (when (or (not (variable-lower-bound x))
              (not (variable-upper-bound x))
              (>= (/ (- lower-bound (variable-lower-bound x))
                     (- (variable-upper-bound x) (variable-lower-bound x)))
                  *minimum-shrink-ratio*))
      (local (setf (variable-lower-bound x) lower-bound))
      (cond ((eq (variable-enumerated-domain x) t)
             (if (and lower-bound
                      (variable-upper-bound x)
                      (variable-integer? x)
                      (or (null *maximum-discretization-range*)
                          (<= (- (variable-upper-bound x) lower-bound)
                              *maximum-discretization-range*)))
                 (set-enumerated-domain!
                  x (integers-between lower-bound
                                      (variable-upper-bound x)))))
            ((some #'(lambda (element) (< element lower-bound))
                   (variable-enumerated-domain x))
             ;; NOTE: Could do less consing if had LOCAL DELETE-IF.
             ;;       This would also allow checking list only once.
             (set-enumerated-domain!
              x (remove-if #'(lambda (element) (< element lower-bound))
                           (variable-enumerated-domain x)))))
      (when (and (variable-lower-bound x)
                 (variable-upper-bound x)
                 (zerop (- (variable-upper-bound x) (variable-lower-bound x)));(roughly-= (variable-upper-bound x) (variable-lower-bound x))
				 )
        (local (setf (variable-value x) (variable-lower-bound x))))
      (run-noticers x))))

(defun restrict-upper-bound! (x upper-bound)
  ;; NOTE: X must be a variable.
  ;; NOTE: UPPER-BOUND must be a real constant.
  (when (variable-integer? x)
    (setf upper-bound (floor upper-bound)))
  (when (and (or (eq (variable-value x) x) (not (variable? (variable-value x))))
             (or (not (variable-upper-bound x))
                 (< upper-bound (variable-upper-bound x))))
    (when (and (variable-lower-bound x) (> (variable-lower-bound x) upper-bound))
      (fail))
    (when (or (not (variable-lower-bound x))
              (not (variable-upper-bound x))
              (>= (/ (- (variable-upper-bound x) upper-bound)
                     (- (variable-upper-bound x) (variable-lower-bound x)))
                  *minimum-shrink-ratio*))
      (local (setf (variable-upper-bound x) upper-bound))
      (cond ((eq (variable-enumerated-domain x) t)
             (when (and (variable-lower-bound x)
                        upper-bound
                        (variable-integer? x)
                        (or (null *maximum-discretization-range*)
                            (<= (- upper-bound (variable-lower-bound x))
                                *maximum-discretization-range*)))
               (set-enumerated-domain!
                x (integers-between (variable-lower-bound x)
                                    upper-bound))))
            ((some #'(lambda (element) (> element upper-bound))
                   (variable-enumerated-domain x))
             ;; NOTE: Could do less consing if had LOCAL DELETE-IF.
             ;;       This would also allow checking list only once.
             (set-enumerated-domain!
              x (remove-if #'(lambda (element) (> element upper-bound))
                           (variable-enumerated-domain x)))))
      (when (and (variable-lower-bound x)
                 (variable-upper-bound x)
                 (zerop (- (variable-upper-bound x) (variable-lower-bound x)));(roughly-= (variable-lower-bound x) (variable-upper-bound x))
				 )
        (local (setf (variable-value x) (variable-lower-bound x))))
      (run-noticers x))))
	  
(defun restrict-bounds! (x lower-bound upper-bound)
  ;; NOTE: X must be a variable.
  ;; NOTE: LOWER-BOUND and UPPER-BOUND must be real constants.
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
                 ;; NOTE: Could do less consing if had LOCAL DELETE-IF.
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
            (when (or (and (numberp domain) (= domain 1))
                      (and (numberp range) (zerop range))) ;<== modified to zerop (phraposo) ;(roughly-= range 0.0)
              (local (setf (variable-value x) ;<== added local (phraposo)
                    (cond ((and enumerated (listp enumerated)) (first enumerated))
                          (lower lower)
                          (t (variable-value x)))))))
          (run-noticers x)))))

; ================================================================================================ ;
;; CHANGES FROM REPMUS LIBRARY (PATCHWORK - OPENMUSIC)
;; SWAPNEILS
;; NEW TESTS (RULES)
; ================================================================================================ ;

(defun +-rule-up (z x y)
(if (and (variable-integer? x) (variable-integer? y)) (restrict-integer! z))
;; note: We can't assert that Z in not an integer when either X or Y are not
;;       integers since they may be Gaussian integers. But we can if either
;;       X or Y is real. If the Screamer type system could distinguish
;;       Gaussian integers from other complex numbers we could whenever X or
;;       Y was not a Gaussian integer.
;;=============================================================================
;; note on this version: If X or Y are not integers, Z may be an integer.
;; Ex.: (= 1 (+ 0.5 0.5)) or (= 1 (+ 1/2 1/2)).
;(if (and (or (variable-noninteger? x) (variable-noninteger? y))
;         (or (variable-real? x) (variable-real? y)))
;    (restrict-noninteger! z))
;; In the case of the combinations (non-integer + integer) or (integer + non-integer)
;; we can assert that Z is not an integer. 
(if (and (or (variable-noninteger? x) (variable-noninteger? y))
         (or (variable-integer? x) (variable-integer? y)))
    (restrict-noninteger! z))
;; =========================================================================
(if (and (variable-real? x) (variable-real? y)) (restrict-real! z))
;; note: Ditto.
(if (and (or (variable-nonreal? x) (variable-nonreal? y))
	   (or (variable-real? x) (variable-real? y)))
  (restrict-nonreal! z))
(if (and (variable-real? x) (variable-real? y) (variable-real? z))
  (restrict-bounds!
   z
   (infinity-+ (variable-lower-bound x) (variable-lower-bound y))
   (infinity-+ (variable-upper-bound x) (variable-upper-bound y))))
(let ((x (value-of x))
	(y (value-of y))
	(z (value-of z)))
(if (and (not (variable? x))
		 (not (variable? y))
		 (not (variable? z))
		 (/= z (+ x y)))
	(fail))))

(defun +-rule-down (z x y)
;; note: We can't assert that X and Y are integers when Z is an integer since
;;       Z may be an integer when X and Y are Gaussian integers. But we can
;;       make such an assertion if either X or Y is "an integer" (original: "is real").
;;	   If the Screamer type system could distinguish Gaussian integers from other
;;       complex numbers we could make such an assertion whenever either X or Y was
;;       not a Gaussian integer.
(if (and (variable-integer? z) (or (variable-integer? x) (variable-integer? y))) ;<== new (swapneils)
;(if (and (variable-integer? z) (or (variable-real? x) (variable-real? y)));<== original from SCREAMER 3.2
(restrict-integer! x))
;; note on this version: In the case of the combinations (non-integer + integer) or (integer + non-integer)
;; we can assert that Z is not an integer. 
(if (and (variable-noninteger? z) (or (variable-integer? x) (variable-integer? y)))
(restrict-noninteger! x))
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

(defun *-rule-up (z x y)
(if (and (variable-integer? x) (variable-integer? y)) (restrict-integer! z))
;; note: We can't assert that Z in not an integer when either X or Y are not
;;       integers since they may be Gaussian integers. But we can if either
;;       X or Y is real. If the Screamer type system could distinguish
;;       Gaussian integers from other complex numbers we could whenever X or
;;       Y was not a Gaussian integer.
;; =========================================================================
;; original note (Repmus): allow Screamer to state that 3/2 * 2/3 is an
;; integer although the operands are not.
;(if (and (or (variable-noninteger? x) (variable-noninteger? y))
;	      (or (variable-real? x) (variable-real? y)))
;    (restrict-noninteger! z))	
;; =========================================================================
(if (and (variable-real? x) (variable-real? y)) (restrict-real! z))
;; note: Ditto.
(if (and (or (variable-nonreal? x) (variable-nonreal? y))
	 (or (variable-real? x) (variable-real? y)))
  (restrict-nonreal! z))
(if (and (variable-real? x) (variable-real? y) (variable-real? z))
  ;; note: Can sometimes do better than the following even when ranges are
  ;;       not finite.
  (restrict-bounds!
   z
   (infinity-min
	(infinity-* (variable-lower-bound x) (variable-lower-bound y))
	(infinity-min
	 (infinity-* (variable-lower-bound x) (variable-upper-bound y))
	 (infinity-min
	  (infinity-* (variable-upper-bound x) (variable-lower-bound y))
	  (infinity-* (variable-upper-bound x) (variable-upper-bound y)))))
   (infinity-max
	(infinity-* (variable-lower-bound x) (variable-lower-bound y))
	(infinity-max
	 (infinity-* (variable-lower-bound x) (variable-upper-bound y))
	 (infinity-max
	  (infinity-* (variable-upper-bound x) (variable-lower-bound y))
	  (infinity-* (variable-upper-bound x) (variable-upper-bound y)))))))
(let ((x (value-of x))
	  (y (value-of y))
	  (z (value-of z)))
  (if (and (not (variable? x))
	   (not (variable? y))
	   (not (variable? z))
	   (/= z (* x y)))
	(fail))))

(defun *-rule-down (z x y)
;; note: We can't assert that X and Y are integers when Z is an integer since
;;       Z may be an integer when X and Y are Gaussian integers. But we can
;;       make such an assertion if either X or Y is "an integer" (original: "is real").
;;       If the Screamer type system could distinguish Gaussian integers from other
;;       complex numbers we could make such an assertion whenever either X or Y was
;;       not a Gaussian integer.
;; =========================================================================
;; note on this version: If Z is an integer and X or Y are integers, we can't assert thar either
;; X or Y are integers. 
;; Ex.: (= 1 (* 4 0.25)) or (= 1 (* 4 1/4)), where Z and X are integers and Y are not.   
;(if (and (variable-integer? z) (or (variable-integer? x) (variable-integer? y)));<== new (swapneils)
;(if (and (variable-integer? z) (or (variable-real? x) (variable-real? y)));<== original from SCREAMER 3.2 (removed in REPMUS)
;(restrict-integer! x));<==(removed in REPMUS) 
;; =========================================================================
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
