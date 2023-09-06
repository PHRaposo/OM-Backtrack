(IN-PACKAGE :om-screamer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FROM T2L-SCREAMER

"Copyright (c) 2007, Kilian Sprotte. All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

  * Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.

  * Redistributions in binary form must reproduce the above
    copyright notice, this list of conditions and the following
    disclaimer in the documentation and/or other materials
    provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE."

(defmacro n-values (n
		    &body forms)
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

(defun funcallv-rec (fn tree)
 (cond ((equalv nil tree) nil)
       ((funcallv #'consp tree) 
        (cons (funcallv-rec fn (funcallv #'car tree))
              (funcallv-rec fn (funcallv #'cdr tree))))
    (t (funcallv fn tree))))

(defun modv-alt (n d) 
 (let ((x (an-integerv)))     
   (assert! (>=v x (minv 0 (+v d 1))))
   (assert! (<=v x (maxv 0 (-v d 1))))
   (assert! (=v x (-v n (*v d (an-integerv)))))
   x))

(defun %v (n d) (modv n d))

(defun powv (a b) (funcallv #'pow a b))

(cl:defun nsucc (input n &key step list-padding pad-character)
  (cond
   ((null step) (nsucc input n :step (1- n) :list-padding list-padding :pad-character pad-character))
   (t
    (let* ((list (if list-padding 
                     (append input
                             (make-sequence 'list (* -1 (- (length input) 
                                                           (* n (ceiling (/ (length input) n))))) :initial-element pad-character))
                   input))
           (length (length list)))
      (loop for i from 0
            for j = (* i step)
            for k = (+ j n)
            while (< j (- (length list) step))
            collect (subseq list j (if (<= k length) k length)))))))
			
(cl:defun reduce-chunks (fn input &key default)
  (cond
   ((null input) default)
   ((not (listp input)) (reduce-chunks fn (list input) :default default))
   ((>= (length input) call-arguments-limit) 
    (reduce fn (mapcar #'(lambda (chunk) (apply fn chunk)) 
                       (nsucc input call-arguments-limit :step call-arguments-limit))))
   (t (apply fn input))))

(defun sumv (list)
  (reduce-chunks #'+v list :default 0))

#|
(defun all-membersv (e sequence)
  (let ((sequence-flat (om::flat sequence)))
   (cond ((listp e) (reduce-chunks #'andv (mapcar #'(lambda (x) (memberv x sequence-flat)) (om::flat e))))
          (t (memberv e sequence-flat)))))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OM-SCREAMER   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VARIABLES GENERATORS

(defun list-of-members-ofv (n dom)
  (if (zerop n) nil
      (cons (a-member-ofv dom)
            (list-of-members-ofv (1- n) dom))))

(defun list-of-integers-betweenv (n min max)
  (if (zerop n) nil
      (cons (an-integer-betweenv min max)
            (list-of-integers-betweenv (1- n) min max))))

(defun list-of-integers-abovev (n min)
  (if (zerop n) nil
      (cons (an-integer-abovev min)
            (list-of-integers-abovev (1- n) min))))

(defun list-of-integers-belowv (n max)
  (if (zerop n) nil
      (cons (an-integer-belowv max)
            (list-of-integers-belowv (1- n) max))))

(defun list-of-booleansv (n)
  (if (zerop n) nil
      (cons (a-booleanv)
            (list-of-booleansv (1- n)))))

(defun list-of-integersv (n)
  (if (zerop n) nil
      (cons (an-integerv)
            (list-of-integersv (1- n)))))

(defun list-of-realsv (n)
  (if (zerop n) nil
      (cons (a-realv)
            (list-of-realsv (1- n)))))

(defun list-of-reals-betweenv (n min max)
  (if (zerop n) nil
      (cons (a-real-betweenv min max)
            (list-of-reals-betweenv (1- n) min max))))

(defun list-of-reals-abovev (n min)
  (if (zerop n) nil
      (cons (a-real-abovev min)
            (list-of-reals-abovev (1- n) min))))

(defun list-of-reals-belowv (n max)
  (if (zerop n) nil
      (cons (a-real-belowv  max)
            (list-of-reals-belowv  (1- n) max))))

(defun list-of-numbersv (n)
  (if (zerop n) nil
      (cons (a-numberv)
            (list-of-numbersv (1- n)))))

(defun list-of-chords-inv (lst1 lst2 &optional random?)
 (let ((v (mapcar #'(lambda (x)
             (if random? (list-of-random-members-ofv x lst2) (list-of-members-ofv x (reverse lst2)))) 
            lst1)))
 (mapcar #'(lambda (x) (all-ascendingv x)) v)
 (mapcar #'assert!-all-differentv v)
(value-of v)))

#| ;TOO SLOW
(defun list-of-chords-inv-domains (lst1 lst2 &optional random?)
 (let ((v (if random?
             (mapcar #'(lambda (x) (list-of-random-members-ofv (length lst1) x)) lst2) 
             (mapcar #'(lambda (x) (list-of-members-ofv (length lst1) x)) lst2)))) 
 (mapcar #'(lambda (x) (all-ascendingv x)) v)
 (mapcar #'assert!-all-differentv v)
(value-of v)))
|#

(defun list-of-random-members-ofv (n dom)
  (if (zerop n) nil
      (cons (a-random-member-ofv dom)
            (list-of-random-members-ofv (1- n) dom))))

(defun list-of-integers-modv (n d)
  (if (zerop n) nil
      (cons (an-integer-modv (an-integerv) d)
            (list-of-integers-modv (1- n) d))))

(defun list-of-mcs->pcsv (mcs)
(mapcar #'a-mc->pcv mcs))

(defun random-test (list)
  (nthv (funcallv #'random (1- (length list))) list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NEW-VARIABLES 
	
(defun a-random-member-of (values)
    (a-member-of (om::permut-random values)))

(defun a-random-member-ofv (values &optional (name nil name?))
  (let ((v (if name? (make-variable name) (make-variable))))
    (assert! (memberv v (om::permut-random values)))
   (value-of v)))

(defun a-real-multiple-ofv (n m-max)
 (let ((v (a-realv))
        (all-multiples (reverse (all-values (*v (an-integer-between 1 m-max) n)))))
(assert! (memberv v all-multiples))
(value-of v)))

(defun a-multiple-of (n1 n2)
(integerpv (/v n1 n2))) 

(defun an-integer-modv (n d) 
 (let ((x (an-integerv)))
   (assert! (andv (>=v x 0) (<v x d)))    
   (assert! (=v x (-v n (*v d (an-integerv)))))  
  x))

(defun an-integer-roundv (n) ;&optional (d 1)) 
  (let ((x (an-integer-betweenv (-v (-v n 0.5) 1e-6) (-v (+v n 0.5) 1e-6)))) ;(an-integer-betweenv (-v (-v (/v n d) 0.5) 1e-6) (-v (+v (/v n d) 0.5) 1e-6)))
         ;(rem-x (a-realv))) ;;;FIX-ME ===> REMAINDER (NEGATIVE-NUMBERS)
  (value-of x)))
  ;(assert! (=v rem-x (-v (absv n) (*v x d))))		
  ; (values (value-of x) (value-of rem-x))))

; N = (* D X + REM-X)
; REM-X = N - (* D X)

(defun member-of-pcv? (var pc-list)
 (let ((pcv (om::mc->pcv var)))
  (assert! (memberv pcv pc-list))
 (memberv var (all-values (solution pcv (static-ordering #'linear-force))))))
    
(defun a-mc->pcv (n)
 (let ((x (an-integerv)))
  (assert! (=v x (/v (an-integer-modv n 1200) 100)))
(value-of x)))

(defun first-nv (list n)
  (ifv (<v (lengthv list) n) list)
   (funcallv #'butlast list (-v (lengthv list) n)))

(defun last-nv (list n)
  (funcallv #'last list n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NEW FUNCTIONS 

(defun assert!-all-differentv (list)
   (labels ((all-different (x xs)
              (if (null xs)
                  t
                  (andv (assert! (notv (memberv x xs)))
                        (all-different (car xs) (cdr xs))))))
     (all-different (car list) (cdr list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SCREAMER ALL-ROTATIONS / SMAT-TRANS / MODV

(defun all-rotations-internal (list accumul)
 (let ((rotation (if accumul (om::x-append (cdr (first accumul)) (first (first accumul)))
                                         (om::x-append (cdr list) (first list)))))
(if (equal (first list) (first rotation))
    (om::x-append (list rotation) (reverse accumul))
    (all-rotations-internal list (om::x-append (list rotation) accumul)))))

(defun all-rotations (list)
 (all-rotations-internal list nil))

(defun smat-trans (lists) ;MAT-TRANS WITHOUT LOOP
(let* ((maxl (apply #'max (mapcar #'length lists)))
       (nths (all-values (an-integer-between 0 (1- maxl)))))
(mapcar #'(lambda (nth) (nth-of-lists nth lists)) nths)))

(defun nth-of-lists (n lists)
 (mapcar #'(lambda (lst) (nth n lst)) lists))

(defun spermut-random (list)
 (all-values (a-random-member-of list)))

(defun n-random-members (list n)
 (n-values n (a-random-member-of list)))

(defun spermutations (list)
(let ((var-list (list-of-members-ofv (length list) (reverse list))))
(assert!-all-differentv var-list)
 (all-values (solution var-list (static-ordering #'linear-force)))))
 
(defun asc-permutations (list n)
 (let ((var-list (list-of-members-ofv n (reverse list))))
 (assert!-all-differentv var-list)
 (assert! (apply #'<v var-list))
  (all-values (solution var-list (static-ordering #'linear-force)))))

(defun scombinations (list)
(let ((var-list (list-of-members-ofv (length list) (reverse list))))
 (all-values (solution var-list (static-ordering #'linear-force)))))

(defun closest-midic (note midics-domain)
(let* ((mcv (a-member-ofv midics-domain)))
  (let ((intervals (all-values 
                          (solution (om?::absv (-v note mcv)) 
                          (static-ordering #'linear-force)))))
(assert! (orv (=v mcv (+v note (om::list-min intervals)))
                    (=v mcv (-v note (om::list-min intervals)))))
(one-value (solution mcv (static-ordering #'linear-force))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SCREAMER-CONSTRAINTS

;;;FORMAT CONSTRAINTS 
;;; (DEFUN <NAME> (<VAR-LIST> &OPTIONAL <ARGS>)
;;;  (ASSERT!-APPLY-REC #'(LAMBDA (<INPUT1 INPUT2 ... INPUT-N>) 
;;;  (SIMPLE-CONSTRAINT-FN <INPUT1 INPUT2 ... INPUT-N> <ARGS>)) <VAR-LIST>))

(defun assert!-apply-rec (fn list)
 (let* ((fn-inputs (length (om::function-lambda-list fn)))
        (list-inputs (all-values (an-integer-between 0 (1- fn-inputs))))) 
  (labels ((app-rec (x)
           (if (null (nth (1- fn-inputs) x))
               t
               (andv (assert! (apply fn (mapcar #'(lambda (n) 
			  	                         (nth n x)) list-inputs)))
                     (app-rec (cdr x))))))
  (app-rec list)
 )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CONSTRAINTS FOR PITCHES - ONE MELODIC LINE (ASSERT! IS NOT NEEDED)

(defun all-ascendingv (var-list)
 (assert!-apply-rec #'(lambda (x y) (all-asc x y)) var-list)) 

(defun all-asc (var1 var2)
 (>v (-v var2 var1) 0))
	  
(defun all-descendingv (var-list)
 (assert!-apply-rec #'(lambda (x y) (all-desc x y)) var-list)) 

(defun all-desc (var1 var2)
 (<v (-v var2 var1) 0))

(defun allowed-melodic-intervals (var-list int-list)
 (assert!-apply-rec #'(lambda (x y) (al-mel-ints x y int-list)) var-list)) 

(defun al-mel-ints (var1 var2 list)
 (memberv (absv (-v var2 var1)) list))

(defun not-allowed-melodic-intervals (var-list int-list)
 (assert!-apply-rec #'(lambda (x y) (not-al-mel-ints x y int-list)) var-list)) 

(defun not-al-mel-ints (var1 var2 list)
 (notv (memberv (absv (-v var2 var1)) list)))

(defun no-repeated-melodic-intervals (var-list)
 (assert!-apply-rec #'(lambda (x y z) (no-repeat-mel-ints x y z)) var-list)) 

(defun no-repeat-mel-ints (var1 var2 var3)
 (/=v (-v var2 var1) (-v var3 var2)))

(defun ballistic? (var-list) ;;;
" < From PW-CONSTRAINTS by Mikael Laurson >
A ballistic movement allows two jumps in the same direction, 
but the larger jump has to be below the smaller one."
 (assert!-apply-rec #'(lambda (x y z) (blstc? x y z)) var-list)) 

(defun blstc? (midi1 midi2 midi3) 
" midi1, midi2 and midi3 should form a 'ballistic' melodic movement.
A ballistic movement allows two jumps in the same direction, 
but the larger jump has to be below the smaller one."
  (let* ((up (<v midi1 midi2 midi3))
         (down (>v midi1 midi2 midi3))
         (mel-diff1 (absv (-v midi1 midi2)))
         (mel-diff2 (absv (-v midi2 midi3)))
         (jump-case? (orv (>v mel-diff1 2) (>v mel-diff2 2)))
         (same-direction? (orv up down)))
    (ifv (and jump-case? same-direction?)
       (ifv up 
           (>=v  mel-diff1 mel-diff2)
           (<=v  mel-diff1 mel-diff2))
        t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CONSTRAINTS FOR CHORDS (LIST OF VARIABLES)

(defun symm? (var-list)
 (let ((int-mod12v (om::mod12v (x->dxv var-list))))
(assert! (equalv int-mod12v (reverse int-mod12v)))))

  
