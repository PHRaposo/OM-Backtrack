(in-package :om)

(defmethod! contain-variables? ((domain-list list))		  
    :initvals '((6000 nil nil (s::an-integerv)))
    :indoc '( "domain-list") 
    :doc "
First output: Returns t if the list containts any screamer variable and nil if it is an empty list (nil) or if contains only numbers.
Second output: Returns all screamer variables if the first output is true and nil otherwise."                    
    :icon 487
	:numouts 2
 (let ((screamer-variables (s::variables-in domain-list)))
  (values (not (null screamer-variables))
          (if screamer-variables
		 	 (if (list-of-listp domain-list)
	             (group-list screamer-variables (mapcar #'length domain-list) 'linear)
		      screamer-variables)
		  nil))
  ))

(defmethod! contain-rests? ((domain-list list))		  
    :initvals '((6000 nil nil (s::an-integerv)))
    :indoc '( "domain-list") 
    :doc "Returns t if the list containts any rests (represented as null value <nil> in the screamer-score domains)."                    
    :icon 487
(not (null (position 'nil domain-list))))
 
(defmethod! pcset-equalv ((domain-list list) (pcset list))		  
    :initvals '((6000 6400 6700) (0 4 7))
    :indoc '( "midics" "pcset list") 
    :doc "Returns t if a list of midics <input1> containts all the pitch-classes in pcset list <input2>."  
    :icon 487
(all-membersv (mc->pcv domain-list) pcset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;COUNTERPOINT ==> IN PROGRESS

(defmethod! harmonic-intervalsv ((domain-list list) (mode string))		  
    :initvals '(nil "+/-")
    :indoc '( "midics" "positive/negative or absolute") 
    :doc "Returns the harmonic intervals (positive/negative or absolute) of a given list on different outputs.
Use > and < to add/remove outputs." 
	:menuins '((1 (("+/-" "+/-") ("abs" "abs")))) 
    :icon 487
	:numouts 2
 (let ((intsv (if (equal mode "+/-") 
	(mapcar #'(lambda (x y) (?::ifv (s::orv (null x) (null y)) nil (s::-v y x))) domain-list (cdr domain-list)) 
	(mapcar #'(lambda (x y) (?::ifv (s::orv (null x) (null y)) nil (om?::absv (s::-v y x)))) domain-list (cdr domain-list))))) 
  (values-list (first-n intsv (length intsv)))))

(defmethod get-boxcallclass-fun ((self (eql 'harmonic-intervalsv))) 'OMBoxSplit)

(defmethod! melodic-intervalsv ((domain-list1 list) (domain-list2 list) (mode string))		  
    :initvals '(nil nil "+/-")
    :indoc '( "midics" "positive/negative or absolute") 
    :doc "Returns the melodic intervals (positive/negative or absolute) between two lists on different outputs.
Use > and < to add/remove outputs." 
	:menuins '((1 (("+/-" "+/-") ("abs" "abs")))) 
    :icon 487
	:numouts 2
 (let ((intsv (if (equal mode "+/-") 
	(mapcar #'(lambda (x y) (?::ifv (s::orv (null x) (null y)) nil (s::-v y x))) domain-list1 domain-list2) 
	(mapcar #'(lambda (x y) (?::ifv (s::orv (null x) (null y)) nil (om?::absv (s::-v y x)))) domain-list1 domain-list2)))) 
  (values-list (first-n intsv (length intsv)))))

(defmethod get-boxcallclass-fun ((self (eql 'melodic-intervalsv))) 'OMBoxSplit)


;MELODIC AND HARMONIC INTERVALS  - VOICES MOTIONS (OBLIQUE, DIRECT, CONTRARY AND PARALLEL)

(defun voices-mel-har-mot (domain-list1 domain-list2) 	  
 (let* ((voices-combinations-posn (om?::asc-permutations (arithm-ser 0 (1- (length domain-list1)) 1) 2))
       (voices-combinations (mapcar #'(lambda (x) (posn-match x voices-combinations-posn)) (list domain-list1 domain-list2)))
       (melodic-intervals (mat-trans (posn-match (mapcar #'(lambda (x y)(?::ifv (s::orv (null x) (null y)) nil (s::-v y x))) domain-list1 domain-list2) voices-combinations-posn)))
      (jumps? (mapcar #'(lambda (x y)
                                                 (?::ifv (s::orv (null x) (null y)) nil
                                                 (?::ifv (s::>v (om?::absv (s::-v y x)) 200) "jump" nil))) 
                                 domain-list1 domain-list2))
	   (harmonic-intervals (mapcar #'(lambda (x)
	                                  (mapcar #'(lambda (y)
									            (?::ifv (s::orv (null (first y)) (null (second y))) nil 
										         (om?::absv (s::-v (first y) (second y))))) 
									   x))
						    voices-combinations))
	  (voices-motions (mapcar #'(lambda (m1 m2)
								 (cond ((s::orv (null m1) (null m2)) "rest")
	 
									   ((s::orv (s::andv (s::=v m1 0)
									                             (s::/=v m2 0)) 
										       (s::andv (s::/=v m1 0)
											 	     (s::=v m2 0))) "oblique")
						 
								          ((s::orv (s::andv (s::<v m1 0)
									                            (s::>v m2 0)) 
										      (s::andv (s::>v m1 0)
											 	    (s::<v m2 0))) "contrary")	
							 
								         ((s::orv (s::andv (s::>v m1 0)
								                                   (s::>v m2 0)
											      	   (s::/=v m1 m2)) 
									 	     (s::andv (s::<v m1 0)
									 	   	           (s::<v m2 0)
												   (s::/=v m1 m2))) "direct")

								        ((s::orv (s::andv (s::>v m1 0)
								                                  (s::>v m2 0)
									      	               	  (s::=v m1 m2)) 
								 	            (s::andv (s::<v m1 0)
								 	   	                  (s::<v m2 0)
												  (s::=v m1 m2))) "parallel")
									  (t t)))
					(first melodic-intervals) (second melodic-intervals)))
              (outer-voices-posn (nth (- (length domain-list1) 2) voices-combinations-posn)))
               ;voice-count)

;(setf voice-count 0)

#|
(mapcar #'(lambda (h1 h2 mot)
 (s::assert!
  (?::ifv (s::orv (s::=v (mc->pcv h2) 7) (s::=v (mc->pcv h2) 0))
            (s::orv (s::equalv mot "contrary")  (s::equalv mot "oblique"))))
|#
 
 (list melodic-intervals harmonic-intervals voices-motions jumps? outer-voices-posn))) 	  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MELODIC-LINE-INTERVALS ;==> IN PROGRESS ADAPT TO SCREAMER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(defun melodic-line-intervals (melody allowed-mel-ints) ;;;R-PITCHES-ONE-VOICE :ALL-PITCHES
(let* ((asc-or-desc-lines (get-asc-desc-lines melody))
      (ascending (split-ascending (first asc-or-desc-lines)))
      (descending (split-descending (second asc-or-desc-lines)))
      (all-intervals (mapcar #'get-melodic-intervals (om::x-append ascending descending))))
(loop for interval in all-intervals
       always (member interval allowed-mel-ints)))) 

(defun get-asc-desc-lines (notes)
(let ((all-positions
(loop for n from 0 to (- (length notes) 2)
 if (> (nth n notes) (nth (1+ n) notes))
    collect (x-append n (1+ n)) into descending
 else 
    collect (x-append n (1+ n)) into ascending
finally (return (x-append (list (flat ascending)) 
                                       (list (flat descending)))))))
(posn-match notes 
                        (mapcar #'remove-duplicates all-positions))))

(defun split-ascending (notes)
(om::group-list 
notes 
(om::x->dx 
(om::x-append 
0  
(loop for n from 0 to (- (length notes) 2)
 if (> (nth n notes) (nth (1+ n) notes))
   collect (1+ n))
(length notes)))
'linear))

(defun split-descending (notes)
(om::group-list 
notes 
(om::x->dx 
(om::x-append 
0  
(loop for n from 0 to (- (length notes) 2)
 if (< (nth n notes) (nth (1+ n) notes))
   collect (1+ n))
(length notes)))
'linear))

(defun get-melodic-intervals (asc-or-desc-line)
 (abs (- (first asc-or-desc-line) (om::last-elem asc-or-desc-line))))	
|#					 
