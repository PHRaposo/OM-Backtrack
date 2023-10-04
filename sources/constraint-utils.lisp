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
;;; HARMONIC AND MELODIC INTERVALS

(defmethod! harmonic-intervalsv ((domain-list list) (mode string))		  
    :initvals '(nil "+/-")
    :indoc '( "midics" "positive/negative or absolute") 
    :doc "Returns the harmonic intervals (positive/negative or absolute) of a given list on different outputs.
Use > and < to add/remove outputs." 
	:menuins '((1 (("+/-" "+/-") ("abs" "abs")))) 
    :icon 487
	:numouts 2
 (let ((intsv (if (equal mode "+/-") 
	(mapcar #'(lambda (x y) (if (or (null x) (null y)) nil (s::-v y x))) domain-list (cdr domain-list)) 
	(mapcar #'(lambda (x y) (if (or (null x) (null y)) nil (om?::absv (s::-v y x)))) domain-list (cdr domain-list))))) 
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
	(mapcar #'(lambda (x y) (if (or (null x) (null y)) nil (s::-v y x))) domain-list1 domain-list2) 
	(mapcar #'(lambda (x y) (if (or (null x) (null y)) nil (om?::absv (s::-v y x)))) domain-list1 domain-list2)))) 
  (values-list (first-n intsv (length intsv)))))

(defmethod get-boxcallclass-fun ((self (eql 'melodic-intervalsv))) 'OMBoxSplit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;==> IN PROGRESS

(defun parallel? (list1 list2)
 (if (or (null (first list1)) (null (first list2)) 
	     (null (second list1)) (null (second list2))) 
	  nil
 (let ((m1 (s::-v (first list2) (first list1)))
       (m2 (s::-v (second list2) (second list1))))
  (s::orv (s::andv (s::>v m1 0) ;same direction - same intervals
                   (s::>v m2 0)
	      	       (s::=v m1 m2)) 
 	      (s::andv (s::<v m1 0)
 	   	           (s::<v m2 0)
				   (s::=v m1 m2))))))
				   
(defun direct? (list1 list2)
(if (or (null (first list1)) (null (first list2)) 
     (null (second list1)) (null (second list2))) 
  nil
(let ((m1 (s::-v (first list2) (first list1)))
      (m2 (s::-v (second list2) (second list1))))
   (s::orv (s::andv (s::>v m1 0) ;same direction - different intervals
		          (s::>v m2 0)
		          (s::/=v m1 m2)) 
	     (s::andv (s::<v m1 0)
			   (s::<v m2 0)
		          (s::/=v m1 m2))))))
				  
(defun contrary? (list1 list2)
(if (or (null (first list1)) (null (first list2)) 
     (null (second list1)) (null (second list2))) 
  nil
(let ((m1 (s::-v (first list2) (first list1)))
      (m2 (s::-v (second list2) (second list1))))			 
 (s::orv (s::andv (s::<v m1 0) ;opposite directions
			      (s::>v m2 0)) 
		 (s::andv (s::>v m1 0)
		          (s::<v m2 0))))))	
					  
(defun oblique? (list1 list2)
(if (or (null (first list1)) (null (first list2)) 
     (null (second list1)) (null (second list2))) 
  nil
 (let ((m1 (s::-v (first list2) (first list1)))
       (m2 (s::-v (second list2) (second list1))))					  
	(s::orv (s::andv (s::=v m1 0)  ;interval 1 = 0 - interval 2 /= 0
			         (s::/=v m2 0))
			(s::andv (s::/=v m1 0);interval 1 /= 0 - interval 2 = 0
				     (s::=v m2 0))))))

(defun stepwise? (n1 n2)
 (s::<=v (om?::absv (s::-v n2 n1)) 200))

(defun any-step? (list1 list2)
 (s::orv (stepwise? (second list1) (second list2))
           (stepwise? (first list1) (first list2))))

(defun step-upper-voice? (list1 list2)
 (stepwise? (first list1) (first list2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		
;;; CONSTRAINTS

(defmethod! constraint-scale-one-voice ((mode string) (scale list) (voices-list list))		  
    :initvals '("midics" (0 200 400 500 700 900 1100 1200) (0))
    :indoc '( "midics/pcs" "list" "list") 
    :doc "Returns a screamer-score-constraint object." 
	:menuins '((0 (("midics" "midics") ("pcs" "pcs")))) 
    :icon 487

(if (equal mode "pcs")
    (let ((constraint (eval `#'(lambda (x) (s::assert! (?::hard-memberv x ,(reclist-vars (mc->pcv scale))))))))
     (constraint-one-voice constraint  "n-inputs" voices-list "pitch"))

    (let ((constraint  (eval `#'(lambda (x) (s::assert! (?::hard-memberv (mc->pcv x) ,(reclist-vars (mc->pcv scale))))))))
     (constraint-one-voice constraint  "n-inputs" voices-list "pitch"))))

(defmethod! constraint-chords-alldiff-notes ((mode string) (input-mode string) &optional voices-list)		  
    :initvals '("midics" "all-voices" nil)
    :indoc '("midics/pcs" "all-voices/voices-list" "list") 
    :doc "Returns a screamer-score-constraint object." 
	:menuins '((0 (("midics" "midics") ("pcs" "pcs")))
                         (1 (("all-voices" "all-voices") ("voices-list" "voices-list")))) 
    :icon 487
    (let ((constraint (if (equal mode "midics")
                                 (eval ` #'(lambda (x) (om?::assert!-all-differentv (remove nil x)))) 
                                 (eval `#'(lambda (x) (om?::assert!-all-differentv (mc->pcv (remove nil x))))))))
    (if (equal input-mode "all-voices")
       (constraint-harmony constraint  "n-inputs" "all-voices" "all")
       (constraint-harmony constraint  "n-inputs" "voices-list" "all" voices-list))))

(defmethod! no-crossing-voices ((input-mode string) (unison? string) &optional voices-list)	  
    :initvals '("all-voices" "no" nil)
    :indoc '( "all-voices/voices-list" "yes/no" "list") 
    :doc "Returns a screamer-score-constraint object." 
    :menuins '((0 (("all-voices" "all-voices") ("voices-list" "voices-list")))
                         (1 (("no" "no") ("yes" "yes")))) 
    :icon 487
    (let ((constraint (if (equal unison? "no")
                                 (eval `#'(lambda (x) (om?::assert!-apply-rec #'(lambda (y z) (s::>v y z)) x))) 
                                 (eval `#'(lambda (x) (om?::assert!-apply-rec #'(lambda (y z) (s::>=v y z)) x))))))
                                     
    (if (equal input-mode "all-voices")
       (constraint-harmony constraint  "n-inputs" "all-voices" "all")
       (constraint-harmony constraint  "n-inputs" "voices-list" "all" voices-list))))

(defmethod! not-parallel-fifths-octaves ((voices-list list))		  
    :initvals '( ((0 1) (0 2)) )
    :indoc '("list") 
    :doc "Returns a screamer-score-constraint object." 
    :icon 487
    (let ((constraint
             (eval `#'(lambda (x y)
             (if (or (some #'null x) (some #'null y)) 
                  nil
               (let ((interval1  (s::funcallv #'mod (om?::absv (s::-v (first x) (second x))) 1200))
                     (interval2 (s::funcallv #'mod (om?::absv (s::-v (first y) (second y))) 1200)))
                      
             (s::assert! 
              (?::ifv (parallel? x y)
                   (s::orv (s::notv (s::memberv interval1 '(0 700)))
                                        (s::notv (s::memberv interval2 '(0 700))))

               t)
              )
             )
            )
           )
          )
        )
      )
   (constraint-harmony constraint  "n-inputs" "voices-list" "all" voices-list)))