;;;   -*-  package: user   mode: COMMON-LISP -*-
;;; ************************************************************************
;;; 
;;; --- Configuration Information ---
;;; System:             Screamer
;;; Config Item:        
;;; Component:          
;;; Filename:           retract.lisp
;;; Revision:           
;;; Person Responsible: Buddy Kresge
;;;                     kresge@ggt.gsi.com
;;; Status:             preliminary
;;; Environment:        Lucid Common Lisp 
;;;                     running on an HP9000 series 700 computer
;;;
;;;
;;; ************************************************************************
;;; File Description: 
;;;   This files adds the following extensions to screamer.
;;;   
;;;      Retractability
;;;         The ability to retract assertions made on screamer
;;;         variables.  
;;;
;;;   The retraction capabilities are built on top of screamer.  This
;;;   was done so that further releases of screamer can be utilized
;;;   as is with no modifications.  The ability to retract assertions
;;;   is accomplished by the top-level functions:
;;;        RASSERT!  (stands for Retractible ASSERT!)
;;;        RETRACT!
;;;
;;;   RASSERT! acts much like ASSERT! except that it does some
;;;   preprocessing so as to allow the assertion to later be
;;;   retracted.  IT IS NOT POSSIBLE to retract an assertion asserted
;;;   with ASSERT!.  Obviously, RETRACT! removes the effects of any
;;;   previous RASSERT!-tion, and undoes the preprocessing that
;;;   RASSERT! did.
;;; ************************************************************************
;;; Revision History:
;;;
;;; Sep 27 1993  Buddy Kresge (kresge@ggt.gsi.com)
;;;    Original.  
;;;
;;; Jul 10 2023 Paulo Raposo (depends on :iterate)
;;; 

(in-package :screamer)

(export '(
	  rassert!
	  retract!
	  ))

;;; ************************************************************************
;;; Section 0.0  Declaration, manipulation, etc of hash tables
;;;              *rassert-HT* and *variable-in-assertions*
;;; ************************************************************************

(defun reset-hash-tables ()
  (clrhash screamer::*noticers-ht*)
  (clrhash screamer::*rassertions-ht*))

;;; ************************************************************************
;;; When we rassert! a new form we store an alist, of variables in the
;;; form and the newly added noticers that were added to the variable's
;;; 'noticers' slot, in hash table *noticers-HT*.  The key to the hash table
;;; is the form supplied as the argument to rassert!.
;;; ************************************************************************

(defvar *noticers-HT* (make-hash-table :test #'equal))

(defun get-noticers (form)
  "Gets all noticers that were added by asserting form."
  (declare (special *rassert-HT*))
  (gethash form *noticers-HT*))

(defun record-noticers (form noticer-a-list)
  "Records all noticers that were added by asserting form."
  (declare (special *noticers-HT*))
  (setf (gethash form *noticers-HT*) noticer-a-list))

(defun remove-noticers-entry (form)
  "Removes the hash table entry indexed by form."
  (declare (special *noticers-HT*))
  (remhash form *noticers-HT*))



;;; ************************************************************************
;;; For each variable in the form asserted by rassert!, we record the
;;; assertion in *rassertions-HT* (indexed by the variable).  The reason
;;; is that we need to know all forms that a variable occurs in.
;;; ************************************************************************

(defvar *rassertions-HT* (make-hash-table))

(defun get-rassertions (var)
  "Gets all assertions that the variable is contained in."
  (declare (special *rassertions-HT*))
  (gethash var *rassertions-HT*))

(defun record-rassertion (var form)
  "Adds new form that variable appears in into the hash table."
  (declare (special *rassertions-HT*))
  (pushnew form (gethash var *rassertions-HT*) :test #'equal))

(defun remove-rassertion (var form)
  "Removes a form that variable appears in fro the hash table."
  (declare (special *rassertions-HT*))
  (let ((rassertions (gethash var *rassertions-HT*)))
    (if (equal form (car rassertions))
	(setf (gethash var *rassertions-HT*)
	      (cdr (gethash var *rassertions-HT*)))
      (delete form (gethash var *rassertions-HT*) 
	      :test #'equal))
    ))
	      
(defun remove-rassertions-entry (var)
  "Removes the hash table entry indexed by var."
  (declare (special *rassertions-HT*))
  (remhash var *rassertions-HT*))


;;; ************************************************************************
;;; Section 1.0  Misc./Support functions
;;; ************************************************************************

(defun delete-noticer (var noticer)
  "Removes the noticer from the variables 'noticers' slot."
  (if (eq noticer (car (variable-noticers var)))
      (setf (variable-noticers var) (cdr (variable-noticers var)))
    (delete noticer (variable-noticers var))))


(defun rerun-noticers (var)
  "Runs all the noticers for a given variable."
  (let ((script (reverse (variable-noticers var))))
  (iterate::iter (iterate::for noticer in script) 
   (iterate::collect (funcall noticer))))) 
	;(loop for noticer in script do
	;  (funcall noticer))))

(defun reset-variable (var)
  "Resets certain fields of a variable."
  (setf (variable-enumerated-domain var) t)
  (setf (variable-value var) var)
  (setf (variable-lower-bound var) nil)
  (setf (variable-upper-bound var) nil)
  t
)


(defun delete-noticers (var noticers)
  "Given a list of noticers for a variable, delete them."
  (iterate::iter (iterate::for noticer in noticers) 
   (iterate::collect (delete-noticer var noticer))))   
  ;(loop for noticer in noticers do
  ;	(delete-noticer var noticer))
;)

(defun extract-variables (form)
  "Given a form, returns a list of all screamer variables."
  (iterate::iter (iterate::for x in form)  
   (iterate::appending 
   	(cond ((consp x) (extract-variables x))
   	      ((and (not (constantp x))
   		    (boundp x)
   		    (variable? (symbol-value x)))
   	       (list (symbol-value x)))))))
		   
  ;(loop for x in form
  ;	append
  ;	(cond ((consp x) (extract-variables x))
  ;	      ((and (not (constantp x))
  ;		    (boundp x)
  ;		    (variable? (symbol-value x)))
  ;	       (list (symbol-value x))))))



;;; ************************************************************************
;;; Section 2.0  Retraction of previously 'rassert!-ed' forms
;;; ************************************************************************

;;; ************************************************************************
;;; Retraction Algorithm:
;;;   
;;;   1. For each variable in the form
;;;        a. Reset the variable
;;;        b. Remove the noticers that form added from the
;;;            'noticers' slot of the variable.
;;;        c. Remove the form associated with the variable
;;;            from *rassertions-ht*.
;;;
;;;   2. Unwind the effects of the assertions
;;;        a. Get all assertions that are affected by retraction
;;;        b. For each affected assertion
;;;            1. perform steps 1a and 1b.
;;;            2. remove the entry in *noticers-ht* for the 
;;;               affected assertion.
;;;        c. For each variable in all the affected forms
;;;            rerun all remaining noticers.
;;;        d. For each affected assertion
;;;            RASSERT! it again.
;;;
;;;   3. Remove the entry for the assertion from *noticers-ht*.
;;;
;;; ************************************************************************


(defun get-affected-forms-via-variables (vars forms vars-visited)
  "Given a list of variables, get all the forms that a
   change in any of the variables would effect."
  (cond ((endp vars) forms)
	((find (car vars) vars-visited)
	 (get-affected-forms-via-variables (cdr vars) 
					   forms
					   vars-visited))
	(t
	 (let* ((new-forms (reverse (get-rassertions (car vars))))
		    (new-vars (iterate::iter (iterate::for var in (extract-variables new-forms)) 
			           (when (not (find var vars-visited))
				        (iterate::collect var))))
		     )
			 ;(loop for var in (extract-variables new-forms)
					;		 when (not (find var vars-visited))
					;		 collect var))
					;)				
	   (get-affected-forms-via-variables (append (cdr vars) new-vars)
					     (delete-duplicates 
					      (append forms new-forms)
					      :test #'equal)
					     (cons (car vars)
						   vars-visited))))
	))



(defun get-affected-forms (form)
  "Given an assertions, find all other assertions this could affect."
  (let ((variables (extract-variables form)))
    (get-affected-forms-via-variables variables nil nil)
    ))


(defun retract (form)
  "Given an assertion form, reset all the variables in that form and
   remove the noticers that were created from each variable in form."
  (let ((noticer-a-list (get-noticers form))
	 )
     (iterate::iter (iterate::for var-noticers-pair in noticer-a-list)
	 (reset-variable (car var-noticers-pair)))
	 ;(loop for var-noticers-pair in noticer-a-list do
	  ; (reset-variable (car var-noticers-pair)))
     (iterate::iter (iterate::for var-noticers-pair in noticer-a-list)
 	 (delete-noticers (car var-noticers-pair)
	 	  	  (cdr var-noticers-pair)))
     ;(loop for var-noticers-pair in noticer-a-list do
	 ; (delete-noticers (car var-noticers-pair)
	 ;		    (cdr var-noticers-pair)))
     ))


(defun unwind-form-effects (form)
  "Given a form, undo all the affects of asserting that form."
  (let* ((affected-forms (get-affected-forms form))
	     (variables (delete-duplicates
		     (append (extract-variables form)
			     (extract-variables affected-forms))))
	 )
	 (iterate::iter (iterate::for a-form in affected-forms)
       (progn
        (retract a-form)
        (remove-noticers-entry a-form)))
	;(loop for a-form in affected-forms do
	;  (progn
	;    (retract a-form)
	;    (remove-noticers-entry a-form)))
	(iterate::iter (iterate::for variable in variables)
	  (rerun-noticers variable))	
    ;(loop for variable in variables do
	;  (rerun-noticers variable))
	 (iterate::iter (iterate::for a-form in affected-forms)
     (let ((rassert-form `(rassert! ,a-form)))
       (eval rassert-form)))
    ;(loop for a-form in affected-forms do
	;  (let ((rassert-form `(rassert! ,a-form)))
	;    (eval rassert-form)))
    ))

(defmacro retract! (form)
  "Top-Level function for retraction."
  `(let ((variables (extract-variables ,form)) ;(quote ,form)))
	 ) 	 
	 (iterate::iter (iterate::for var in variables) 
  	   (unless (member ,form (get-rassertions var) :test #'equal)
  	 	   (error "~s not found in assertions made ~S~%"
  	 		  ,form ;(quote ,form)
  	 		  (get-rassertions var))))			  
	 ;(loop for var in variables do
	 ;  (unless (member form (get-rassertions var) :test #'equal)
	; 	   (error "~s not found in assertions made ~S~%"
	; 		  (quote ,form)
	; 		  (get-rassertions var))))
     (retract ,form) ;(quote ,form))
	 (iterate::iter (iterate::for var in variables)
	 (remove-rassertion var ,form)) ;(quote ,form))
     ;(loop for var in variables do
	 ;  (remove-rassertion var (quote ,form)))
     (unwind-form-effects ,form) ;(quote ,form))
     (remove-noticers-entry ,form)) ;(quote ,form))
     )
	 

;;; ************************************************************************
;;; Section 3.0  Rassert!  (Retractible ASSERT!)
;;; ************************************************************************

;;; ************************************************************************
;;; RAssertion Algorithm:
;;;   1. Get all variables in the form 
;;;   2. Get all noticers for each variable
;;;   3. ASSERT! the form
;;;   4. Get the new noticers that the form asserted
;;;   5. For each variable in form, store the assertion for the
;;;      variable in *rassertions-HT*.
;;;   6. Store the new noticers, added to each variables 'noticers'
;;;      slot, in *noticers-HT*.
;;;
;;; ************************************************************************

(defmacro rassert! (form)
  `(let* ((variables (extract-variables (quote ,form)))
	 (orig-noticers
	     (iterate::iter (iterate::for v in variables) 
	      (iterate::collect (cons v 
			      (variable-noticers v))))) 
	  ;(loop for v in variables
	  ;	collect (cons v 
	  ;		      (variable-noticers v))))
	 (ret-val (assert! ,form))
	 (new-noticers 
	     (iterate::iter (iterate::for v in variables) 
	      (iterate::collect (cons v
			      (set-difference
			       (variable-noticers v)
			       (cdr (assoc v orig-noticers))))))) 
	 	  ;(loop for v in variables
	 	  ;	collect (cons v
	 	  ;		      (set-difference
	 	  ;		       (variable-noticers v)
	 	  ;		       (cdr (assoc v orig-noticers))))))
	 )
	 (iterate::iter (iterate::for variable in variables)
	   (record-rassertion variable (quote ,form))) 
     ;(loop for variable in variables do
	 ;  (record-rassertion variable (quote ,form)))
     (record-noticers (quote ,form) new-noticers)
     ret-val))
	 
#|	 
(defun test1 ()
(setq var (an-integer-betweenv 0 10))
 (iterate::iter (iterate::for x from 0 to 9) 
	   (progn (rassert!(>v var x)) (print var) (retract! '(>v var x))))
var)

(defun triplesv-test2 (n)
 (setq a (an-integer-betweenv 1 n)) ;==> rassert!->retract! do not work with let bindings ???
 (setq b (an-integer-betweenv 1 n))
 (setq c (an-integer-betweenv 1 n))
 (rassert! (=v (+v (*v a a) (*v b b)) (*v c c)))
 (print 
  (format nil "SOLUTION: ~A" 
   (all-values
    (solution
     (list a b c)
      (reorder #'range-size
               #'(lambda (x) (< x 1e-6))
               #'>
               #'divide-and-conquer-force)))))			  
 (print (format nil "RETRACT! SUCCEED?: ~A" (retract! '(=v (+v (*v a a) (*v b b)) (*v c c)))))
 (all-values
   (solution
    (list a b c)
     (static-ordering #'linear-force))))
|#


