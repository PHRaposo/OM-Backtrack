(in-package :om)
;;=============================================================================
;; Constraint output BOX
;;=============================================================================

(defclass* csp-box (OMBoxcall) ()
  (:metaclass OMStandardClass))

(defmethod get-frame-name ((self csp-box)) (name self))
(defmethod get-boxcallclass-fun ((self (eql 'om-solver))) 'csp-box)
(defmethod do-delete-one-input-extra ((self csp-box))  t)
(defmethod do-add-one-input-extra ((self csp-box)) t)

(defmethod! om-solver  ((solver integer) (const t) &rest opp-cons) 
  :numouts 1 
  :initvals '(0 nil nil) 
  :indoc '("solver" "constraint" "add constraint")
  :doc "Solve a csp"
  :menuins '((0 (("Situation" 0) ("Screamer" 1) ("Local Search" 2)))) 
  :icon 284
  (declare (ignore solver))
  (let ((csp (make-instance 'csp))
	variables rep)
    (setf (constraints csp) 
	  (loop for item in (append (list! const) (flat opp-cons)) 
		when (constraint-p item) collect item))
    (setf variables (loop for item in (constraints csp) append
			  (extract-variables item)))
    (setf variables (remove-duplicates variables :test 'equal))
    (setf (vars csp) variables)
    csp))

;;=============================================================================
;; EVAL AND COMPILE
;;=============================================================================

(defmethod* omNG-box-value ((self csp-box) &optional (numout 0))
  (omNG-box-value self numout))

;;=============================================================================
;; Constraints
;;=============================================================================
(defvar *evaluation-of-constraint* nil)

(defclass* constraint-box (OMBoxcall) ()
  (:metaclass OMStandardClass))

(defmethod eval-box-inputs ((self constraint-box))
  (let ((*evaluation-of-constraint* t))
    (print (loop for input in (inputs self) collect (gen-code input 0)))))

(defmethod gen-code ((self constraint-box) numout)
  (cond (*evaluation-of-constraint* (omng-box-value self numout))
	(t (call-next-method))))

(defmethod* alldiff ((self list))
  :icon 286
  (make-instance 'constraint 
		 :name "alldiff"
		 :exprs self))

(defmethod* c-exp ((rel t) (self list))
  :icon 288
  (make-instance 'constraint 
		 :name rel
		 :exprs self))

(defmethod* c= ((self t)  rest)
  :icon 290
  (make-instance 'constraint 
		 :name "equal"
		 :exprs (list self rest)))

(defmethod* c/= ((self cons-expr) &rest rest)
  :icon 298
  (make-instance 'constraint 
		 :name "diff"
		 :exprs (cons self rest)))

(defmethod* c< ((self t) (exp t))
  :icon 291
  (make-instance 'constraint 
		 :name "<="
		 :exprs (list self exp)))

(defmethod* c-mem ((self t) (exp t))
  :icon 292
  (make-instance 'constraint 
		 :name "member"
		 :exprs (list self exp)))

(defmethod* c-or ((self cons-expr) &rest rest)
  :icon 293
  (make-instance 'constraint 
		 :name "or"
		 :exprs (cons self rest)))

(defmethod* c-and ((self t) &rest rest)
  :icon 296
  (make-instance 'constraint 
		 :name "and"
		 :exprs (flat (append (list! self) rest))))

(defmethod* c-all ((self t) (vars list) (const constraint) )
  :icon 294
  (make-instance 'constraint 
		 :name "call"
		 :exprs (list self vars const)))

(defmethod* c-exi ((self t) (vars list) (const constraint))
  :icon 295
  (make-instance 'constraint 
		 :name "cext"
		 :exprs (list self vars const)))

(defmethod get-boxcallclass-fun ((self (eql 'alldiff))) 'constraint-box)
(defmethod get-boxcallclass-fun ((self (eql 'c-exp))) 'constraint-box)
(defmethod get-boxcallclass-fun ((self (eql 'c=))) 'constraint-box)
(defmethod get-boxcallclass-fun ((self (eql 'c/=))) 'constraint-box)
(defmethod get-boxcallclass-fun ((self (eql 'c<))) 'constraint-box)
(defmethod get-boxcallclass-fun ((self (eql 'c-mem))) 'constraint-box)
(defmethod get-boxcallclass-fun ((self (eql 'c-or))) 'constraint-box)
(defmethod get-boxcallclass-fun ((self (eql 'c-and))) 'constraint-box)

(defmethod get-boxcallclass-fun ((self (eql 'c-all))) 'constraint-box)
(defmethod get-boxcallclass-fun ((self (eql 'c-exi))) 'constraint-box)

;;-Expressiones----------------------------------------------------------------

(defmethod* relation ((self t)  termes)
  :icon 287
  (make-instance 'cons-expr
		 :rel self
		 :exprs termes))

(defmethod* i-rel ((self list) (rel symbol) (index list))
  :icon 281
  (loop for i from 0 to (- (length self) (+ (apply 'max index) 1)) collect
	(make-instance 'cons-expr 
		       :rel rel
		       :exprs (loop for ind in index collect
				    (nth (+ i ind) self)))))

(defmethod* i-rel ((self list) (rel ompatch) (index list))
  :icon 281
  (loop for i from 0 to (- (length self) (+ (apply 'max index) 1)) collect
	(make-instance 'cons-expr 
		       :rel rel
		       :exprs (loop for ind in index collect
				    (nth (+ i ind) self)))))

;;-Vars definiton--------------------------------------------------------------

(defclass* variable-box (OMBoxcall) ()
  (:metaclass OMStandardClass))

(defmethod gen-code ((self variable-box) numout)
  (cond (*evaluation-of-constraint* (omng-box-value self numout))
	(t (call-next-method))))

(defmethod omNG-box-value ((self variable-box) &optional (numout 0))
  (let ((rep (call-next-method)))
    (if *evaluation-of-constraint* rep
      (first (domaine rep)))))

(defmethod* permut-vars ((values list))
  :icon 300
  (let ((values (expand-lst values)))
    (loop for item in values collect
	  (let ((name (string (gensym "v"))))
	    (make-instance 'permut-variable 
			   :name name
			   :obj-ref name
			   :domaine item)))))

(defmethod* n-vars ((n integer) domaine)
  :icon 299
  (let ((domaine (expand-lst domaine)))
    (loop for i from 0 to (- n 1) collect
	  (let ((name (string (gensym "v"))))
	    (make-instance 'c-variable 
			   :name name
			   :obj-ref name
			   :domaine (nth i domaine))))))

(defmethod* var ((domaine t))
  :icon 289
  (let ((domaine (expand-lst domaine)))
    (make-instance 'c-variable 
		   :name (string (gensym "v"))
		   :obj-ref (string (gensym "v"))
		   :domaine domaine)))

(defmethod* ind-var ((self c-variable))
  :icon 297
  (make-instance 'cons-expr 
		 :rel 'var-ind
		 :exprs self))

(defmethod get-boxcallclass-fun ((self (eql 'permut-vars))) 'variable-box)
(defmethod get-boxcallclass-fun ((self (eql 'n-vars))) 'variable-box)
(defmethod get-boxcallclass-fun ((self (eql 'var))) 'variable-box)
(defmethod get-boxcallclass-fun ((self (eql 'ind-var))) 'variable-box)

;;-----------------------------------------------------------------------------
#|
(defvar *const-boxes-package*
  (omNG-protect-object (omNG-make-new-package "Constraints")))
(defvar *vars-boxes-package*
  (omNG-protect-object (omNG-make-new-package "Variables")))

|#
#|
(omNG-add-element *constraint-package* *const-boxes-package*)
(omNG-add-element *constraint-package* *vars-boxes-package*)

(AddGenFun2Pack '(alldiff c-exp c= c/= c< c-mem c-or c-and c-all c-exi)
		*const-boxes-package*)
(AddGenFun2Pack '(n-vars permut-vars var om-solver) *vars-boxes-package*)
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; screamer_trans.lisp

(defmethod scream-replace-vars ((self constraint) vars)
  (om-beep-msg "ayayay"))
   
(defmethod scream-replace-vars ((self list) vars) 
  (loop for item in self
	collect (scream-replace-vars item vars)))

(defmethod scream-replace-vars ((self t) vars) self)
(defmethod scream-replace-vars ((self c-variable) vars) 
  `(nth ,(position self vars :test 'equal) VarArray)) 

(defmethod translate-csp ((solver (eql 1)) csp)
  (let* ((constraintes (loop for item in (constraints csp) append 
			     (screamer-translate-constraint  (interne (string (name item))) item csp))))
    `(s::solution
      (let* ((VarArray (loop for var in (list ,@(vars csp)) 
			     collect (s::a-member-ofv  (domaine var)))))
	,@constraintes
	VarArray)
      (s::static-ordering #'s::divide-and-conquer-force))))

(defmethod solve-csp ((solver (eql 1))  csp)
  (eval `(s::one-value ,csp)))

;;-Constraint by constraint----------------------------------------------------

(defmethod screamer-translate-constraint  ((name (eql 'equal)) constraint csp)
  (let* ((allvars (vars csp))
	 (vars (extract-variables constraint))
	 (symb (loop for item in vars collect (interne (string (name item)))))
	 (pos (loop for item in vars collect `(nth ,(position item allvars :test 'equal) vararray))))
    (list `(s::assert! (s::funcallv #'(lambda ,symb 
					(= ,(replace-variables (first (exprs constraint)))
					   ,(replace-variables (second (exprs constraint)))))
				    ,.pos)))))

(defmethod screamer-translate-constraint ((name (eql 'alldiff)) constraint csp)
  (let* ((allvars (vars csp))
	 (liste (exprs constraint))
	 (vars-in-liste (loop for item in liste collect (extract-variables item)))
	 (replaced-liste (loop for item in liste collect (replace-variables  item)))
	 (index (case (length liste)
		  (0 nil) (1 nil) (2 '((0 1))) 
		  (otherwise (combinations (arithm-ser 0 (- (length liste) 1) 1)  2)))))
    (loop for item in index
	  append 
	  (let* ((i (car item)) (j (second item))
		 (vars2 (append (nth i vars-in-liste)  (nth j vars-in-liste) ))
		 (symbs2 (loop for item in vars2 collect (interne (string (name item)))))
		 (pos2 (loop for item in vars2 collect `(nth ,(position item allvars :test 'equal) vararray))))
	    (list `(s::assert! (s::funcallv #'(lambda ,symbs2 
						(not (equal ,(nth i replaced-liste) ,(nth j replaced-liste))))
					    ,@pos2)))))))

 
(defmethod screamer-translate-constraint  ((name (eql 'rel)) constraint csp) 'afaire)

(defmethod screamer-translate-constraint  ((name (eql 'diff)) constraint csp) 'afaire)

(defmethod screamer-translate-constraint  ((name (eql 'diff)) constraint csp) 'afaire)

(defmethod screamer-translate-constraint ((name (eql '<=)) constraint csp) 'afaire)

(defmethod screamer-translate-constraint ((name (eql 'member)) constraint csp) 'afaire)

(defmethod screamer-translate-constraint  ((name (eql 'or)) constraint csp) 'afaire)

(defmethod screamer-translate-constraint ((name (eql 'and)) constraint csp) 'afaire)

(defmethod screamer-translate-constraint  ((name (eql 'call)) constraint csp) 'afaire)

(defmethod screamer-translate-constraint  ((name (eql 'cext)) constraint csp) 'afaire)
