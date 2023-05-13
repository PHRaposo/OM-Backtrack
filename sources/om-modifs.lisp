(in-package :om)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FROM OM 4.71b ;;;;;;;
;;; save.lisp

(defun load-obj-from-obj (object)
  (if (or (loaded? object)
	  (member object *loaading-stack* :test 'equal)) object
    (with-cursor
     *watch-cursor* 
     (push object *loaading-stack*)
     (print (string+ "Loading..." (mac-namestring (mypathname object))))
     (eval-non-text-file (mypathname object))
     (if *om-current-persistent*
	 (progn
	   (setf (boxes object) nil)
	   (setf (connec object) (connec *om-current-persistent*))
	   (mapc #'(lambda (box) (omNG-add-element object (eval box)))
		 (boxes *om-current-persistent*))
	   (setf (boxes object) (reverse (boxes object)))
	   (load-picts object)
	   (setf (lisp-exp-p object) (lisp-exp-p *om-current-persistent*))
	   (when (lisp-exp-p object)
	     (eval `(screamer::defun ,(intern (string (code object)) :om)
				     ,.(cdr (eval (lisp-exp-p object))))))
	   (setf *om-current-persistent* nil)
	   object) 
       'dead))))
	   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PatchBoxes.lisp

;;screamer
(defmethod gen-code ((self OMBoxPatch) numout)
  (let ((patchfun `,(intern (string (code (reference self))) :om)))
    (cond
     ((equal (allow-lock self) "&") 
      (gen-code-for-ev-once self numout))
     ((equal (allow-lock self) "l")
      (curry-lambda-code self  patchfun))
     ((equal (allow-lock self) "o")  (reference self))
     ((equal (allow-lock self) "x") 
      `(nth ,numout ,(gen-code (value self) 0)))
     (t
      (if (zerop numout)
	  `(,patchfun ,.(decode self))
	`(nth ,numout (multiple-value-list (,patchfun ,.(decode self)))))))))

;;screamer
(defmethod omNG-box-value ((self OMBoxPatch) &optional (num-out 0))
  (portable-omNG-box-value self num-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Events.lisp

(defmethod window-close ((self patch-lambda-exp-window))
  (handler-bind
   ((error #'(lambda (c) (declare (ignore c))
	       (window-select self)
	       (let ((rep  (y-or-n-dialog "Patch Definition Error. Do you want to close the window? If yes no modification will be make to the patch")))
		 (when rep
		   (setf *abort-definition* t)
		   (window-close self))
		 (abort)))))
   (unless *abort-definition*
     (let* ((size (buffer-size (fred-buffer self)))
	    (pos (buffer-skip-fwd-wsp&comments (fred-buffer self) 0 size))
	    (expression (buffer-current-sexp (fred-buffer self) pos)))
       (unless (ccl::lambda-expression-p expression)
	 (beep-msg "Error! this is not a lambda expression. Lambda expressions are of the form '(lambda <param-list> <body>)")
	 (error "error"))
       (unless (equal (lisp-exp-p (patchref self)) expression)
	 (setf (lisp-exp-p (patchref self)) expression)
	 (eval `(screamer::defun ,(intern (string (code (patchref self))) :om)
				 ,.(cdr (lisp-exp-p (patchref self)))))
	 
	 (loop for item in (attached-objs (patchref self)) do
	       (update-from-reference item)))))
   (setf (compiled? (patchref self)) t)
   (setf (editorframe (patchref self)) nil)
   (setf *abort-definition* nil)
   (call-next-method)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; boxframe.lisp 
   
;screamer
(defmethod draw-after-box ((self patchboxFrame))
  (portable-draw-after-box self))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OMPatch.lisp

;screamer
(defmethod compile-patch ((self OMPatch)) 
  "Generation of lisp code from the graphic boxes."
  (unless (compiled? self)
    (if (lisp-exp-p self)
	(eval `(screamer::defun ,(intern (string (code self)) :om)
				,.(cdr (lisp-exp-p self))))
      (let* ((boxes (boxes self))
	     (vars (find-class-boxes boxes 'OMBoxVar))
	     (out-box (find-class-boxes boxes 'OMout))
	     (in-boxes (find-class-boxes boxes 'OMin))
	     (out-symb (code self))
	     (oldletlist *let-list*) symbols body)
	(setf out-box (sort out-box '< :key 'indice))
	(setf in-boxes (sort in-boxes '< :key 'indice))
	(setf symbols (mapcar #'(lambda (thein)
				  (setf (in-symbol thein) (gensym))) in-boxes))
	(setf *let-list* nil)
	(mapc #'(lambda (thevar) (gen-code thevar 0)) vars)
	(setf body `(values ,.(mapcar #'(lambda (theout)
					  (gen-code theout 0)) out-box)))
	(eval  `(screamer::defun ,(intern (string out-symb) :om)  (,.symbols)
				 (let* ,*let-list* ,body)))
	(setf *let-list* oldletlist)))
    (setf (compiled? self) t)))
	
;;screamer
;;T if patch use screamer functions
(defmethod non-deter-patch? ((self OMPatch)) 
   (let ((record (s::get-function-record (intern (string (car (list! (code self)))) :om))))
     (not (s::function-record-deterministic? record))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; preferences.lisp 

;;-SCREAMER--------------------------------------------------------------------

(defvar *screamer-valuation* 0)

;;-SCREAMER--------------------------------------------------------------------

(pushr (make-instance 'pref-icon-view :iconID 214 :view-size (ompoint 40 40)
		      :view-position (ompoint 5 15)
                      :defvals '(nil t ("Geneva" 10 :srcor) 0 "Guarigocha" 0)  
                      ;;(specialfilewirter messageerrorhandle commentstyle
		      ;;		 commentcolor)
                      ) *pref-item-list*)

(defmethod put-preferences ((iconID (eql 214)))
  (let ((modulepref (get-pref-by-icon iconID)))
    (setf *delete-file*         (first (defvals modulepref)))
    (setf *msg-error-label-on*  (second (defvals modulepref)))
    (setf *comment-style*       (third (defvals modulepref)))
    (setf *comment-color*       (fourth (defvals modulepref)))
    (setf *composer-name*       (or (fifth (defvals modulepref)) "Guarigocha"))
    (setf *screamer-valuation*  (or (sixth (defvals modulepref)) 0))))


	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; omloop.lisp

;;screamer
(defmethod call-gen-code ((self OMLoop-Box) numout)
  (let ((in-list (mapcar #'(lambda (thein) (gen-code thein 0)) (inputs self))))
    (if (zerop numout) 
	`(,(intern (string (first (code (patch self)))) :om) ,.in-list)
      `(nth ,numout (multiple-value-list (,(intern (string (first (code (patch self)))) :om) ,.in-list))))))

;;screamer
(defmethod gen-code-call ((self OMLoop-Box))
  (let ((in-list (mapcar #'(lambda (thein) (gen-code thein 0)) (inputs self))))
    `(,(intern (string (first (code (patch self)))) :om) ,.in-list)))

;;screamer
(defmethod special-lambda-value ((self OMLoop-Box) symbol)
  "Eval a loop box in lambda mode."
  (let* ((nesymbs nil)
	 (args  (mapcar #'(lambda (input)
			    (if (connected? input)
				`',(omNG-box-value  input)
			      (let ((newsymbol (gensym)))
				(push newsymbol nesymbs)
				newsymbol))) (inputs self))))
    (if (null nesymbs)
	symbol
      (eval `#'(lambda ,(reverse nesymbs)
		 (apply (fdefinition ',(intern (string (first (code (patch self)))) :om)) (list ,.args)))))))

;;screamer
(defmethod curry-lambda-code ((self OMLoop-Box) symbol)
  "Lisp code generation for a loop box in lambda mode."
  (let* ((nesymbs nil)
         (args  (mapcar #'(lambda (input)
                            (if (connected? input)
				(gen-code input 0)
                              (let ((newsymbol (gensym)))
                                (push newsymbol nesymbs)
                                newsymbol))) (inputs self))))
    (if (null nesymbs)
	symbol
      `#'(lambda ,(reverse nesymbs)
	   (apply (fdefinition ',(intern (string (first (code (patch self)))) :om)) (list ,.args))))))

;;screamer
;(defmethod draw-after-box ((self loopBoxFrame))
;   (with-fore-color 16719095
;     (when (non-deter-patch? (patch (object self)))
;       (#_Textsize 28)
;       (draw-char (- (round (w self) 2) 8) (+ (round (h self) 2) 6) #\? ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; omboxes.lisp
;;=============================================================================
;; Constraint output BOX
;;=============================================================================

(defclass csp-box (OMBoxcall) ()
  (:metaclass OMStandardClass))

(defmethod get-frame-name ((self csp-box)) (name self))
(defmethod get-boxcallclass-fun ((self (eql 'om-solver))) 'csp-box)
(defmethod do-delete-one-input-extra ((self csp-box))  t)
(defmethod do-add-one-input-extra ((self csp-box)) t)

(defmethod! om-solver  ((solver integer) (const t) &rest opp-cons) :numouts 1 
  :initvals '(0 nil nil) 
  :indoc '("solver" "constraint" "add constraint")
  :doc "Solve a csp"
  :menuins ((0 (("Situation" 0) ("Screamer" 1) ("Local Search" 2)))) :icon 284
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
  (portable-omNG-box-value self numout))

;;=============================================================================
;; Constraints
;;=============================================================================
(defvar *evaluation-of-constraint* nil)

(defclass constraint-box (OMBoxcall) ()
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

(defclass variable-box (OMBoxcall) ()
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
	    (make-instance 'variable 
			   :name name
			   :obj-ref name
			   :domaine (nth i domaine))))))

(defmethod* var ((domaine t))
  :icon 289
  (let ((domaine (expand-lst domaine)))
    (make-instance 'variable 
		   :name (string (gensym "v"))
		   :obj-ref (string (gensym "v"))
		   :domaine domaine)))

(defmethod* ind-var ((self variable))
  :icon 297
  (make-instance 'cons-expr 
		 :rel 'var-ind
		 :exprs self))

(defmethod get-boxcallclass-fun ((self (eql 'permut-vars))) 'variable-box)
(defmethod get-boxcallclass-fun ((self (eql 'n-vars))) 'variable-box)
(defmethod get-boxcallclass-fun ((self (eql 'var))) 'variable-box)
(defmethod get-boxcallclass-fun ((self (eql 'ind-var))) 'variable-box)

;;-----------------------------------------------------------------------------

(defvar *const-boxes-package*
  (omNG-protect-object (omNG-make-new-package "Constraints")))
(defvar *vars-boxes-package*
  (omNG-protect-object (omNG-make-new-package "Variables")))

(omNG-add-element *constraint-package* *const-boxes-package*)
(omNG-add-element *constraint-package* *vars-boxes-package*)

(AddGenFun2Pack '(alldiff c-exp c= c/= c< c-mem c-or c-and c-all c-exi)
		*const-boxes-package*)
(AddGenFun2Pack '(n-vars permut-vars var om-solver) *vars-boxes-package*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; screamer_trans.lisp

(defmethod scream-replace-vars ((self constraint) vars)
  (beep-msg "ayayay"))
   
(defmethod scream-replace-vars ((self list) vars) 
  (loop for item in self
	collect (scream-replace-vars item vars)))

(defmethod scream-replace-vars ((self t) vars) self)
(defmethod scream-replace-vars ((self variable) vars) 
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; portable

	 (defmethod portable-draw-after-box ((self patchboxFrame))
	   (with-fore-color
	    16719095
	    (when (non-deter-patch? (reference (object self)))
	      (#_Textsize 28)
	      (draw-char (- (round (w self) 2) 8) (+ (round (h self) 2) 6) #\? ))))



defmethod portable-draw-after-box ((self aliasBoxframe))
  (let ((view (iconView self)))
    (with-focused-view view
		       (rlet ((r :rect                         
				 :topleft (ompoint 0 0)
				 :bottomright  (view-size view))
			      (ps :penstate)) 
			     (#_GetPenState ps)
			     (#_PenPat *light-gray-pattern*)
			     (#_PenMode 11)
			     (#_PaintRect r)
			     (#_SetPenState ps)))))

(defmethod portable-draw-after-box ((self boxEditorFrame)) 
  (when (view-of-patch (object self))
    (with-focused-view self
		       (ccl::with-hilite-mode
			(with-pen-state (:mode :patxor)
					(fill-rect* 0 0 (w self) (h self)))))))

(defmethod portable-draw-after-box ((self patchboxFrame))
  (with-fore-color
   16719095
   (when (non-deter-patch? (reference (object self)))
     (#_Textsize 28)
     (draw-char (- (round (w self) 2) 8) (+ (round (h self) 2) 6) #\? ))))
	 
(defmethod* portable-omNG-box-value ((self OMBoxCall) &optional (numout 0))
  (handler-bind
   ((error #'(lambda (c)
	       (when *msg-error-label-on*
		 (message-dialog (string+ "Error while evaluating the box " (string (name self)) " " (ccl::report-condition c nil)) :size (make-point 300 200))
		 (abort)))))
   (cond
    ((equal (allow-lock self) "l")
     (special-lambda-value self (reference self)))
    ((equal (allow-lock self) "o") (fdefinition (reference self)))
    ((and (equal (allow-lock self) "x") (value self))
     (nth numout (value self)))
    ((and (equal (allow-lock self) "&") (ev-once-p self))
     (nth numout (value self)))
    (t (let* ((args  (eval-box-inputs self))
	      (themethod (compute-applicable-methods (fdefinition (reference self)) args)) rep)
	 (if (null themethod)
             (progn (dialog-message (string+ "no method is defined for inputs in box " (name self)))
                    (abort))
	   (progn
	     (when (and (EditorFrame (car themethod))
			(not (compiled? (car themethod))))
	       (modify-genfun (EditorFrame (car themethod))))
	     (if (equal (class-name (class-of self)) 'OMBoxcall)
                 (setf rep (multiple-value-list (apply (reference self) args)))
	       (setf rep (multiple-value-list  (special-value self args))))))
	 (when (equal (allow-lock self) "&")
	   (setf (ev-once-p self) t)
	   (setf (value self) rep))
	 (when (equal (allow-lock self) "x")
	   (setf (value self) rep))
	 (nth numout rep))))))

(defmethod portable-omNG-box-value ((self OMBoxRelatedWClass)
				    &optional (numout 0))
  (handler-bind
   ((error #'(lambda (c)
	       (when *msg-error-label-on*
		 (message-dialog (string+ "Error while evaluating the box " (string (name self)) " " (ccl::report-condition c nil)) :size (make-point 300 200))
		 (abort)))))
   (let ((editorclass (class-name (reference self))))
     (cond
      ((equal (allow-lock self) "l") (special-lambda-value self editorclass))
      ((equal (allow-lock self) "o") (reference self))
      ((and (equal (allow-lock self) "x") (value self))
       (rep-editor (value self) numout))
      ((and (equal (allow-lock self) "&") (ev-once-p self))
       (rep-editor (value self) numout))
      (t (let* ((args  (mapcar #'(lambda (input) (omNG-box-value input))
			       (inputs self)))
		rep)
	   (setf rep (cons-new-object (value self) args
				      (connected? (first (inputs self)))))
	   (if (null rep)
               (progn
                 (beep-msg (string+ "I can not construct a " (string editorclass) " with these parameters"))
                 (abort))
	     (progn
	       (setf (value self) rep)
	       (update-if-editor self)
	       (when (equal (allow-lock self) "&")
		 (setf (ev-once-p self) t))
	       (rep-editor (value self) numout)))))))))
		   
(defmethod portable-omNG-box-value ((self arrayBox) &optional (numout 0))
  (handler-bind
   ((error #'(lambda (c)
	       (when *msg-error-label-on*
		 (message-dialog (format nil "Error while evaluating the ~D factory ~%~D" (class-name (reference self)) (report-condition c nil)) :size (make-point 300 200))
		 (abort)))))
   (let ((editorclass (class-name (reference self))))
     (cond
      ((equal (allow-lock self) "l") (special-lambda-value self editorclass))
      ((equal (allow-lock self) "o") (reference self))
      ((and (equal (allow-lock self) "x") (value self)) 
       (rep-editor (value self) (numout2label-num self numout)))
      ((and (equal (allow-lock self) "&") (ev-once-p self))
       (rep-editor (value self) (numout2label-num self numout)))
      (t (let* ((args (loop for input in (inputs self)
			    when (not (keyword-input-p input))
			    collect (omNG-box-value input)))
		(argkeys (eval-keywords self))
		rep)
	   (setf rep (cons-new-array (value self) args argkeys
				     (connected? (first (inputs self)))
				     editorclass))
	   (if (null rep)
               (progn
                 (beep-msg (string+ "I can not construct a "
				    (string editorclass)
				    " with these parameters"))
                 (abort))
	     (progn
	       (setf (value self) rep)
	       (when (showpict self)
		 (update-miniview (iconview (car (frames self))) (value self)))
	       (when (editorFrame self)
		 (update-editor-after-eval (editorFrame self) rep))
	       (when (equal (allow-lock self) "&")
		 (setf (ev-once-p self) t))
	       (numout2label-num self numout)
	       (rep-editor (value self) (numout2label-num self numout))))))))))
		   
   (defmethod portable-omNG-box-value ((self OMTextFilebox) &optional (numout 0))
     (handler-bind
      ((error #'(lambda (c)
   	       (when *msg-error-label-on*
   		 (message-dialog (string+ "Error while evaluating the box " (string (name self)) " " (report-condition c nil)) :size (ompoint 300 200))
   		 (abort)))))
      (let ((intype (omNG-box-value (third (inputs self)))))
        (setf (ed-mode (value self)) intype)
        (cond
         ((equal (allow-lock self) "o") self)
         ((equal (allow-lock self) "l")
          "sorry not lambda mode for textfile boxes")
         ((and (equal (allow-lock self) "x") (value self))
          (rep-editor (value self) numout))
         ((and (equal (allow-lock self) "&") (ev-once-p self))
          (rep-editor (value self) numout))
         (t
          (let ((val (omNG-box-value  (second (inputs self))))
   	     (newtextfile (make-instance (type-of (value self))
   					 :ed-mode intype)))
   	 (setf (file-name newtextfile) (file-name (value self)))
   	 (setf val (list! val))
   	 (setf (file-name newtextfile) (file-name (value self)))
   	 (setf (buffer-text newtextfile)
   	       (add/replace-to-buffer (value self) val t))
   	 (setf (value self) newtextfile)
   	 (update-if-editor self)
   	 (rep-editor (value self) numout)))))))
