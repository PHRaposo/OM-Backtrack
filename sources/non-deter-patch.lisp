;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; REVISED VERSION (RESTORATION OF OM-BACKTRACK)
;;; Copyright 2024 PAULO HENRIQUE RAPOSO AND KARIM HADDAD
;;;
;;; OM-BACKTRACK 2.0 
;;; Copyright 2024 PAULO HENRIQUE RAPOSO 
;;;

(in-package :om)
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; "?" - NONDETERMINISTIC PATCH (NEW VERSION FOR BACKTRACK 2.0)

(defmethod nondeter-omlispfun? ((self OMBoxlispCall))
 (let* ((funname (car (gen-code self 0)))
        (record (screamer::get-function-record funname)))
  (not (screamer::function-record-deterministic? record))))
  
(defmethod non-deter-patch? ((self OMLispPatchAbs))
 (let* ((exp (get-lisp-exp (lisp-exp self)))
        (subst? (multiple-value-list (ignore-errors (screamer::needs-substitution? exp nil)))))
  (if (and (= (length subst?) 2) ;<== FROM SCREAMER-PLUS (CAREFULLY)
		      (null (car subst?))
		      (typep (second subst?) (find-class 'error)))
	   nil
	  (car subst?))))
	  
(defmethod non-deter-patch? ((self OMPatch)) 
 (let* ((boxes (boxes self))
	    (screamerboxes (find-class-boxes boxes 'screamerboxes)) ;<== SCREAMER FUNCTIONS
	    (screamer-valuation-boxes (find-class-boxes boxes 'screamer-valuation-boxes)) ;<== SCREAMER VALUATION
        (lispfuns (find-class-boxes boxes 'omboxlispcall)) ;<== OMLISPFUN
        (sub-patches (find-class-boxes boxes 'omboxabspatch)) ;<== OMBOXABSPATCH (SUB PATCHES)
        (non-deter-sub-patch? (not (null (position t (mapcar #'(lambda (x) (non-deter-patch? (reference x))) sub-patches))))))
  (if (or screamerboxes (some #'nondeter-omlispfun? lispfuns) non-deter-sub-patch? screamer-valuation-boxes)
      t
      nil)))

(defmethod om-draw-contents :after ((self patch-icon-box))
  (when (non-deter-patch? (reference (object (om-view-container self))))
   (om-with-fg-color self *om-pink-color*
    (om-with-font (om-make-font "Courier" (* *icon-size-factor* 34))
      (om-draw-char (- (round (w self) 2) (* *icon-size-factor* 10)) 
		            (+ (round (h self) 2) (* *icon-size-factor* 10)) 
	    #\?)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; omNG-box-value - OMBoxPatch

(defmethod omNG-box-value ((self OMBoxPatch) &optional (num-out 0))
(handler-bind ((error #'(lambda (c)
                          (when *msg-error-label-on*
                            (om-message-dialog (string+ "Error while evaluating the box " (string (name self)) " : " 
                                                     (om-report-condition c))
                                               :size (om-make-point 300 200))
                            (clear-after-error self)
                            (om-abort)))))
   (cond
	   	   	           
   ;((and (equal (allow-lock self) "l") ;<== TEST
   ;	     (non-deter-patch? (reference self)))		 	 		 	 
   ;	      (om-message-dialog "Nondeterministic patches in lambda mode has not been implemented yet.")
   ;           (clear-after-error self)
   ;           (om-abort))	
				  				             				 	 			
    ((and (equal (allow-lock self) "x") (value self))
     (nth num-out (value self)))	 
		 
	((and (equal (allow-lock self) "o")
	 (setf (value self) (list (reference self))) (car (value self))))
	 	 
    ((equal (allow-lock self) "l")
     (unless (compiled? (reference self))
       (if (and (lisp-exp-p (reference self)) (editorframe self))
         (compile-without-close (editorframe self))          
          (compile-patch (reference self))))				 	 				  
         (setf (value self) ;;; test ...  => OM 7.2
                (list (special-lambda-value self (intern (string (code (reference self))) :om))))
         (car (value self)))
		 		  
    ((and (equal (allow-lock self) "&") (ev-once-p self)) 
     (nth num-out (value self)))
	 
    (t (unless (compiled? (reference self))
           (if (and (lisp-exp-p (reference self)) (editorframe (reference self))) 
             (compile-without-close (editorframe (reference self)))
             (compile-patch (reference self))))
		(let* ((args  (mapcar #'(lambda (input) 
                                 (omNG-box-value  input)) (inputs self)))
              (rep nil))
       
			 (if (non-deter-patch? (reference self))
			     (setf rep (multiple-value-list (eval `(,(intern (string (code (reference self))) :om) 
                                           ,.(loop for item in args collect `',item)))))
		         (setf rep (multiple-value-list (apply (intern (string (code (reference self))) :om) args))))
			 			 
         (when (equal (allow-lock self) "&")
           (setf (ev-once-p self) t)
           (setf (value self) rep))
         (when (equal (allow-lock self) "x")
           (setf (value self) rep))
           ;;;; TEST => OM 7.2
           (when (equal (allow-lock self) nil)
             (setf (value self) rep))
           ;;;;	 	   
             (nth num-out rep))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COMPILE-PATCH
	   
(defmethod compile-patch ((self OMPatch)) 
 "Generation of lisp code from the graphic boxes."
	(unless (compiled? self)
	(if (lisp-exp-p self)
	    (if (non-deter-patch? self)
            (compile (eval `(screamer::defun ,(intern (string (code self)) :om)
                             ,.(cdr (get-lisp-exp (lisp-exp self))))))
	        (compile (eval `(defun ,(intern (string (code self)) :om)
	                         ,.(cdr (get-lisp-exp (lisp-exp self))))))) 									
	(let* ((boxes (boxes self))
	   (temp-out-box (find-class-boxes boxes 'OMtempOut))
	   (self-boxes (patch-has-temp-in-p self)) 
	   (out-box (find-class-boxes boxes 'OMout))
	   (in-boxes (find-class-boxes boxes 'OMin))
	   (out-symb (code self))
	   (oldletlist *let-list*)
	   (oldlambdacontext *lambda-context*)		 
	  symbols body)
	  (setf out-box (list+ temp-out-box (sort out-box '< :key 'indice)))
	  (setf in-boxes (list+ self-boxes (sort in-boxes '< :key 'indice)))
	  (setf symbols (mapcar #'(lambda (thein) (setf (in-symbol thein) (gensym))) in-boxes))
	  (setf *let-list* nil)	 
	  (cond ((non-deter-patch? self)
	         (setf body `(values ,.(mapcar #'(lambda (theout) (gen-code theout 0)) out-box)))
	  	  	 (eval `(screamer::defun ,(intern (string out-symb) :om)  (,.symbols)
	  	  	     	(let* ,(reverse *let-list*) ,body))))												   													   
  		    (t (setf body `(values ,.(mapcar #'(lambda (theout)
  					               (gen-code theout 0)) out-box)))					   
  			   (eval `(defun ,(intern (string out-symb) :om)  (,.symbols)
  			   	       (let* ,(reverse *let-list*) ,body))))) 						   						   								   			  	  				 						 						 		 	 
	  (setf *let-list* oldletlist)
	  (setf *lambda-context* oldlambdacontext)
		   ))	
	(setf (compiled? self) t)))
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
;;; PATCH-CODE

(defmethod gen-patch-code ((self OMPatch)) 
	"Prints the lisp code from a clone of a patch in itself mode."
	(let ((patch-clone (clone self)))
	(if (lisp-exp-p patch-clone)		
		(if (non-deter-patch? self)
	        `(screamer::defun ,(intern (string (code patch-clone)) :om)
	                        ,.(cdr (get-lisp-exp (lisp-exp patch-clone))))
		    `(defun ,(intern (string (code self)) :om)
		                    ,.(cdr (get-lisp-exp (lisp-exp patch-clone))))) 					
	(let* ((boxes (boxes patch-clone))
	   (temp-out-box (find-class-boxes boxes 'OMtempOut))
	   (self-boxes (patch-has-temp-in-p patch-clone)) 
	   (out-box (find-class-boxes boxes 'OMout))
	   (in-boxes (find-class-boxes boxes 'OMin))
	   (out-symb (code patch-clone))
	   (oldletlist *let-list*)
	   (oldlambdacontext *lambda-context*)		 
	  symbols body patch-code)
	  (setf out-box (list+ temp-out-box (sort out-box '< :key 'indice)))
	  (setf in-boxes (list+ self-boxes (sort in-boxes '< :key 'indice)))
	  (setf symbols (mapcar #'(lambda (thein) (setf (in-symbol thein) (gensym))) in-boxes))
	  (setf *let-list* nil) 
	  (cond ((non-deter-patch? self) 
		     (setf body `(values ,.(mapcar #'(lambda (theout) (gen-code theout 0)) out-box)))
		     (setf patch-code `(screamer::defun ,(intern (string out-symb) :om)  (,.symbols)
		  		  	     	    (let* ,(reverse *let-list*) ,body))))
								
  		    (t (setf body `(values ,.(mapcar #'(lambda (theout)
  					               (gen-code theout 0)) out-box)))					   
  			   (setf patch-code `(defun ,(intern (string out-symb) :om)  (,.symbols)
  			   	       (let* ,(reverse *let-list*) ,body))))) 																	   													  
	  (setf *let-list* oldletlist)
	  (setf *lambda-context* oldlambdacontext)    
	   patch-code ;===> returns the function code
	 )))	
	)
	   
;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;OMLispPatch

(defun compile-lisp-patch-fun (patch)
  (if (get-lisp-exp (lisp-exp patch))
      (if (non-deter-patch? patch)
          (eval `(screamer::defun ,(intern (string (code patch)) :om)
                   ,.(cdr (get-lisp-exp (lisp-exp patch)))))
          (eval `(defun ,(intern (string (code patch)) :om)
                  ,.(cdr (get-lisp-exp (lisp-exp patch))))))
    (eval `(defun ,(intern (string (code patch)) :om) () nil))))
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
;;;TODO (IF NEEDED)
;;; 
;;; => OMLOOP (???)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
