(in-package :om)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; "?" - NONDETERMINISTIC PATCH

(defmethod non-deter-patch? ((self OMPatch)) 
(let ((record (s::get-function-record (intern (string (car (list! (code self)))) :om))))
(not (s::function-record-deterministic? record))))

(defmethod om-draw-contents :after ((self patch-icon-box))
  (when (non-deter-patch? (reference (object (om-view-container self))))
   (om-with-fg-color self *om-pink-color*
    (om-with-font (om-make-font "Courier" (* *icon-size-factor* 34))
      (om-draw-char (- (round (w self) 2) (* *icon-size-factor* 10)) 
		            (+ (round (h self) 2) (* *icon-size-factor* 10)) 
	    #\?)))))

#|
;------------------------------------------------------------
; SOLUTION-PATCH

(defmethod solution-patch? ((self OMPatch)) 
 (let* ((boxes (boxes self))
        (lisp-boxes (find-class-boxes boxes 'omboxlispcall))
        (references (mapcar #'reference boxes)))
  (not (null (find 'om-solution references)))))

;;DRAWS A PINK QUESTION MARK FOR BACKTRACK AND YELLOW FOR SOLUTION

(defmethod om-draw-contents :after ((self patch-icon-box))
  (when (solution-patch? (reference (object (om-view-container self))))
   (om-with-fg-color self *om-yellow-color*
    (om-with-font (om-make-font "Courier" (* *icon-size-factor* 34))
      (om-draw-char (- (round (w self) 2) (* *icon-size-factor* 10)) 
		            (+ (round (h self) 2) (* *icon-size-factor* 10)) 
	    #\?))))
(when (non-deter-patch? (reference (object (om-view-container self))))
 (om-with-fg-color self *om-pink-color*
  (om-with-font (om-make-font "Courier" (* *icon-size-factor* 34))
    (om-draw-char (- (round (w self) 2) (* *icon-size-factor* 10)) 
	            (+ (round (h self) 2) (* *icon-size-factor* 10)) 
    #\?)))))
	
;------------------------------------------------------------
; NON DETER FOR OMPATCH (NOT ABS)

(defmethod om-draw-contents :after ((self patch-finder-icon)) ; maybe not needed
  (when (non-deter-patch? (object (om-view-container self)))
      (om-with-fg-color self *om-pink-color*
		  (if (= (presentation (editor *om-workspace-win*)) 0) ;do not update size when the patch is inside folders
              (om-with-font (om-make-font "Courier" 34)
               (om-draw-char (- (round (w self) 2) 10) (+ (round (h self) 2) 10) #\?))
               (om-with-font (om-make-font "Courier" 22)
                (om-draw-char (- (round (w self) 2) 6) (+ (round (h self) 2) 6) #\?))
	))))
|#

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
    ;((and (equal (allow-lock self) "l") 
    ;	      (non-deter-patch? (reference self))
    ;	      (lisp-exp-p (reference self))) 													      
;	      (om-message-dialog "Nondeterministic lisp patches in lambda mode has not been implemented yet.")
 ;             (clear-after-error self)
  ;            (om-abort))
	           
   ((and (equal (allow-lock self) "l") 
	     (non-deter-patch? (reference self)))
         (let ((screamer-patchfun (screamer-patch-code (reference self))))
          (setf (value self) 
           (list (special-lambda-value self (intern (string screamer-patchfun) :om))))
         (car (value self))))
		 
  		;(compile-screamer-patch (reference self))
        ;  (setf (value self) ;;; test ...  => OM 7.2
        ;   (list (special-lambda-value self (intern (string (code (reference self))) :om))))
        ;  (car (value self)))
		 	 		 	 
	      ;(om-message-dialog "Nondeterministic patches in lambda mode has not been implemented yet.")
              ;(clear-after-error self)
              ;(om-abort)) 
				       
         ;compile-patch ???
         ;(setf (value self) ??? (list    
   	     ;(special-lambda-value self (intern (string (code (reference self))) :om))) ;)
         ;(car (value self))) ???
	           				 	 			
    ((and (equal (allow-lock self) "x") (value self))
     (nth num-out (value self)))
    ;((and (equal (allow-lock self) "o") (reference self))) ; => OM 4 
	((and (equal (allow-lock self) "o")
	 (setf (value self) (list (reference self))) (car (value self)))) ; => OM 7.2

    ((equal (allow-lock self) "l")
     (unless (compiled? (reference self))
       (if (and (lisp-exp-p (reference self)) (editorframe self))
         (compile-without-close (editorframe self))
          (compile-patch (reference self))))	
			 	 				  
         (setf (value self) ;;; test ...  => OM 7.2
                (list (special-lambda-value self (intern (string (code (reference self))) :om))))
         (car (value self)))

            ; (special-lambda-value self (intern (string (code (reference self))) :om)) ; =>OM 4
		  
    ((and (equal (allow-lock self) "&") (ev-once-p self)) 
     (nth num-out (value self)))
    (t (let* ((args  (mapcar #'(lambda (input) 
                                 (omNG-box-value  input)) (inputs self)))
              (rep nil) non-deter?)
       
         (unless (compiled? (reference self))
           (if (and (lisp-exp-p (reference self)) (editorframe (reference self))) 
             (compile-without-close (editorframe (reference self)))
             (compile-patch (reference self))))
         (setf non-deter? (non-deter-patch? (reference self)))
         (setf rep (if non-deter?
                     (case *screamer-valuation*
                       (0 (multiple-value-list (eval `(s::one-value (,(intern (string (code (reference self))) :om) 
                                                                     ,.(loop for item in args collect `',item))))))
                       (1 (multiple-value-list (eval `(s::all-values (,(intern (string (code (reference self))) :om) 
                                                                      ,.(loop for item in args collect `',item))))))  
                       (2 (multiple-value-list (eval `(s::print-values (,(intern (string (code (reference self))) :om) 
                                                                        ,.(loop for item in args collect `',item)))))))
                     (multiple-value-list (apply  (intern (string (code (reference self))) :om) args))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COMPILE-PATCH

(defmethod compile-patch ((self OMPatch)) 
 "Generation of lisp code from the graphic boxes."
	(unless (compiled? self)
	(if (lisp-exp-p self)		
	  (compile (eval `(screamer::defun ,(intern (string (code self)) :om)
	            ,.(cdr (get-lisp-exp (lisp-exp self)))))) ;(lisp-exp-p self))))))					
	(let* ((boxes (boxes self))
	   ;(vars (find-class-boxes boxes 'screamerboxes)) ;;;=> 'OMBoxVar in OM 4 
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
	  ;(mapc #'(lambda (thevar) (gen-code thevar 0)) vars) ;; ??? => OM 4 					
	  (setf body `(values ,.(mapcar #'(lambda (theout)
					               (gen-code theout 0)) out-box)))
 	  ;(let* ((internal-patch? (find-class-boxes boxes 'omboxabspatch))
 	  ;      (non-deter-abspatch? (loop for boxabspatch in internal-patch? collect 
;				                  (if (and (not (null (car internal-patch?))) 
;									       (non-deter-patch? (reference boxabspatch))) 
;									   boxabspatch nil))))
; 		(if (car non-deter-abspatch?)
; 			(mapcar #'(lambda (abspatch) 
; 				       (subst (code (reference abspatch)) (screamer-patch-code (reference abspatch)) body)) non-deter-abspatch?)
; 			nil))
;			(print (list "BODY:" body))								   
	  (eval `(screamer::defun ,(intern (string out-symb) :om)  (,.symbols)
	      (let* ,(reverse *let-list*) ,body)))				 						 						 		 	 
	  (setf *let-list* oldletlist)
	  (setf *lambda-context* oldlambdacontext)
		   ))	
	(setf (compiled? self) t)))

(defmethod gen-patch-code ((self OMPatch)) 
	"Prints the lisp code from a clone of a patch in itself mode."
	(let ((patch-clone (clone self)))
	(if (lisp-exp-p patch-clone)		
	  `(defun ,(intern (string (code patch-clone)) :om)
	    ,.(cdr (get-lisp-exp (lisp-exp patch-clone))))					
	(let* ((boxes (boxes patch-clone))
	   (temp-out-box (find-class-boxes boxes 'OMtempOut))
	   (self-boxes (patch-has-temp-in-p patch-clone)) 
	   (out-box (find-class-boxes boxes 'OMout))
	   (in-boxes (find-class-boxes boxes 'OMin))
	   (out-symb (code patch-clone))
	   (new-out-symb (gensym))
	   (oldletlist *let-list*)
	   (oldlambdacontext *lambda-context*)		 
	  symbols body)
	  (setf out-box (list+ temp-out-box (sort out-box '< :key 'indice)))
	  (setf in-boxes (list+ self-boxes (sort in-boxes '< :key 'indice)))
	  (setf symbols (mapcar #'(lambda (thein) (setf (in-symbol thein) (gensym))) in-boxes))
	  (setf *let-list* nil) 
	  (setf body `(values ,.(mapcar #'(lambda (theout)
					              (gen-code theout 0)) out-box)))						  
	  (setf patch-code `(defun ,(intern (string out-symb) :om)  (,.symbols)
	      (let* ,(reverse *let-list*) ,body)))
	  (setf *let-list* oldletlist)
	  (setf *lambda-context* oldlambdacontext)    
	   patch-code ;===> returns the function code
	 )))	
	)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SPECIAL-LAMBDA-VALUE TEST 
#|
;screamer
(defmethod special-lambda-value ((self OMBoxPatch) symbol)
  "Eval a patch box in lambda mode."
  (let* ((nesymbs nil)
         (args  (mapcar #'(lambda (input)
                            (if (connected? input)
                              `',(omNG-box-value  input)
                              (let ((newsymbol (gensym)))
                                (push newsymbol nesymbs)
                                newsymbol))) (inputs self))))
	(if (non-deter-patch? (reference self)) (print "NON-DETER-PATCH") (print "NORMAL-PATCH"))							
    (if (null nesymbs)
      symbol
       (eval`#'(lambda ,(reverse nesymbs)
		(case *screamer-valuation*
		 (0 (s::one-value (apply (fdefinition ',(intern (string (code (reference self))) :om)) (list ,.args))))
		 (1 (s::all-values (apply (fdefinition ',(intern (string (code (reference self))) :om)) (list ,.args))))		 
		 (2 (s::print-values (apply (fdefinition ',(intern (string (code (reference self))) :om)) (list ,.args)))))))
   )
  )
 )				   	  

;screamer
(defmethod curry-lambda-code ((self OMBoxPatch) symbol)
 "Eval a patch box in lambda mode."

 (let ((nesymbs nil)
       (oldlambdacontext *lambda-context*))
   (setf *lambda-context* t)

(unwind-protect 	 
 (let* ((nesymbs nil)
        (args  (mapcar #'(lambda (input)
                           (if (connected? input)
                             `',(omNG-box-value  input)
                             (let ((newsymbol (gensym)))
                               (push newsymbol nesymbs)
                               newsymbol))) (inputs self))))
   (if (null nesymbs)
     symbol
	`#'(lambda ,(reverse nesymbs)
		(case *screamer-valuation*
		 (0 (s::one-value (apply (fdefinition ',(intern (string (code (reference self))) :om)) (list ,.args))))
		 (1 (s::all-values (apply (fdefinition ',(intern (string (code (reference self))) :om)) (list ,.args))))		 
		 (2 (s::print-values (apply (fdefinition ',(intern (string (code (reference self))) :om)) (list ,.args))))))
	)
  )		 		 
 (setf *lambda-context* oldlambdacontext)	
 )))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SCREAMER-PATCH-CODE

(defmethod compile-screamer-patch ((self OMPatch)) 
"Lisp code generation from a nondeterministic patch in lambda mode."
(unless (compiled? self)
 (if (lisp-exp-p self)
  (let ((out-symb  (code self)))
     (eval `(screamer::defun ,(intern (string out-symb) :om) (,.(second (get-lisp-exp (lisp-exp self))))	  
             (case *screamer-valuation*
             (0 (s::one-value ,.(cddr (get-lisp-exp (lisp-exp self)))))
             (1 (s::all-values ,.(cddr (get-lisp-exp (lisp-exp self)))))
             (2 (s::print-values ,.(cddr (get-lisp-exp (lisp-exp self)))))))))        						
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
	    (setf body `(case *screamer-valuation* ; => new-body
                     (0 (s::one-value  ,.(mapcar #'(lambda (theout)
               		                     (gen-code theout 0)) out-box)))
                     (1 (s::all-values   ,.(mapcar #'(lambda (theout)
              	 	                       (gen-code theout 0)) out-box)))
                     (2 (s::print-values  ,.(mapcar #'(lambda (theout)
           			                        (gen-code theout 0)) out-box)))))
	  (eval `(screamer::defun ,(intern (string out-symb) :om)  (,.symbols)
	                            (let* ,(reverse *let-list*) ,body)))									  	              
	  (setf *let-list* oldletlist)
	  (setf *lambda-context* oldlambdacontext)))	  
 (setf (compiled? self) t)))

|#

(defmethod screamer-patch-code ((self OMPatch)) 
"Prints the lisp code from a clone of a nondeterministic patch in itself mode."
(let ((patch-clone (clone self)))

(if (lisp-exp-p patch-clone)

    (let ((out-symb  (code patch-clone)))
	
     (setf screamer-patchfun `(screamer::defun ,(intern (string out-symb) :om) (,.(second (get-lisp-exp (lisp-exp patch-clone))))
                                               ;,.(cdr (get-lisp-exp (lisp-exp self)))))) ;(lisp-exp-p self))))))	  
(case *screamer-valuation*
     (0 (s::one-value ,.(cddr (get-lisp-exp (lisp-exp patch-clone)))))
     (1 (s::all-values ,.(cddr (get-lisp-exp (lisp-exp patch-clone)))))
     (2 (s::print-values ,.(cddr (get-lisp-exp (lisp-exp patch-clone))))))))

     (compile (eval screamer-patchfun)) ;===> compile-eval function 
  
     out-symb   ;===> returns the function name

     ;screamer-patchfun ;===> returns the function code
    )
              						
(let* ((boxes (boxes patch-clone))
   (temp-out-box (find-class-boxes boxes 'OMtempOut))
   (self-boxes (patch-has-temp-in-p patch-clone)) 
   (out-box (find-class-boxes boxes 'OMout))
   (in-boxes (find-class-boxes boxes 'OMin))
   (out-symb (code patch-clone))
   (oldletlist *let-list*)
   (oldlambdacontext *lambda-context*)		 
  symbols body)
  (setf out-box (list+ temp-out-box (sort out-box '< :key 'indice)))
  (setf in-boxes (list+ self-boxes (sort in-boxes '< :key 'indice)))
  (setf symbols (mapcar #'(lambda (thein) (setf (in-symbol thein) (gensym))) in-boxes))
  (setf *let-list* nil) 
  (setf body `(case *screamer-valuation* ; => new-body
                                 (0 (s::one-value  ,.(mapcar #'(lambda (theout)
			               		(gen-code theout 0)) out-box)))
                                 (1 (s::all-values   ,.(mapcar #'(lambda (theout)
			              	 	(gen-code theout 0)) out-box)))
                                 (2 (s::print-values  ,.(mapcar #'(lambda (theout)
			           			(gen-code theout 0)) out-box)))))
  (setf screamer-patchfun `(screamer::defun ,(intern (string out-symb) :om)  (,.symbols)
                            (let* ,(reverse *let-list*) ,body)))
									
  (compile (eval screamer-patchfun)) ;===> compile-eval function   
	              
  (setf *let-list* oldletlist)
  (setf *lambda-context* oldlambdacontext)   

 out-symb   ;===> returns the function name

 ;screamer-patchfun ;===> returns the function code
	
 ))) 
)


(defmethod print-screamer-patch-code ((self OMPatch)) 
"Prints the lisp code from a clone of a nondeterministic patch in itself mode."
(let ((patch-clone (clone self)))

(if (lisp-exp-p patch-clone)

    (let ((out-symb  (code patch-clone)))
	
     (setf screamer-patchfun `(screamer::defun ,(intern (string out-symb) :om) (,.(second (get-lisp-exp (lisp-exp patch-clone))))
                                               ;,.(cdr (get-lisp-exp (lisp-exp self)))))) ;(lisp-exp-p self))))))	  
(case *screamer-valuation*
     (0 (s::one-value ,.(cddr (get-lisp-exp (lisp-exp patch-clone)))))
     (1 (s::all-values ,.(cddr (get-lisp-exp (lisp-exp patch-clone)))))
     (2 (s::print-values ,.(cddr (get-lisp-exp (lisp-exp patch-clone))))))))

     ;(compile (eval screamer-patchfun)) ;===> compile-eval function 
  
     ;out-symb   ;===> returns the function name

     screamer-patchfun ;===> returns the function code
    )
              						
(let* ((boxes (boxes patch-clone))
   (temp-out-box (find-class-boxes boxes 'OMtempOut))
   (self-boxes (patch-has-temp-in-p patch-clone)) 
   (out-box (find-class-boxes boxes 'OMout))
   (in-boxes (find-class-boxes boxes 'OMin))
   (out-symb (code patch-clone))
   (oldletlist *let-list*)
   (oldlambdacontext *lambda-context*)		 
  symbols body)
  (setf out-box (list+ temp-out-box (sort out-box '< :key 'indice)))
  (setf in-boxes (list+ self-boxes (sort in-boxes '< :key 'indice)))
  (setf symbols (mapcar #'(lambda (thein) (setf (in-symbol thein) (gensym))) in-boxes))
  (setf *let-list* nil) 
  (setf body `(case *screamer-valuation* ; => new-body
                                 (0 (s::one-value  ,. (mapcar #'(lambda (theout)
			               		(gen-code theout 0)) out-box)))
                                 (1 (s::all-values   ,. (mapcar #'(lambda (theout)
			              	 	(gen-code theout 0)) out-box)))
                                 (2 (s::print-values  ,. (mapcar #'(lambda (theout)
			           			(gen-code theout 0)) out-box)))))
  (setf screamer-patchfun `(screamer::defun ,(intern (string out-symb) :om)  (,.symbols)
                            (let* ,(reverse *let-list*) ,body)))
									
  ;(compile (eval screamer-patchfun)) ;===> compile-eval function   
	              
  (setf *let-list* oldletlist)
  (setf *lambda-context* oldlambdacontext)   

 ;out-symb   ;===> returns the function name

 screamer-patchfun ;===> returns the function code
	
 ))) 
)
	
#|
(defmethod compile-screamer-patch ((self OMPatch)) ; QUESTION MARK DISAPPEARS
"Prints the lisp code from a clone of a nondeterministic patch in itself mode."
(if (lisp-exp-p self)
    (let ((out-symb  (code self)))	
     (setf screamer-patchfun `(screamer::defun ,(intern (string out-symb) :om) (,.(second (get-lisp-exp (lisp-exp self))))                                             	  
							   (case *screamer-valuation*
							     (0 (s::one-value ,.(cddr (get-lisp-exp (lisp-exp self)))))
							     (1 (s::all-values ,.(cddr (get-lisp-exp (lisp-exp self)))))
							     (2 (s::print-values ,.(cddr (get-lisp-exp (lisp-exp self))))))))
     (compile (eval screamer-patchfun)) ;===> compile-eval function 
    )              						
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
  (setf body `(case *screamer-valuation* ; => new-body
                                 (0 (s::one-value  ,. (mapcar #'(lambda (theout)
			               		(gen-code theout 0)) out-box)))
                                 (1 (s::all-values   ,. (mapcar #'(lambda (theout)
			              	 	(gen-code theout 0)) out-box)))
                                 (2 (s::print-values  ,. (mapcar #'(lambda (theout)
			           			(gen-code theout 0)) out-box)))))
  (setf screamer-patchfun `(screamer::defun ,(intern (string out-symb) :om)  (,.symbols)
                            (let* ,(reverse *let-list*) ,body)))									
  (compile (eval screamer-patchfun)) ;===> compile-eval function   	              
  (setf *let-list* oldletlist)
  (setf *lambda-context* oldlambdacontext)
  (setf (compiled? self) t)   
 )))	


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SPECIAL-LAMBDA-VALUE / CURRY-LAMBDA-CODE
;;; => ADAPTED FROM OMLOOP <= ;;;
		 
;screamer
(defmethod special-lambda-value ((self OMBoxPatch) symbol)
  "Eval a patch box in lambda mode."
  (let* ((nesymbs nil)
         (args  (mapcar #'(lambda (input)
                            (if (connected? input)
                              `',(omNG-box-value  input)
                              (let ((newsymbol (gensym)))
                                (push newsymbol nesymbs)
                                newsymbol))) (inputs self))))
    (if (null nesymbs)
      symbol
   (let ((screamer-patchfun (eval (screamer-patch-code (reference self))))
         (qargs (loop for val in args collect (if (omlistp val) `',val val))) ) 
       (eval `#'(lambda ,(reverse nesymbs)
               (apply ',screamer-patchfun (list ,.qargs))))))))	
		   ;`#'(lambda ,(reverse nesymbs)
		   ;     (apply (fdefinition ',(intern (string (code (reference self))) :om)) (list ,.args)))))				   	  

;screamer
(defmethod curry-lambda-code ((self OMBoxPatch) symbol)
 "Eval a patch box in lambda mode."

 (let ((nesymbs nil)
       (oldlambdacontext *lambda-context*))
   (setf *lambda-context* t)

(unwind-protect 	 
 (let* ((nesymbs nil)
        (args  (mapcar #'(lambda (input)
                           (if (connected? input)
                             `',(omNG-box-value  input)
                             (let ((newsymbol (gensym)))
                               (push newsymbol nesymbs)
                               newsymbol))) (inputs self))))
   (if (null nesymbs)
     symbol
   (let ((screamer-patchfun (eval (screamer-patch-code (reference self))))
         (qargs (loop for val in args collect (if (omlistp val) `',val val))) ) 

           `#'(lambda ,(reverse nesymbs)
               (apply ',screamer-patchfun  (list ,.qargs))))))
		   ;`#'(lambda ,(reverse nesymbs)
		   ;     (apply (fdefinition ',(intern (string (code (reference self))) :om)) (list ,.args)))))
 (setf *lambda-context* oldlambdacontext)	

 )))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OMLISPPATCH

(defvar *om-screamerfuns* ;SCREAMER FUNCTIONS
 (list  
	'screamer::one-value
	'screamer::all-values
	'screamer::print-values
	'screamer::a-boolean
	'screamer::an-integer
	'screamer::an-integer-above
	'screamer::an-integer-below
	'screamer::an-integer-between
	'screamer::a-member-of
	'screamer::boolean
	'screamer::booleanp
	'screamer::make-variable
	'screamer::numberpv
	'screamer::realpv
	'screamer::integerpv
	'screamer::booleanpv
	'screamer::memberv
	'screamer::assert!
	'screamer::=v
	'screamer::<v
	'screamer::<=v
	'screamer::>v
	'screamer::>=v
	'screamer::/=v
	'screamer::a-booleanv
	'screamer::an-integerv
	'screamer::an-integer-abovev
	'screamer::an-integer-belowv
	'screamer::an-integer-betweenv
	'screamer::a-realv
	'screamer::a-real-abovev
	'screamer::a-real-belowv
	'screamer::a-real-betweenv
	'screamer::a-numberv
	'screamer::a-member-ofv
	'screamer::notv
	'screamer::andv
	'screamer::orv
	'screamer::+v
	'screamer::-v
	'screamer::*v
	'screamer::/v
	'screamer::minv
	'screamer::maxv
	'screamer::funcallv
	'screamer::applyv
	'screamer::equalv
	'screamer::linear-force
	'screamer::divide-and-conquer-force
	'screamer::static-ordering
	'screamer::domain-size
	'screamer::range-size
	'screamer::reorder
	'screamer::solution
	))

#|
(defvar *om-screamerplusfuns* ;SCREAMERPLUS FUNCTIONS	   
 (list   
	'screamer+::listpv 
	'screamer+::conspv 
	'screamer+::symbolpv 
	'screamer+::stringpv 
	'screamer+::typepv 
	'screamer+::a-listv 
	'screamer+::a-consv 
	'screamer+::a-symbolv
	'screamer+::a-stringv 
	'screamer+::a-typed-varv 
	'screamer+::impliesv 
	'screamer+::not-equalv 
	'screamer+::ifv 
	'screamer+::make-equal
	'screamer+::carv 
	'screamer+::cdrv 
	'screamer+::consv 
	'screamer+::firstv 
	'screamer+::secondv 
	'screamer+::thirdv 
	'screamer+::fourthv 
	'screamer+::restv 
	'screamer+::nthv      
	'screamer+::subseqv
	'screamer+::lengthv 
	'screamer+::appendv 
	'screamer+::make-listv 
	'screamer+::all-differentv 
	'screamer+::set-equalv 
	'screamer+::subsetpv
	'screamer+::intersectionv 
	'screamer+::unionv 
	'screamer+::bag-equalv 
	'screamer+::make-arrayv 
	'screamer+::arefv 
	'screamer+::make-instancev 
	'screamer+::classpv
	'screamer+::slot-valuev 
	'screamer+::class-ofv 
	'screamer+::class-namev  
	'screamer+::slot-exists-pv 
	'screamer+::reconcile
	'screamer+::funcallinv 
	'screamer+::mapcarv 
	'screamer+::maplistv 
	'screamer+::everyv 
	'screamer+::somev 
	'screamer+::noteveryv 
	'screamer+::notanyv
	'screamer+::at-leastv 
	'screamer+::at-mostv 
	'screamer+::exactlyv 
	'screamer+::constraint-fn
	'screamer+::slot-names-of 
	'screamer+::objectp 
	'screamer+::eqv 
	'screamer+::funcallgv 
    'screamer+::setq-domains))
|#
	
(defun screamer-symb? (x)
 (or (not (null (find-symbol (string x) :screamer)))))
    ;(not (null (find-symbol (string x) :screamer+))))) 
		 
(defmethod compile-before-close ((self patch-lambda-exp-window))
 (handler-bind ((error #'(lambda (c) 
                           (declare (ignore c))
                           (setf *patch-definition-aborted* 
                                 (string+ "Error in function definition: " 
                                          (om-report-condition c) 
                                          ".~%~%Close editor anyway?~%(No modification will be made to the patch)."))
                           )))
 (let* ((expression (om-get-lisp-expression self)))
   (unless (lambda-expression-p expression)
     (setf *patch-abort-definition* "Error in lambda expression.~%Lambda expressions are of the form (lambda <param-list> <body>).~%~%Close editor anyway?~%(No modification will be made to the patch)."))
   (unless *patch-abort-definition*
     ;(setf (lisp-exp-p (patchref self)) expression)
     (setf (lisp-exp (patchref self)) (om-get-text self))
     (let* ((exp (cdr (get-lisp-exp (lisp-exp (patchref self)))))
             (symbols  (loop for x in (remove nil (flat exp)) 
                                if (and (symbolp x) (screamer-symb? x)) collect x))
              (scream?  (remove-if-not (lambda (x) (member x *om-screamerfuns*)) symbols)))
  (if scream?
 (eval `(screamer::defun ,(intern (string (code (patchref self))) :om)
                   ,.(cdr (get-lisp-exp (lisp-exp (patchref self))))))
     (eval `(defun ,(intern (string (code (patchref self))) :om)
                   ,.(cdr (get-lisp-exp (lisp-exp (patchref self)))))))))
     )))

(defmethod compile-without-close ((self patch-lambda-exp-window))
 (let* ((expression (om-get-lisp-expression self)))
  (unless (lambda-expression-p expression)
    (om-message-dialog (string+ "Error! The expression in the Lisp patch" (name (patchref self)) " is not a valid lambda expression. 
 Lambda expression are of the form (lambda <param-list> <body>)"))
    (om-abort))
  (unless (equal (get-lisp-exp (lisp-exp (patchref self))) expression)
    ;;;(setf (lisp-exp-p (patchref self)) expression)
    (setf (lisp-exp (patchref self)) (om-get-text self))
     (let* ((exp (cdr (get-lisp-exp (lisp-exp (patchref self)))))
             (symbols  (loop for x in (remove nil (flat exp)) 
                                if (and (symbolp x) (screamer-symb? x)) collect x))
              (scream?  (remove-if-not (lambda (x) (member x *om-screamerfuns*)) symbols)))
  (if scream?
   (compile-lisp-patch-screamerfun self)
   (compile-lisp-patch-fun self)))
    (loop for item in (attached-objs (patchref self)) do
          (update-from-reference item)))
  (setf (compiled? (patchref self)) t)))	

(defun compile-lisp-patch-screamerfun (patch)
(if (get-lisp-exp (lisp-exp patch))
 (eval `(screamer::defun ,(intern (string (code patch)) :om)
               ,.(cdr (get-lisp-exp (lisp-exp patch)))))
(eval `(screamer::defun ,(intern (string (code patch)) :om) () nil))))

(defmethod compile-patch ((self OMLispPatch)) 
"Generation of lisp code from the graphic boxes."
(unless (compiled? self)
(handler-bind 
   ((error #'(lambda (err)
               (capi::display-message "An error of type ~a occurred: ~a" (type-of err) (format nil "~A" err))
               (abort err))))
     (let* ((exp (cdr (get-lisp-exp (lisp-exp self))))
             (symbols  (loop for x in (remove nil (flat exp)) 
                                if (and (symbolp x) (screamer-symb? x)) collect x))
              (scream?  (remove-if-not (lambda (x) (member x *om-screamerfuns*)) symbols)))
  (if scream?
   (compile-lisp-patch-screamerfun self)
   (compile-lisp-patch-fun self)))
 (setf (compiled? self) t))
))

(defmethod omNG-copy ((self OMLispPatch))
`(let ((copy ,(call-next-method)))
    (setf (lisp-exp copy) (lisp-exp ,self))
     (let* ((exp (cdr (get-lisp-exp (lisp-exp copy))))
             (symbols  (loop for x in (remove nil (flat exp)) 
                                if (and (symbolp x) (screamer-symb? x)) collect x))
              (scream?  (remove-if-not (lambda (x) (member x *om-screamerfuns*)) symbols)))
  (if scream?
    (compile-lisp-patch-screamerfun copy)
    (compile-lisp-patch-fun copy)))
   copy))
 
(defmethod om-save ((self OMLispPatchAbs) &optional (values? nil))
  "Generation of code to save 'self'."
     (let* ((exp (cdr (get-lisp-exp (lisp-exp self))))
             (symbols  (loop for x in (remove nil (flat exp)) 
                                if (and (symbolp x) (screamer-symb? x)) collect x))
              (scream?  (remove-if-not (lambda (x) (member x *om-screamerfuns*)) symbols)))
  (if scream?
  `(om-load-lisp-abs-nondeterpatch ,(name self) ,*om-version* ,(str-without-nl (lisp-exp self)))
  `(om-load-lisp-abspatch ,(name self) ,*om-version* ,(str-without-nl (lisp-exp self))) )))

(defun om-load-lisp-abs-nondeterpatch (name version expression)
 (let ((newpatch (make-instance 'OMLispPatchAbs :name name :icon 123)))
   (setf (omversion newpatch) version)
   (setf (lisp-exp newpatch) (get-lisp-str expression))
     (let* ((exp (cdr (get-lisp-exp (lisp-exp newpatch))))
             (symbols  (loop for x in (remove nil (flat exp)) 
                                if (and (symbolp x) (screamer-symb? x)) collect x))
              (scream?  (remove-if-not (lambda (x) (member x *om-screamerfuns*)) symbols)))
  (if scream?
   (compile-lisp-patch-screamerfun newpatch)
   (compile-lisp-patch-fun newpatch))) 
   newpatch))

(defmethod get-patch-inputs ((self OMLispPatch))
 (unless (compiled? self)
     (let* ((exp (cdr (get-lisp-exp (lisp-exp self))))
             (symbols  (loop for x in (remove nil (flat exp)) 
                                if (and (symbolp x) (screamer-symb? x)) collect x))
              (scream?  (remove-if-not (lambda (x) (member x *om-screamerfuns*)) symbols)))
  (if scream?
   (compile-lisp-patch-screamerfun self)
   (compile-lisp-patch-fun self))))
 (let* ((args (arglist (intern (string (code self)) :om)))
        (numins (min-inp-number-from-arglist args)) (i -1))
   (mapcar #'(lambda (name) 
               (make-instance 'omin
                              :indice (incf i)
                              :name (string name))) 
           (subseq args 0 numins))))


#|
(defvar *ScreamerLispPatch* 1)
		   
(defmethod handle-key-event ((self patchPanel) char) 
     (modify-patch self)
     (let* ((actives (get-actives self))
            (activeboxes (mapcar 'object actives)))

       (when (and (char-num-p char) actives (not (equal char #\0)))
         (let ((boxes
                (remove-if-not #'(lambda (item) (or (boxframe-p item) (boxeditorframe-p item))) actives)))
           (loop for i in boxes
                 do (let ((input (nth (1- (digit-char-p char)) (inputframes i))))
                      (when input
                        (if (connected? (object input))
                            (unless (member *target-out* (outframes i))
                              (disconnect-box i input))
                          (connect-box *target-out* input))))) 
           ))

       (case char
         (:om-key-delete (delete-general self))
         (#\? (if (= 1 *ScreamerLispPatch*) (progn (print "NONDETERMINISTIC LISP PATCH ON") (setf *ScreamerLispPatch* 0))
	          (if (= 0 *ScreamerLispPatch*) (progn (print "NONDETERMINISTIC LISP PATCH OFF") (setf *ScreamerLispPatch* 1))
                   (om-beep))))
         (#\f (make-undefined-funct-box self (om-mouse-position self)))
         (#\c (if actives (edit-comment-box actives)
                (make-comment-box self (om-mouse-position self))))
         (#\x (create-comment-box self (om-mouse-position self)))
         (#\d  (mapc 'show-big-doc actives))
         (#\D (mapc 'update-doc actives))
         (#\C  (patch-color self))
         (#\F (font-comments self))
         (#\e (mapc 'show-fun-code actives))
         (#\g (getdacode self))
         (#\v  (om-eval-enqueue 
                `(progn
                   (setf *cur-eval-panel* ,self)
                   (mapc 'eval-box ',actives)
                   (clear-ev-once ,self))
                ))
         (#\h  (show-help-window (format nil "Commands for ~A Editor" 
                                         (string-upcase (class-name (class-of (object (editor self))))))
                                 (get-help-list (editor self)) 410)) 
         (#\H (mapc 'open-helpfile actives))
         ;;; in the menu
         (#\k (mapc 'add-keywords actives))
         (#\K (mapc 'erase-keywords actives))
         (#\I (mapc 'reinit-size actives) 
              (reinit-connections self)
              (reinit-bg-picts self))
         (#\R (mapc 'reinit-contents actives))

         (#\b (mapc 'add-rem-lock-button actives))
         (#\l (mapc 'add-rem-lambda-button actives))
         (#\o (mapc 'add-rem-evonce-button actives))
         (#\i (mapc 'add-rem-itself-button actives))
         (#\a (mapc 'internalize-patch actives))
         (#\A (mapc 'align-one-boxframe actives)
              (make-move-after self actives))

         (#\p (play-boxes activeboxes) (mapcar 'om-invalidate-view actives))
         (#\s (stop-boxes activeboxes) (mapcar 'om-invalidate-view actives))      
         (#\Space (if (idle-p *general-player*)
                      (play-boxes activeboxes)
                    (stop-all-boxes))
                  (mapcar 'om-invalidate-view actives))

         (#\t (mapc 'show-online-tutorial actives))

         (#\m (mapc 'change-edit-mode actives))
         (#\n (mapc 'set-show-box-name actives))
         (#\M (change-edit-mode-all (get-subframes self)))
         (#\z (if *curved-connections*
                  (setf *curved-connections* nil)
                (setf *curved-connections* t)))

         (:om-key-up 
          (if (and (om-command-key-p) (om-shift-key-p))
              (mapc 'box-resize-y-minus actives)
            (progn
              (mapc #'(lambda (item) (move-frame-delta item 0)) actives)
              (make-move-after self actives))))
         (:om-key-down 
          (if (and (om-command-key-p) (om-shift-key-p))
              (mapc 'box-resize-y-plus actives)
            (progn
              (mapc #'(lambda (item) (move-frame-delta item 1)) actives)
              (make-move-after self actives))))
         (:om-key-left
          (cond 
           ((om-option-key-p)
            (mapc #'(lambda (item) (delete-all-inputs item) t) actives))
           ((and (om-command-key-p) (om-shift-key-p))
            (mapc 'box-resize-x-minus actives))
           (t
            (progn
              (mapc #'(lambda (item) (move-frame-delta item 3)) actives)
              (make-move-after self actives)
              ))
           ))
         (:om-key-right 
          (cond 
           ((om-option-key-p)
            (mapc #'(lambda (item) (add-all-inputs item)) actives))
           ((and (om-command-key-p) (om-shift-key-p))
            (mapc 'box-resize-x-plus actives))
           (t
            (progn (mapc #'(lambda (item) (move-frame-delta item 2)) actives)
              (make-move-after self actives)))))

         (#\< (mapc #'(lambda (item) (delete-one-input item)) actives))
         (#\> (mapc #'(lambda (item) (add-one-input item)) actives))

         (#\r #+om-reactive(mapc #'(lambda (boxframe) 
                                     (set-active (object boxframe) (not (active (object boxframe))))
                                     (om-invalidate-view boxframe))
                                 actives))

         (#\E (om-encapsulate self actives))
         (#\U (om-unencapsulate self actives))

         (otherwise (loop
   		    with hotbox = nil
   		    for box in activeboxes
                   
                       do (when (find-method #'handle-key-event '() (list (class-of box)
                                                                          (find-class t)) nil)
                            (setf hotbox t)  
                            (handle-key-event box char))
                       finally
                       ;do 
                         (unless hotbox 
                           (om-beep)			    ;no boxes have specialized handle-key-event methods
                           ))))))

|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TODO - OMLOOP

;(defmethod get-icon-box-class ((self omloop-box)) 'omloop-icon-box) ??

;;; PRINT OMLOOP FUNCTION FOR TESTING ;;;
#|
(defmethod compile-patch ((self patchForLoop))
   "Code generates by Loop patches is generate by this method."
(let ((omloop-clone (clone self)))
   (let* ((boxes (boxes omloop-clone))
          (oldletlist *let-list*)
          (*let-list* nil)
          (oldlambdacontext *lambda-context*)
          (do-box (car (find-class-boxes boxes 'OMLoopDo)))
          (init-box (car (find-class-boxes boxes 'OMinitDo)))
          (final-box (car (find-class-boxes boxes 'OMFinalDo)))
          (in-boxes (sort (find-class-boxes boxes 'OMin) '< :key 'indice))
          (symbols (mapcar #'(lambda (thein) (setf (in-symbol thein) (gensym))) in-boxes))
          (loop-boxes (find-loop-boxes boxes))
          (loop-code (mapcan #'(lambda (theout)
                                 (loop-gen-code theout 0)) loop-boxes))
          (acum-boxes (find-acum-boxes boxes))
          (acum-declaration (mapcar #'(lambda (acumm)
                                        (declare-closure acumm)) acum-boxes))
          (acum-inits (mapcar #'(lambda (acumm)
                                  (gen-code acumm -1)) acum-boxes))
          body init)
 
     (setf init (gen-code init-box 0))
     (setf body (gen-code do-box 0))	 
     (setf final (loop for i from 0 to (1- (length (inputs final-box))) collect (gen-code final-box i)))
     ;(eval 
	(setf omloop-code (gen-loop-function (intern (string (car (list! (code omloop-clone)))) :om) 
                          symbols
                          acum-declaration acum-inits init loop-code final 
                          (reverse *let-list*) 
                          body))	
;(eval `(screamer::defun ,(intern (string (first (code omloop-clone))) :om)  (,.symbols)  ; ====> OM 4
;       (let (,.acum-declaration) ,.acum-inits
;            (loop ,.loop-code 
;                  finally (return (values ,.(loop for i from 0 to (- (length (inputs final-box)) 1)
;                                                  collect  (gen-code final-box i)))) do
;                  (let* ,*let-list*
;                    ,body)))))							  			  
				  						  
     (setf *lambda-context* oldlambdacontext)
     (setf *let-list* oldletlist)
	 (print omloop-code) ; =====> returns the omloop code
	 )))
	 
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CODE FROM OMLOOP.LISP => OM 7.2  
#|
;screamer
(defmethod call-gen-code ((self omloop-box) numout)
   (let ((in-list (mapcar #'(lambda (thein) (gen-code thein 0)) (inputs self))))
     (if (zerop numout) 
       `(,(intern (string (first (code (patch self)))) :om) ,.in-list)
       `(nth ,numout (multiple-value-list (,(intern (string (first (code (patch self)))) :om) ,.in-list))))))
	   
;screamer
(defmethod gen-code-call ((self omloop-box) &optional args)
  (let ((in-list (mapcar #'(lambda (thein) (gen-code thein 0)) (inputs self))))
    `(,(intern (string (first (code (patch self)))) :om) ,.in-list)))
	
;screamer
(defmethod special-lambda-value ((self omloop-box) symbol)
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
				  
;screamer
(defmethod curry-lambda-code ((self omloop-box) symbol)
"Lisp code generation for a loop box in lambda mode."
(let* ((nesymbs nil)
       (oldlambdacontext *lambda-context*))

  (setf *lambda-context* t)

  (unwind-protect
      (let ((args  (mapcar #'(lambda (input)
                               (if (connected? input)
                                   (gen-code input 0)
                                 (let ((newsymbol (gensym)))
                                   (push newsymbol nesymbs)
                                   newsymbol))) (inputs self))))
        (if (null nesymbs)
            symbol
          `#'(lambda ,(reverse nesymbs)
               (apply (fdefinition ',(intern (string (first (code (patch self)))) :om)) (list ,.args)))))

    (setf *lambda-context* oldlambdacontext))
  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CODE FROM PATCHBOXES.LISP => OM 7.2  

;screamer
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
		   
 |#	
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;; CODE FROM OMLOOP.LISP => OM 4 (DISABLED)
 
 ;;screamer
 ;(defmethod special-value ((self OMLoop-Box) &optional (args nil))
 ;   (let* (non-deter?)
 ;     (unless (compiled? (patch self)) 
 ;       (compile-patch (patch self))
 ;       (setf (compiled? (patch self)) t))
 ;     (setf non-deter? (non-deter-patch? (patch self)))
 ;     (if non-deter?
 ;       (case *screamer-valuation*
 ;         (0 (eval `(s::one-value (,(intern (string (first (code (patch self)))) :om) ,.args))))
 ;         (1 (eval `(s::all-values (,(intern (string (first (code (patch self)))) :om) ,.args))))  
 ;         (2 (eval `(s::print-values (,(intern (string (first (code (patch self)))) :om) ,.args)))))
 ;       (apply (fdefinition (intern (string (first (code (patch self)))) :om)) args)))) 

 ;;screamer
 ;(defmethod compile-patch ((self patchForLoop))
 ;   "Code generates by Loop patches is generate by this method."
 ;   (let* ((boxes (boxes self))
 ;          (oldletlist *let-list*)
 ;          (*let-list* nil)
 ;          (do-box (car (find-class-boxes boxes 'OMLoopDo)))
 ;          (final-box (car (find-class-boxes boxes 'OMFinalDo)))
 ;          (in-boxes (sort (find-class-boxes boxes 'OMin) '< :key 'indice))
 ;          (symbols (mapcar #'(lambda (thein) (setf (in-symbol thein) (gensym))) in-boxes))
 ;          (loop-boxes (find-loop-boxes boxes))
 ;          (loop-code (mapcan #'(lambda (theout)
 ;                                 (loop-gen-code theout 0)) loop-boxes))
 ;          (acum-boxes (find-acum-boxes boxes))
 ;          (acum-declaration (mapcar #'(lambda (acumm)
 ;                                        (declare-closure acumm)) acum-boxes))
 ;          (acum-inits (mapcar #'(lambda (acumm)
 ;                                  (gen-code acumm -1)) acum-boxes))
 ;          body)
 ;     (setf body (gen-code do-box 0))
 ;     (eval `(screamer::defun ,(intern (string (first (code self))) :om)  (,.symbols) 
 ;              (let (,.acum-declaration) ,.acum-inits
 ;                   (loop ,.loop-code 
 ;                         finally (return (values ,.(loop for i from 0 to (- (length (inputs final-box)) 1)
 ;                                                         collect  (gen-code final-box i)))) do
 ;                         (let* ,*let-list*
 ;                           ,body)))))
 ;     (setf *let-list* oldletlist)))

 ;;screamer
 ;(defmethod draw-after-box ((self loopBoxFrame))
 ;   (with-fore-color 16719095
 ;     (when (non-deter-patch? (patch (object self)))
 ;       (#_Textsize 28)
 ;       (draw-char (- (round (w self) 2) 8) (+ (round (h self) 2) 6) #\? ))))	
