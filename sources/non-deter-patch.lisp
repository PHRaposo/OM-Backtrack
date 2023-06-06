(in-package :om)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	  
;;;;;;;;;;; OM-MODIFS ADAPTED TO OM 7.2

(defmethod non-deter-patch? ((self OMPatch)) 
(let ((record (s::get-function-record (intern (string (car (list! (code self)))) :om))))
(not (s::function-record-deterministic? record))))

#|
(defmethod non-deter-patch? ((self OMLispPatch)) ; needs a better solution to prevent crashes
(if (or (null (get-lisp-exp (lisp-exp self)))
	(and (null (first (cdr (get-lisp-exp (lisp-exp self)))))
             (equal (second (cdr (get-lisp-exp (lisp-exp self))))'(ombeep))))
    (declare (ignore self)) 
(let* ((fun (eval `(screamer::defun ,(intern (string (code self)) :om)
              ,.(cdr (get-lisp-exp (lisp-exp self))))))
         (record (s::get-function-record fun)))
(not (s::function-record-deterministic? record)))))
|#

(defmethod omNG-box-value ((self OMBoxPatch) &optional (num-out 0))
(handler-bind ((error #'(lambda (c)
                          (when *msg-error-label-on*
                            (om-message-dialog (string+ "Error while evaluating the box " (string (name self)) " : " 
                                                     (om-report-condition c))
                                               :size (om-make-point 300 200))
                            (clear-after-error self)
                            (om-abort)))))
   (cond
   ((and (equal (allow-lock self) "l") (non-deter-patch? (reference self)))
          ;compile function -> special-lambda-value -> for nondeterministic patches
          (om-message-dialog "Nondeterministic patches in lambda mode has not been implemented yet.")
         (clear-after-error self)
         (om-abort))		   
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
		             ;(special-lambda-value self (intern (string (code (reference self))) :om)))
			  
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
         (eval `(screamer::defun ,(intern (string out-symb) :om)  (,.symbols)
		 				 (let* ,(reverse *let-list*) ,body)))
					 						 		 	 
       (setf *let-list* oldletlist)
       (setf *lambda-context* oldlambdacontext)
	   ))	
(setf (compiled? self) t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SCREAMER-PATCH-CODE

(defmethod screamer-patch-code ((self OMPatch)) 
"Prints the lisp code from a clone of a nondeterministic patch in itself mode."
(let ((patch-clone (clone self)))

(if (lisp-exp-p patch-clone)		
  `(screamer::defun ,(intern (string (code patch-clone)) :om)
            ;,.(cdr (get-lisp-exp (lisp-exp self)))))) ;(lisp-exp-p self))))))		
(case *screamer-valuation*
     (0 (s::one-value ,.(cdr (get-lisp-exp (lisp-exp patch-clone)))))
     (1 (s::all-values ,.(cdr (get-lisp-exp (lisp-exp patch-clone)))))
     (2 (s::print-values ,.(cdr (get-lisp-exp (lisp-exp patch-clone)))))))              						
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
                  
  (setf *let-list* oldletlist)
  (setf *lambda-context* oldlambdacontext)   

  screamer-patchfun ;===> returns the function code	
 ))) 
)	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
          ;    (apply (fdefinition ',(intern (string (code (reference self))) :om)) (list ,.args)))))
 (setf *lambda-context* oldlambdacontext)	

 )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OM-DRAW-CONTENTS - "?" - NONDETERMINISTIC PATCH

#|
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

(defmethod om-draw-contents :after ((self patch-icon-box))
  (when (non-deter-patch? (reference (object (om-view-container self))))
   (om-with-fg-color self *om-pink-color*
    (om-with-font (om-make-font "Courier" (* *icon-size-factor* 34))
      (om-draw-char (- (round (w self) 2) (* *icon-size-factor* 10)) 
		    (+ (round (h self) 2) (* *icon-size-factor* 10)) 
                    #\?)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;;; TODO - OMLOOP - OMLISPPATCH

		 

