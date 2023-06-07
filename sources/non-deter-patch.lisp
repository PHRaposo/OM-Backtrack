(in-package :om)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
    ((and (equal (allow-lock self) "l") 
	     (non-deter-patch? (reference self))
	     (lisp-exp-p (reference self)))       
	      (om-message-dialog "Nondeterministic lisp patches in lambda mode has not been implemented yet.")
              (clear-after-error self)
              (om-abort))
	   
   ((and (equal (allow-lock self) "l") (non-deter-patch? (reference self)))
    	  (om-message-dialog "Nondeterministic patches in lambda mode has not been implemented yet.")
          (clear-after-error self)
          (om-abort))
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
  `(screamer::defun ,(intern (string (code patch-clone)) :om) (,.(second (get-lisp-exp (lisp-exp patch-clone))))
            ;,.(cdr (get-lisp-exp (lisp-exp self)))))) ;(lisp-exp-p self))))))		
(case *screamer-valuation*
     (0 (s::one-value ,.(cddr (get-lisp-exp (lisp-exp patch-clone)))))
     (1 (s::all-values ,.(cddr (get-lisp-exp (lisp-exp patch-clone)))))
     (2 (s::print-values ,.(cddr (get-lisp-exp (lisp-exp patch-clone)))))))              						
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
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OMLispPatch

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
     (eval `(screamer::defun ,(intern (string (code (patchref self))) :om)
                   ,.(cdr (get-lisp-exp (lisp-exp (patchref self))))))
     ))))
	 
 (defmethod compile-without-close ((self patch-lambda-exp-window))
 (let* ((expression (om-get-lisp-expression self)))
  (unless (lambda-expression-p expression)
    (om-message-dialog (string+ "Error! The expression in the Lisp patch" (name (patchref self)) " is not a valid lambda expression. 
 Lambda expression are of the form (lambda <param-list> <body>)"))
    (om-abort))
  (unless (equal (get-lisp-exp (lisp-exp (patchref self))) expression)
    ;;;(setf (lisp-exp-p (patchref self)) expression)
    (setf (lisp-exp (patchref self)) (om-get-text self))
   (compile-lisp-patch-screamerfun self)
   ;(compile-lisp-patch-fun self)
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
   (compile-lisp-patch-screamerfun self)
   ;(compile-lisp-patch-fun self)
 (setf (compiled? self) t))
))

(defmethod omNG-copy ((self OMLispPatch))
`(let ((copy ,(call-next-method)))
    (setf (lisp-exp copy) (lisp-exp ,self))
    (compile-lisp-patch-screamerfun copy)
   copy))
 
(defmethod om-save ((self OMLispPatchAbs) &optional (values? nil))
  "Generation of code to save 'self'."
  `(om-load-lisp-abs-nondeterpatch ,(name self) ,*om-version* ,(str-without-nl (lisp-exp self))))

(defun om-load-lisp-abs-nondeterpatch (name version expression)
 (let ((newpatch (make-instance 'OMLispPatchAbs :name name :icon 123)))
   (setf (omversion newpatch) version)
   (setf (lisp-exp newpatch) (get-lisp-str expression))
   (compile-lisp-patch-screamerfun newpatch)
   newpatch))

(defmethod get-patch-inputs ((self OMLispPatch))
 (unless (compiled? self)
   (compile-lisp-patch-screamerfun self))
 (let* ((args (arglist (intern (string (code self)) :om)))
        (numins (min-inp-number-from-arglist args)) (i -1))
   (mapcar #'(lambda (name) 
               (make-instance 'omin
                              :indice (incf i)
                              :name (string name))) 
           (subseq args 0 numins))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;;; TODO - OMLOOP
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
