;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; REVISED VERSION
;;; Copyright 2024 PAULO HENRIQUE RAPOSO AND KARIM HADDAD

(in-package :om)
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SCREAMERBOXES (FUNCTIONS)

(defclass screamerboxes (OMBoxCall) () 
   (:documentation "Screamer boxes"))

(defmethod screamerboxes-p ((self screamerboxes)) t)
(defmethod screamerboxes-p ((self t)) nil)
			 
(defmethod omNG-box-value ((self screamerboxes) &optional (numout 0))
   "Eval the output indexed by 'numout' for the box 'self'. In this method we call the generic function reference of 'self'."
   (handler-bind ((error #'(lambda (c)
                             (when *msg-error-label-on*
                               (om-message-dialog (string+ "Error while evaluating the box " (string (name self)) " " 
                                                                               (om-report-condition c)
                                                                               )
                                               :size (om-make-point 300 200))
                               (om-abort)))))
     (cond
      ((equal (allow-lock self) "l") 
       (setf (value self) (list (special-lambda-value self (intern (string (reference self)) :s)))) ;;;test with :s package
       (car (value self)))
      ((or ;(equal (allow-lock self) "l") 
           (equal (allow-lock self) "o")  
           (and (equal (allow-lock self) "x") (value self)) 
           (and (equal (allow-lock self) "&") (ev-once-p self))) (call-next-method))
      (t (let* ((args  (loop for input in (inputs self)
                             when (not (keyword-input-p input)) collect (omNG-box-value input)))
                (qargs (loop for val in args collect (if (or (symbolp val) (omlistp val)) `',val val))) 
                (themethod (compute-applicable-methods (fdefinition (reference self)) args)) rep)
           (if (null themethod)
             (progn (dialog-message (string+ "no method is defined for inputs in box " (name self)))
                    (abort))
             (progn
               (when (and (EditorFrame (car themethod)) (not (compiled? (car themethod))))
                 (modify-genfun (EditorFrame (car themethod))))
				 (setf rep (multiple-value-list (eval `(,(intern (string (reference self)) :s) ,.qargs)))))			   
            )
           (when (equal (allow-lock self) "&")
             (setf (ev-once-p self) t)
             (setf (value self) rep))
           (when (equal (allow-lock self) "x")
             (setf (value self) rep))
          (progn (setf (value self) rep) ;;; new for om-backtrack in OM 7.2
               (nth numout rep))))))
             )
#|
(defmethod special-lambda-value ((self screamerboxes) symbol)
"Eval a screamerbox in lambda mode."
 (multiple-value-bind (nesymbs args) (get-args-eval-currry self)
  (eval `#'(lambda ,(reverse nesymbs)
             (case *screamer-valuation* 
               (0 (s::one-value (,symbol ,.args)))
               (1 (s::all-values (,symbol ,.args)))
               (2 (s::print-values (,symbol ,.args))))))))


(defmethod curry-lambda-code ((self screamerboxes) symbol)
  "Lisp code generation for a  screamerbox in lambda mode."

   (let ((nesymbs nil)
         (oldlambdacontext *lambda-context*))
     (setf *lambda-context* t)
     
     (unwind-protect 
         (let ((args (mapcan #'(lambda (input)
                                 (let ((a (if (connected? input)
                                              (gen-code input 0)
                                            (let ((newsymbol (gensym)))
                                              (push newsymbol nesymbs)
                                              newsymbol))))
                                   (if (keyword-input-p input) 
                                       (list (value input) a) 
                                     (list a))))
                             (inputs self))))

    `#'(lambda ,(reverse nesymbs)
             (case *screamer-valuation* 
               (0 (s::one-value (,symbol ,.args)))
               (1 (s::all-values (,symbol ,.args)))
               (2 (s::print-values (,symbol ,.args))))))

       (setf *lambda-context* oldlambdacontext)
  )))
|#
	 			
(defmethod gen-code-call ((self screamerboxes) &optional args)
   (let ((screamerfun `,(intern (string (reference self)) :s)))     
     `(,screamerfun ,.(decode self))))

(defmethod gen-code ((self screamerboxes) numout)
   "Generate Lisp code for the box 'self'."
   (let ((screamerfun `,(intern (string (reference self)) :s)))
     (cond
      ((equal (allow-lock self) "&") 
       (gen-code-for-ev-once self numout))
      ((equal (allow-lock self) "x")
       `(nth ,numout ,(gen-code (value self) 0)))
      ((equal (allow-lock self) "o") 
       `',(reference self))
      ((equal (allow-lock self) "l")
       (curry-lambda-code self screamerfun))
      (t  `(,screamerfun ,.(decode self))))))

;Faire la meme chose pour les autres fontions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; TODO - omNG-copy

#|
(defmethod omNG-copy ((self screamerboxes))
  "Cons a Lisp expression that return a copy of self when it is valuated."
  `(let ((rep (make-instance ',(type-of self)
                             :midiport ,(midiport self)
                             :nbtracks ,(nbtracks self)
                             :port ,(port self))))
     (setf (channels-ctrl rep) (list ,.(loop for ctrl in (channels-ctrl self) collect
                                               (omNG-copy ctrl))))
     (setf (miditrack rep) ',(miditrack self))
     (setf (presets rep) ',(presets self))
     rep
     ))
|#

;;; NEW <=== (OM-BACKTRACK 2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SCREAMER-VALUATION-BOXES 
;;; (SCREAMER MACROS: ONE-VALUE- ALL-VALUES, PRINT-VALUES, ITH-VALUE, N-VALUES AND BEST-VALUE)

(defclass screamer-valuation-boxes (OMBoxCall) () 
   (:documentation "Screamer Valuation boxes"))

(defmethod screamer-valuation-boxes-p ((self screamer-valuation-boxes)) t)
(defmethod screamer-valuation-boxes-p ((self t)) nil)
					 
(defmethod omNG-box-value ((self screamer-valuation-boxes) &optional (numout 0))
   "Eval the output indexed by 'numout' for the box 'self'. In this method we call the generic function reference of 'self'."
   (handler-bind ((error #'(lambda (c)
                             (when *msg-error-label-on*
                               (om-message-dialog (string+ "Error while evaluating the box " (string (name self)) " " 
                                                                               (om-report-condition c)
                                                                               )
                                               :size (om-make-point 300 200))
                               (om-abort)))))
     (cond
      ((equal (allow-lock self) "l") 
       (setf (value self) (list (special-lambda-value self (intern (string (reference self)) :s)))) ;;;test with :s package
       (car (value self)))
      ((or ;(equal (allow-lock self) "l") 
           (equal (allow-lock self) "o")  
           (and (equal (allow-lock self) "x") (value self)) 
           (and (equal (allow-lock self) "&") (ev-once-p self))) (call-next-method))
      (t (let ((theinputs (loop for i in (inputs self) ;<=== FROM OMOut (gen-code method -> in-out-boxes.lisp)
	                        collect (connected? i)))
	      (oldletlist *let-list*)
	       themethod code qargs form rep)
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  (setf *let-list* nil) ;<=== TEST (RESET *LET-LIST* BEFORE CODE GNERATION)
				
	  (setf code (loop for box in theinputs
			   collect (if box (gen-code (first box) (second box)) 'nil)))			
		   					 
	  (setf qargs (loop for val in code collect (if (or (symbolp val) (omlistp val)) `',val val)))
				
	  (setf themethod (compute-applicable-methods (fdefinition (reference self)) qargs))
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	            (if (null themethod)
	                (progn (dialog-message (string+ "no method is defined for inputs in box " (name self)))
	                       (abort))
                    (progn
                     (when (and (EditorFrame (car themethod)) (not (compiled? (car themethod))))
                      (modify-genfun (EditorFrame (car themethod))))
					(setf form (let ((valuation (string (reference self))))
					            (cond ((or (equal valuation "ALL-VALUES") 
									       (equal valuation "ONE-VALUE"))
										  `(,(intern (string (reference self)) :s)
										    (let* ,(reverse *let-list*) ,.qargs)))	
									  ((or (equal valuation "N-VALUES") 
									       (equal valuation "ITH-VALUE")) 
										  `(,(intern (string (reference self)) :s) ,(car qargs)
										    (let* ,(reverse *let-list*) ,.(cdr qargs))))
									 (t ;"BEST-VALUE"
									  `(let* ,(reverse *let-list*)
									  (,(intern (string (reference self)) :s) ,.qargs)))))) 
			   ;(print form) <=== check code
			   (setf rep (multiple-value-list (eval form)))
		       ;(setf rep (multiple-value-list (eval `(let* ,(reverse *let-list*) 
			   ;	                                      (,(intern (string (reference self)) :s) ,.qargs)))))
			))
		   (setf *let-list* oldletlist)
           (when (equal (allow-lock self) "&")
             (setf (ev-once-p self) t)
             (setf (value self) rep))
           (when (equal (allow-lock self) "x")
             (setf (value self) rep))
          (progn (setf (value self) rep) ;;; new for om-backtrack in OM 7.2
               (nth numout rep))))))
             )
			 
 (defmethod gen-code-call ((self screamer-valuation-boxes) &optional args)
    (let ((screamerfun `,(intern (string (reference self)) :s)))     
      `(,screamerfun ,.(decode self))))
				  
  (defmethod gen-code ((self screamer-valuation-boxes) numout)
     "Generate Lisp code for the box 'self'."
     (let ((screamerfun `,(intern (string (reference self)) :s)))
       (cond
        ((equal (allow-lock self) "&") 
         (gen-code-for-ev-once self numout))
        ((equal (allow-lock self) "x")
         `(nth ,numout ,(gen-code (value self) 0)))
        ((equal (allow-lock self) "o") 
         `',(reference self))
        ((equal (allow-lock self) "l")
         (curry-lambda-code self screamerfun))
        (t  `(,screamerfun ,.(decode self))))))
							  
#|
(defmethod special-lambda-value ((self screamer-valuation-boxes) symbol)
"Eval a screamerbox in lambda mode."
 (multiple-value-bind (nesymbs args) (get-args-eval-currry self)
  (eval `#'(lambda ,(reverse nesymbs)
             (case *screamer-valuation* 
               (0 (s::one-value (,symbol ,.args)))
               (1 (s::all-values (,symbol ,.args)))
               (2 (s::print-values (,symbol ,.args))))))))


(defmethod curry-lambda-code ((self screamer-valuation-boxes) symbol)
  "Lisp code generation for a  screamerbox in lambda mode."

   (let ((nesymbs nil)
         (oldlambdacontext *lambda-context*))
     (setf *lambda-context* t)
     
     (unwind-protect 
         (let ((args (mapcan #'(lambda (input)
                                 (let ((a (if (connected? input)
                                              (gen-code input 0)
                                            (let ((newsymbol (gensym)))
                                              (push newsymbol nesymbs)
                                              newsymbol))))
                                   (if (keyword-input-p input) 
                                       (list (value input) a) 
                                     (list a))))
                             (inputs self))))

    `#'(lambda ,(reverse nesymbs)
             (case *screamer-valuation* 
               (0 (s::one-value (,symbol ,.args)))
               (1 (s::all-values (,symbol ,.args)))
               (2 (s::print-values (,symbol ,.args))))))

       (setf *lambda-context* oldlambdacontext)
  )))
	 			
|#  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SCREAMER-SOLUTION-BOXES 
;;; (SCREAMER::SOLUTION) 

#|
(defclass screamer-solution-box (OMBoxCall) () 
   (:documentation "Screamer SOLUTION box"))

(defmethod screamer-solution-box-p ((self screamer-solution-box)) t)
(defmethod screamer-solution-box-p ((self t)) nil)
			 
(defmethod omNG-box-value ((self screamer-solution-box) &optional (numout 0))
   "Eval the output indexed by 'numout' for the box 'self'. In this method we call the generic function reference of 'self'."
   (handler-bind ((error #'(lambda (c)
                             (when *msg-error-label-on*
                               (om-message-dialog (string+ "Error while evaluating the box " (string (name self)) " " 
                                                                               (om-report-condition c)
                                                                               )
                                               :size (om-make-point 300 200))
                               (om-abort)))))
     (cond
      ((equal (allow-lock self) "l") 
       (setf (value self) (list (special-lambda-value self (intern (string (reference self)) :s)))) ;;;test with :s package
       (car (value self)))
      ((or ;(equal (allow-lock self) "l") 
           (equal (allow-lock self) "o")  
           (and (equal (allow-lock self) "x") (value self)) 
           (and (equal (allow-lock self) "&") (ev-once-p self))) (call-next-method))
      (t (let* ((args  (loop for input in (inputs self)
                             when (not (keyword-input-p input)) collect (omNG-box-value input)))
                (qargs (loop for val in args collect (if (or (symbolp val) (omlistp val)) `',val val))) 
                (themethod (compute-applicable-methods (fdefinition (reference self)) args)) rep)
           (if (null themethod)
             (progn (dialog-message (string+ "no method is defined for inputs in box " (name self)))
                    (abort))
             (progn
               (when (and (EditorFrame (car themethod)) (not (compiled? (car themethod))))
                 (modify-genfun (EditorFrame (car themethod))))
               
			   (setf rep (multiple-value-list (eval `(,(intern (string (reference self)) :s) ,.qargs)))))	
            )
           (when (equal (allow-lock self) "&")
             (setf (ev-once-p self) t)
             (setf (value self) rep))
           (when (equal (allow-lock self) "x")
             (setf (value self) rep))
          (progn (setf (value self) rep) ;;; new for om-backtrack in OM 7.2
               (nth numout rep))))))
             )
			 
(defmethod gen-code-call ((self screamer-solution-box) &optional args)
 (let ((screamerfun `,(intern (string (reference self)) :s)))     
  `(,screamerfun ,.(decode self))))

  (defmethod gen-code ((self screamer-solution-box) numout)
     "Generate Lisp code for the box 'self'."
     (let ((screamerfun `,(intern (string (reference self)) :s)))
       (cond
        ((equal (allow-lock self) "&") 
         (gen-code-for-ev-once self numout))
        ((equal (allow-lock self) "x")
         `(nth ,numout ,(gen-code (value self) 0)))
        ((equal (allow-lock self) "o") 
         `',(reference self))
        ((equal (allow-lock self) "l")
         (curry-lambda-code self screamerfun))
        (t  `(,screamerfun ,.(decode self))))))
						  

(defmethod special-lambda-value ((self screamer-solution-box) symbol)
"Eval a screamerbox in lambda mode."
 (multiple-value-bind (nesymbs args) (get-args-eval-currry self)
  (eval `#'(lambda ,(reverse nesymbs)
             (case *screamer-valuation* 
               (0 (s::one-value (,symbol ,.args)))
               (1 (s::all-values (,symbol ,.args)))
               (2 (s::print-values (,symbol ,.args))))))))


(defmethod curry-lambda-code ((self screamer-solution-box) symbol)
  "Lisp code generation for a  screamerbox in lambda mode."

   (let ((nesymbs nil)
         (oldlambdacontext *lambda-context*))
     (setf *lambda-context* t)
     
     (unwind-protect 
         (let ((args (mapcan #'(lambda (input)
                                 (let ((a (if (connected? input)
                                              (gen-code input 0)
                                            (let ((newsymbol (gensym)))
                                              (push newsymbol nesymbs)
                                              newsymbol))))
                                   (if (keyword-input-p input) 
                                       (list (value input) a) 
                                     (list a))))
                             (inputs self))))

    `#'(lambda ,(reverse nesymbs)
             (case *screamer-valuation* 
               (0 (s::one-value (,symbol ,.args)))
               (1 (s::all-values (,symbol ,.args)))
               (2 (s::print-values (,symbol ,.args))))))

       (setf *lambda-context* oldlambdacontext)
  )))
	 			
|#  

(defclass screamer-ordering-boxes (OMBoxCall) () 
   (:documentation "Screamer ORDERING box"))

(defmethod screamer-ordering-boxes-p ((self screamer-ordering-boxes)) t)
(defmethod screamer-ordering-boxes-p ((self t)) nil)
			 
(defmethod omNG-box-value ((self screamer-ordering-boxes) &optional (numout 0))
   "Eval the output indexed by 'numout' for the box 'self'. In this method we call the generic function reference of 'self'."
   (handler-bind ((error #'(lambda (c)
                             (when *msg-error-label-on*
                               (om-message-dialog (string+ "Error while evaluating the box " (string (name self)) " " 
                                                                               (om-report-condition c)
                                                                               )
                                               :size (om-make-point 300 200))
                               (om-abort)))))
     (cond
      ((equal (allow-lock self) "l") 
       (setf (value self) (list (special-lambda-value self (intern (string (reference self)) :s)))) ;;;test with :s package
       (car (value self)))
       ((equal (allow-lock self) "#")
        (setf (value self) (list (eval `(function ,(intern (string (reference self)) :s))))))
      ((or ;(equal (allow-lock self) "l") 
           (equal (allow-lock self) "o")  
           (and (equal (allow-lock self) "x") (value self)) 
           (and (equal (allow-lock self) "&") (ev-once-p self))) (call-next-method))
      (t (let* ((args  (loop for input in (inputs self)
                             when (not (keyword-input-p input)) collect (omNG-box-value input)))
                (qargs (loop for val in args collect (if (or (symbolp val) (omlistp val)) `',val val))) 
                (themethod (compute-applicable-methods (fdefinition (reference self)) args)) rep)
           (if (null themethod)
             (progn (dialog-message (string+ "no method is defined for inputs in box " (name self)))
                    (abort))
             (progn
               (when (and (EditorFrame (car themethod)) (not (compiled? (car themethod))))
                 (modify-genfun (EditorFrame (car themethod))))
               
			   (setf rep (multiple-value-list (eval `(,(intern (string (reference self)) :s) ,.qargs)))))	
            )
           (when (equal (allow-lock self) "&")
             (setf (ev-once-p self) t)
             (setf (value self) rep))
           (when (equal (allow-lock self) "x")
             (setf (value self) rep))			 
          (progn (setf (value self) rep) ;;; new for om-backtrack in OM 7.2
               (nth numout rep))))))
             )
			 
(defmethod gen-code-call ((self screamer-ordering-boxes) &optional args)
 (let ((screamerfun `,(intern (string (reference self)) :s)))     
  `(,screamerfun ,.(decode self))))

  (defmethod gen-code ((self screamer-ordering-boxes) numout)
     "Generate Lisp code for the box 'self'."
     (let ((screamerfun `,(intern (string (reference self)) :s)))
       (cond
        ((equal (allow-lock self) "&") 
         (gen-code-for-ev-once self numout))
        ((equal (allow-lock self) "x")
         `(nth ,numout ,(gen-code (value self) 0)))
        ((equal (allow-lock self) "o") 
         `',(reference self))
        ((equal (allow-lock self) "l")
         (curry-lambda-code self screamerfun))
         ((equal (allow-lock self) "#")
           (setf (value self) `(function ,screamerfun)))			 		 
        (t  `(,screamerfun ,.(decode self))))))

(defclass screamer-force-function-boxes (OMBoxCall) () 
   (:documentation "Screamer FORCE FUNCTION box"))

(defmethod screamer-force-function-boxes-p ((self screamer-force-function-boxes)) t)
(defmethod screamer-force-function-boxes-p ((self t)) nil)
	 
(defmethod omNG-box-value ((self screamer-force-function-boxes) &optional (numout 0))
   "Eval the output indexed by 'numout' for the box 'self'. In this method we call the generic function reference of 'self'."
   (handler-bind ((error #'(lambda (c)
                             (when *msg-error-label-on*
                               (om-message-dialog (string+ "Error while evaluating the box " (string (name self)) " " 
                                                                               (om-report-condition c)
                                                                               )
                                               :size (om-make-point 300 200))
                               (om-abort)))))
     (cond
      ((equal (allow-lock self) "l") 
       (setf (value self) (list (special-lambda-value self (intern (string (reference self)) :s)))) ;;;test with :s package
       (car (value self)))
       ((equal (allow-lock self) "#")
        (setf (value self) (list (eval `(function ,(intern (string (reference self)) :s))))))
      ((or ;(equal (allow-lock self) "l")  
           (equal (allow-lock self) "o")  
           (and (equal (allow-lock self) "x") (value self)) 
           (and (equal (allow-lock self) "&") (ev-once-p self))) (call-next-method))
      (t (let* ((args  (loop for input in (inputs self)
                             when (not (keyword-input-p input)) collect (omNG-box-value input)))
                (qargs (loop for val in args collect (if (or (symbolp val) (omlistp val)) `',val val))) 
                (themethod (compute-applicable-methods (fdefinition (reference self)) args))
				 rep)
				 
           (if (null themethod)
             (progn (dialog-message (string+ "no method is defined for inputs in box " (name self)))
                    (abort))
             (progn
               (when (and (EditorFrame (car themethod)) (not (compiled? (car themethod))))
                 (modify-genfun (EditorFrame (car themethod))))
       
			   (setf rep (multiple-value-list (eval `(,(intern (string (reference self)) :s) ,.qargs)))))
						  
            )
           (when (equal (allow-lock self) "&")
             (setf (ev-once-p self) t)
             (setf (value self) rep))
           (when (equal (allow-lock self) "x")
             (setf (value self) rep))	 
          (progn (setf (value self) rep) ;;; new for om-backtrack in OM 7.2
               (nth numout rep))))))
             )
	 
(defmethod gen-code-call ((self screamer-force-function-boxes) &optional args)
 (let ((screamerfun `,(intern (string (reference self)) :s)))  
        `(,screamerfun ,.(decode self))   
	 ))

  (defmethod gen-code ((self screamer-force-function-boxes) numout)
     "Generate Lisp code for the box 'self'."
     (let ((screamerfun `,(intern (string (reference self)) :s)))
       (cond
        ((equal (allow-lock self) "&") 
         (gen-code-for-ev-once self numout))
        ((equal (allow-lock self) "x")
         `(nth ,numout ,(gen-code (value self) 0)))
        ((equal (allow-lock self) "o") 
         `',(reference self))
        ((equal (allow-lock self) "l")
         (curry-lambda-code self screamerfun))
         ((equal (allow-lock self) "#")
           (setf (value self) `(function ,screamerfun)))		 
        (t `(,screamerfun ,.(decode self))  
	    ))))


