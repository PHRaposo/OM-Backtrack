;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; REVISED VERSION
;;; Copyright 2024 PAULO HENRIQUE RAPOSO AND KARIM HADDAD

(in-package :om)

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
               (setf rep 
                     (case *screamer-valuation*
                       (0 (multiple-value-list (eval `(s::one-value (,(intern (string (reference self)) :s) ,.qargs)))))
                       (1 (multiple-value-list (eval `(s::all-values (,(intern (string (reference self)) :s) ,.qargs)))))  
                       (2 (multiple-value-list (eval `(s::print-values (,(intern (string (reference self)) :s) ,.qargs))))))))
             )
           (when (equal (allow-lock self) "&")
             (setf (ev-once-p self) t)
             (setf (value self) rep))
           (when (equal (allow-lock self) "x")
             (setf (value self) rep))
          (progn (setf (value self) rep) ;;; new for om-backtrack in OM 7.2
               (nth numout rep))))))
             )

(defmacro evalsc (fun arg)
  `(let ((varg (loop for item in ,arg collect (quote item))))
     (print (,fun varg))))

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
