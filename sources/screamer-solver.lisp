(in-package :om)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SCREAMER-SOLVER

(defmethod! screamer-solver ((variables t) 
                                                  (p-variables t) 
                                                  (constraints t) 
                                                  (p-constraints t) 
                                                  (screamer-valuation t) 
                                                  (force-function t)
                                                  &optional (output nil) count-failures? objective-form form2) 
													  
    :initvals '(nil nil nil nil "one-value" "static-ordering linear-force" nil nil nil nil)

    :indoc '("variable or list" "propagation-variables<lambda-patch>" "constraint<lambda-patch> or list" 
	     "propagation-constraints<lambda-patch>" "one-value, all-values, listener, n-values, ith-value or best-value"
	     "ordering-force-functions" "symbol or list" "symbol t or nil" "<lambda-patch>"  "<lambda-patch>")

    :doc "Screamer Constraint Solver
  <VARIABLES> variable or list of variables.
  <P-VARIABLES> lambda patch or list of lambda patches. Generates a new-list of variables.
  <CONSTRAINTS> lambda patch or list of lambda patches => constraint to variables.
  <P-CONSTRAINTS> lambda patch or list of lambda patches => constraints to propagation variables.
  <SCREAMER-VALUATION> menuin with four options (one-value, all-values, listener, n-values or ith-value).
  <FORCE-FUNCTION> a string or should be connected to force-function.  
  <OUTPUT> symbol :all or nil or list with symbols or positions. Ex: (nil :all) or ((0 2) (1 3)).
  <COUNT-FAILURES?> symbol t or nil.
  <OBJECTIVE-FORM>and<FORM2>: lambda patches with two inputs (vars / p-vars) -> best-value forms (experimental)." 

    :menuins '((4 (("one-value" "one-value") 
                   ("all-values" "all-values") 
                   ("listener" "listener")
                   ("n-values" '("n-values" 10)) ("ith-value" '("ith-value" 10)) ("best-value" "best-value") ))
	       (7 (("nil" nil) ("t" t))) )                        
    :icon 487 

   (let ((pref-valuation *screamer-valuation*)
          (propagation-variables
           (if (functionp p-variables) 
               (apply p-variables (list variables))
               (mapcar #'(lambda (cs) (apply cs (list variables))) p-variables)))
          (compiled-constraints 
           (if (functionp constraints) 
               (fdefinition (compile-screamer-constraint constraints))
               (mapcar #'(lambda (x) (fdefinition (compile-screamer-constraint x))) constraints)))
          (compiled-p-constraints 
           (if p-constraints 
              (if (functionp p-constraints) 
               (fdefinition (compile-screamer-constraint p-constraints))
               (mapcar #'(lambda (x) (fdefinition (compile-screamer-constraint x))) p-constraints)))))

    (if compiled-constraints
     (if (functionp compiled-constraints) 
         (apply compiled-constraints (list variables))
         (mapcar #'(lambda (cs) (apply cs (list variables))) compiled-constraints))  
        (declare (ignore compiled-constraints)))

     (if compiled-p-constraints
         (if (functionp compiled-p-constraints) 
          (apply compiled-p-constraints (list propagation-variables))
          (mapcar #'(lambda (cs) (apply cs (list propagation-variables))) compiled-p-constraints))
        (declare (ignore compiled-p-constraints)))
	
 (setf s::*count?* count-failures?)

 (setf scs-time (list (get-internal-run-time) (get-internal-real-time)))
 (print "Timing evaluation of screamer-solver...")
	
    (let ((solution 
      (cond ((equal screamer-valuation "one-value") ;ONE-VALUE
	      (s::count-scs-failures
                  (s::one-value 
                   (select-solution variables propagation-variables output
                    (s::solution (list variables propagation-variables)
                      (cond ((equal force-function "static-ordering linear-force") (s::static-ordering #'s::linear-force))
                                ((equal force-function "static-ordering divide-and-conquer-force") (s::static-ordering #'s::divide-and-conquer-force))
                                (t (s::reorder 
                                    (cond ((null (second force-function)) #'s::domain-size)
                                              ((functionp (second force-function)) (second force-function))
                                              ((equal (second force-function) "domain-size") #'s::domain-size)   
                                              ((equal (second force-function) "range-size") #'s::range-size)
                                             (t #'s::domain-size))
                                    (cond ((null (third force-function)) #'(lambda (x) (declare (ignore x)) nil))
                                              ((functionp (third force-function)) (third force-function))
                                              ((equal (third force-function) "(< x 1e-6)") #'(lambda (x) (< x 1e-6)))
                                              (t #'(lambda (x) (declare (ignore x)) nil)))
                                   (if (equal (fourth force-function) ">") #'> #'<) 
                                  (cond ((null (fifth force-function)) #'s::linear-force)
                                        ((functionp (fifth force-function)) (fifth force-function))
                                        ((equal (fifth force-function) "linear-force") #'s::linear-force)   
                                        ((equal (fifth force-function) "divide-and-conquer-force") #'s::divide-and-conquer-force)
                                        (t #'s::linear-force))))))))))

               ((equal screamer-valuation "all-values") ;ALL-VALUES
		 (s::count-scs-failures
                (s::all-values 
                   (select-solution variables propagation-variables output
                    (s::solution (list variables propagation-variables)
                      (cond ((equal force-function "static-ordering linear-force") (s::static-ordering #'s::linear-force))
                                ((equal force-function "static-ordering divide-and-conquer-force") (s::static-ordering #'s::divide-and-conquer-force))
                                (t (s::reorder ;;;REORDER ===> ADAPTED FROM T2L (K.SPROTTE)
                                    (cond ((null (second force-function)) #'s::domain-size)
                                              ((functionp (second force-function)) (second force-function))
                                              ((equal (second force-function) "domain-size") #'s::domain-size)   
                                              ((equal (second force-function) "range-size") #'s::range-size)
                                             (t #'s::domain-size))
                                    (cond ((null (third force-function)) #'(lambda (x) (declare (ignore x)) nil))
                                              ((functionp (third force-function)) (third force-function))
                                              ((equal (third force-function) "(< x 1e-6)") #'(lambda (x) (< x 1e-6)))
                                              (t #'(lambda (x) (declare (ignore x)) nil)))
                                   (if (equal (fourth force-function) ">") #'> #'<) 
                                  (cond ((null (fifth force-function)) #'s::linear-force)
                                        ((functionp (fifth force-function)) (fifth force-function))
                                        ((equal (fifth force-function) "linear-force") #'s::linear-force)   
                                        ((equal (fifth force-function) "divide-and-conquer-force") #'s::divide-and-conquer-force)
                                        (t #'s::linear-force))))))))))

                 ((equal screamer-valuation "listener") ;PRINT-VALUES
                  (setf *screamer-valuation* 2)
		   (s::count-scs-failures
                     (s::print-values
                       ;;; ===> MAKE-INSTANCE FOR CLASSES  <===
                   (select-solution variables propagation-variables output
                    (s::solution (list variables propagation-variables)
                      (cond ((equal force-function "static-ordering linear-force") (s::static-ordering #'s::linear-force))
                                ((equal force-function "static-ordering divide-and-conquer-force") (s::static-ordering #'s::divide-and-conquer-force))
                                (t (s::reorder 
                                    (cond ((null (second force-function)) #'s::domain-size)
                                              ((functionp (second force-function)) (second force-function))
                                              ((equal (second force-function) "domain-size") #'s::domain-size)   
                                              ((equal (second force-function) "range-size") #'s::range-size)
                                             (t #'s::domain-size))
                                    (cond ((null (third force-function)) #'(lambda (x) (declare (ignore x)) nil))
                                              ((functionp (third force-function)) (third force-function))
                                              ((equal (third force-function) "(< x 1e-6)") #'(lambda (x) (< x 1e-6)))
                                              (t #'(lambda (x) (declare (ignore x)) nil)))
                                   (if (equal (fourth force-function) ">") #'> #'<) 
                                  (cond ((null (fifth force-function)) #'s::linear-force)
                                        ((functionp (fifth force-function)) (fifth force-function))
                                        ((equal (fifth force-function) "linear-force") #'s::linear-force)   
                                        ((equal (fifth force-function) "divide-and-conquer-force") #'s::divide-and-conquer-force)
                                        (t #'s::linear-force))))))))))
	      
                 ((equal screamer-valuation "best-value") ;BEST-VALUE (EXPERIMENTAL)
                   (s::count-scs-failures      
                   (s::best-value                
                     (s::solution (list variables propagation-variables)
                      (cond ((equal force-function "static-ordering linear-force") (s::static-ordering #'s::linear-force))
                                ((equal force-function "static-ordering divide-and-conquer-force") (s::static-ordering #'s::divide-and-conquer-force))
                                (t (s::reorder 
                                    (cond ((null (second force-function)) #'s::domain-size)
                                              ((functionp (second force-function)) (second force-function))
                                              ((equal (second force-function) "domain-size") #'s::domain-size)   
                                              ((equal (second force-function) "range-size") #'s::range-size)
                                             (t #'s::domain-size))
                                    (cond ((null (third force-function)) #'(lambda (x) (declare (ignore x)) nil))
                                              ((functionp (third force-function)) (third force-function))
                                              ((equal (third force-function) "(< x 1e-6)") #'(lambda (x) (< x 1e-6)))
                                              (t #'(lambda (x) (declare (ignore x)) nil)))
                                   (if (equal (fourth force-function) ">") #'> #'<) 
                                  (cond ((null (fifth force-function)) #'s::linear-force)
                                        ((functionp (fifth force-function)) (fifth force-function))
                                        ((equal (fifth force-function) "linear-force") #'s::linear-force)   
                                        ((equal (fifth force-function) "divide-and-conquer-force") #'s::divide-and-conquer-force)
                                        (t #'s::linear-force))))))
                        (cond ((not (null form2))
                                   (apply objective-form (list variables propagation-variables)) 
                                   (apply form2 (list variables propagation-variables)))
                           (t (apply objective-form (list variables propagation-variables)))))))	      

                  (t (if (equal (first screamer-valuation) "n-values")
		    (s::count-scs-failures	 
		   (om?::n-values (second screamer-valuation) ;N-VALUES
                   (select-solution variables propagation-variables output
                    (s::solution (list variables propagation-variables)
                      (cond ((equal force-function "static-ordering linear-force") (s::static-ordering #'s::linear-force))
                            ((equal force-function "static-ordering divide-and-conquer-force") (s::static-ordering #'s::divide-and-conquer-force))
                            (t (s::reorder 
                                (cond ((null (second force-function)) #'s::domain-size)
                                      ((functionp (second force-function)) (second force-function))
                                      ((equal (second force-function) "domain-size") #'s::domain-size)   
                                      ((equal (second force-function) "range-size") #'s::range-size)
                                      (t #'s::domain-size))
                                (cond ((null (third force-function)) #'(lambda (x) (declare (ignore x)) nil))
                                      ((functionp (third force-function)) (third force-function))
                                      ((equal (third force-function) "(< x 1e-6)") #'(lambda (x) (< x 1e-6)))
                                      (t #'(lambda (x) (declare (ignore x)) nil)))
                                (if (equal (fourth force-function) ">") #'> #'<) 
                                (cond ((null (fifth force-function)) #'s::linear-force)
                                      ((functionp (fifth force-function)) (fifth force-function))
                                     ((equal (fifth force-function) "linear-force") #'s::linear-force)   
                                     ((equal (fifth force-function) "divide-and-conquer-force") #'s::divide-and-conquer-force)
                                     (t #'s::linear-force)))))))))
	      (s::count-scs-failures								
	       (s::ith-value (second screamer-valuation) ;ITH-VALUE
                (select-solution variables propagation-variables output
                 (s::solution (list variables propagation-variables)
                   (cond ((equal force-function "static-ordering linear-force") (s::static-ordering #'s::linear-force))
                             ((equal force-function "static-ordering divide-and-conquer-force") (s::static-ordering #'s::divide-and-conquer-force))
                             (t (s::reorder 
                                 (cond ((null (second force-function)) #'s::domain-size)
                                           ((functionp (second force-function)) (second force-function))
                                           ((equal (second force-function) "domain-size") #'s::domain-size)   
                                           ((equal (second force-function) "range-size") #'s::range-size)
                                          (t #'s::domain-size))
                                 (cond ((null (third force-function)) #'(lambda (x) (declare (ignore x)) nil))
                                           ((functionp (third force-function)) (third force-function))
                                           ((equal (third force-function) "(< x 1e-6)") #'(lambda (x) (< x 1e-6)))
                                           (t #'(lambda (x) (declare (ignore x)) nil)))
                                (if (equal (fourth force-function) ">") #'> #'<) 
                               (cond ((null (fifth force-function)) #'s::linear-force)
                                     ((functionp (fifth force-function)) (fifth force-function))
                                     ((equal (fifth force-function) "linear-force") #'s::linear-force)   
                                     ((equal (fifth force-function) "divide-and-conquer-force") #'s::divide-and-conquer-force)
                                     (t #'s::linear-force)))))))))))										
                 )))
	 
  (print-scs-time scs-time)
	 
  (progn (setf *screamer-valuation* pref-valuation) 
            (cond ((atom solution) solution)
                  ((equal screamer-valuation "best-value") 
                   (x-append (select-solution variables propagation-variables output (first solution))
                             (second solution))) 
                   ;((list-of-listp (first solution)) (mat-trans solution)) 
                   (t solution)))
 )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VARIABLES

(defmethod! list-of-lists-ofv ((n-vars list) (variables string) &optional args) 
  :initvals '((3 4 3) "an-integer-betweenv" (0 10))
  :indoc '("list" "screamer-variable" "list")
  :doc "List of lists of screamer variables" 
  :menuins '((1 (("an-integer-betweenv" "an-integer-betweenv") 
                         ("a-member-ofv" "a-member-ofv")
                         ("a-random-member-ofv" "a-random-member-ofv")
                         ("an-integerv" "an-integerv") 
                         ("an-integer-abovev" "an-integer-abovev")
                         ("an-integer-belowv" "an-integer-belowv") 
                         ("a-realv" "a-realv") 
                         ("a-real-abovev" "a-real-abovev")									
                         ("a-real-below" "a-real-below")  
                         ("a-real-betweenv" "a-real-betweenv")
                         ("a-numberv" "a-numberv")
                         ("a-booleanv" "a-booleanv")
                  ))
                   )
  :icon 487
(mapcar #'(lambda (x)
(make-lists-ofv x variables args)) n-vars))

(defun make-lists-ofv (n var &optional args)
 (cond 
   ((equal var "an-integer-betweenv") (om?::list-of-integers-betweenv n (first args) (second args)))
   ((equal var "a-member-ofv") (om?::list-of-members-ofv n args))
   ((equal var "a-random-member-ofv") (om?::list-of-random-members-ofv n args))
   ((equal var "a-booleanv") (om?::list-of-booleansv n))
   ((equal var "an-integerv") (om?::list-of-integersv n))
   ((equal var "an-integer-abovev") (om?::list-of-integers-abovev n (first args)))
   ((equal var "an-integer-belowv") (om?::list-of-integers-belowv n (first args)))
   ((equal var "a-realv") (om?::list-of-realsv n))
   ((equal var "a-real-abovev")  (om?::list-of-reals-abovev n (first args)))					
   ((equal var "a-real-below") (om?::list-of-reals-belowv n (first args)))
   ((equal var "a-real-betweenv") om?::list-of-reals-betweenv (first args) (second args))
   ((equal var "a-numberv") (om?::list-of-numbersv n))))

(defmethod! list-of-chords-inv ((n-chords list) (domain list) &optional random?) 
  :initvals '((3 4 3) (6000 6200 6400 6500 6700 6900 7100 7200) nil)
  :indoc '("list" "list" "t or nil")
  :menuins '((2 (("nil" 'nil)  ("t" 't))))
  :doc "Generates a list of screamer variables interpeted as CHORDS, that is a list with members of domain with no duplications and in ascending order." 
  :icon 487
(om?::list-of-chords-inv n-chords domain random?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COMPILE-CONSTRAINT
; => ADAPTED FROM OMCS AND CLUSTER-ENGINE

(defun make-anon-screamerfun (fn) ;;; OMCS 
  (eval `(screamer::defun ,(gensym) ,.(rest fn))))

(defun compile-screamer-constraint (fun) ;;;CE
 (let* ((expr (function-lambda-expression fun)))
    (if (compiled-function-p expr)
        expr
    (compile (make-anon-screamerfun expr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OUTPUT OPTIONS

(defun list-of-listp (thing) (and (listp thing) (every #'listp thing)))
(deftype list-of-lists () '(satisfies list-of-listp))

(defun select-solution (vars p-vars out solution)
 (cond
  ((null out) (first solution))
  ((and (symbolp out) 
           (equal out :all))
                solution)
  ((listp out)
    (let ((variables 
           (cond ((equal (first out) :all) (first solution))
                      ((listp (first out)) (posn-match (first solution) (first out)))
                      (t nil)))
           (p-variables 
           (cond ((equal (second out) :all) (second solution))
                      ((listp (second out)) (posn-match (second solution) (second out)))
                      (t (nil)))))
     (if variables 
         (if p-variables (list variables p-variables) variables)
     p-variables)))
  (t (progn (om-message-dialog "ERROR: the <OUTPUT> should be a symbol :all or nil, or a list with two symbols [ex. (:all nil)] or a list containing two lists of positions [ex. ((0 2) (1 3))]") (om-abort)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;TIME 

(defun seconds->time (s)
 (let* ((ms (* s 1000))
         (ms (mod ms 3600000))
         (minutes (floor ms 60000))
         (ms (mod ms 60000))
         (seconds (floor ms 1000))
         (ms (round (mod ms 1000))))
    (format nil "~2,'0d:~2,'0d.~3,'0d"
            minutes seconds ms)))

(defun print-scs-time (scs-time)
 (print (format nil 
" 
------------------------------------- 
   User time   =    ~A 
   Elapsed time   =    ~A 
------------------------------------- "
  (seconds->time (float (/ (- (get-internal-run-time) (first scs-time)) internal-time-units-per-second)))
  (seconds->time (float (/ (- (get-internal-real-time) (second scs-time)) internal-time-units-per-second))))))

;Allocation   =   ~A
;System time   =   ~A 
;Page faults   ~A
;Calls to %EVAL   ~A    
;GC time   =   ~A 
;" 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FORCE-FUNCTION

(defmethod! force-function ((force-function string) 
                                             &optional (cost-function "domain-size") 
                                                             (terminate? "(declare (ignore x))")
                                                             (order ">" )
                                                             (reorder-force-function"linear-force"))

  :initvals '("static-ordering linear-force" "domain-size" "(declare (ignore x))" ">" "linear-force")

  :indoc '("ordering-force-functions" "domain-size or range-size" "terminate-function" "> or <" "linear-force or divide-and-conquer-force")

  :doc "Screamer Ordering and Force-Functions" 

  :menuins '((0 (("static-ordering linear-force" "static-ordering linear-force") 
                        ("static-ordering divide-and-conquer-force" "static-ordering divide-and-conquer-force") 
                        ("reorder" "reorder" )))

                   (1 (("domain-size" "domain-size") 
                        ("range-size" "range-size")))

                   (2 (("(declare (ignore x))"  "(declare (ignore x))" ) 
                        ("(< x 1e-6)"  "(< x 1e-6)" ) ))

                   (3 ((">" ">") 
                        ("<" "<")))

                   (4 (("linear-force" "linear-force") 
                        ("divide-and-conquer-force" "divide-and-conquer-force")))
                   )
  :icon 487 
(if (or (equal force-function "static-ordering linear-force")
          (equal force-function "static-ordering divide-and-conquer-force"))
      force-function
(list force-function cost-function terminate? order reorder-force-function)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ===> IN-PROGRESS <===
;;; SCREAMER-SCORE - ONLY PITCH - ONE-VOICE

(defun update-pitches (voice-object solution)
(make-instance 'voice
    :tree (tree voice-object)
    :tempo (tempo voice-object)
    :legato (legato voice-object)
    :ties (ties voice-object)
    :chords solution))

(defmethod! screamer-score ((voice-object voice) 
                                                (domain list)
                                                (constraints t)
                                                (force-function t)) 

  :initvals '(nil (6000 6200 6400) nil "static-ordering linear-force") 

  :indoc '("voice" "list"  "constraint<lambda-patch> or list" "ordering-force-functions")

  :doc "Screamer Score Constraint Solver" 

  :icon 487 

(let* ((pref-valuation *screamer-valuation*)
         (n-notes (length (remove-if #'(lambda (x) (< x 0)) (tree2ratio (tree voice-object)))))
         ;attacks - harmonic-slice - etc..
         (pitch-domain (om?::list-of-members-ofv n-notes (reverse domain))));==> RANDOM? om?::list-of-random-members-ofv

   (if (listp constraints) 
       (mapcar #'(lambda (cs) (apply cs (list pitch-domain))) constraints)
       (apply constraints (list pitch-domain)))

(setf *screamer-valuation* 2)

(let ((solution
        (s::print-values
         (update-pitches voice-object
         (s::solution pitch-domain
                    (cond ((equal force-function "static-ordering linear-force") (s::static-ordering #'s::linear-force))
                              ((equal force-function "static-ordering divide-and-conquer-force") (s::static-ordering #'s::divide-and-conquer-force))
                              (t (s::reorder 
                                  (cond ((null (second force-function)) #'s::domain-size)
                                            ((string= (format nil "~A" (second force-function)) "domain-size") #'s::domain-size)   
                                            ((string= (format nil "~A" (second force-function)) "range-size") #'s::range-size)
                                           (t #'s::domain-size))
                                  (cond ((null (third force-function)) #'(lambda (x) (declare (ignore x)) nil))
                                            ((functionp (third force-function)) (third force-function))
                                            ((string= (format nil "~A" (third force-function)) "(< x 1e-6)") #'(lambda (x) (< x 1e-6)))
                                            (t #'(lambda (x) (declare (ignore x)) nil)))
                                  (if (equal (fourth force-function) ">") #'> #'<) 
                                (cond ((null (fifth force-function)) #'s::linear-force) 
                                      ((string= (format nil "~A" (fifth force-function)) "linear-force") #'s::linear-force)   
                                      ((string= (format nil "~A" (fifth force-function)) "divide-and-conquer-force") #'s::divide-and-conquer-force)
                                      (t #'s::linear-force))
                               )))
            )))))

     (progn (setf *screamer-valuation* pref-valuation) 
                 solution)
      
  )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COUNT-SCS-FAILURES

(in-package :s)

(defvar *count?* nil)

(defmacro-compile-time count-scs-failures (&body forms)
 (let ((values (gensym "VALUES-")))
   (if *count?*
    `(let ((failure-count 0))
       (when-failing ((incf failure-count) 
        (when (integerp (/ failure-count 1000000)) (print (format nil "Number of failures: ~:d." failure-count)))) 
         (let ((,values (multiple-value-list (progn ,@forms))))
            (print (format nil "
------------------------------------- 
Failures    =    ~:d
------------------------------------- " failure-count) )        
           (values-list ,values))))

     `(let ((,values (multiple-value-list (progn ,@forms))))           
           (values-list ,values)))))


