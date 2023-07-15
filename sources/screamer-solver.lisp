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
  <OUTPUT> symbol :all or nil or list with symbols and positions. Ex.: (nil :all) or ((0 2) (1 3)). 
  If a third argument is supplied <om-function-name>, applies a function in the list of results. Ex.: (:all nil mat-trans).
 <COUNT-FAILURES?> symbol t or nil.
 <OBJECTIVE-FORM>and<FORM2>: lambda patches with two inputs (vars / p-vars) -> best-value forms (experimental)." 

    :menuins '((4 (("one-value" "one-value") ("all-values" "all-values") 
                           ("listener" "listener") ("n-values" '("n-values" 10)) ("ith-value" '("ith-value" 10)) ("best-value" "best-value") )) 
                     (7 (("nil" nil) ("t" t))) )                        
    :icon 487 

 (screamer-solution variables 
                               p-variables 
                               constraints 
                               p-constraints 
                               screamer-valuation 
                               force-function 
                               output 
                               count-failures? 
                               objective-form 
                               form2)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SCREAMER-SOLUTION

(defun screamer-solution (vars p-vars cs p-cs valuation ordering-force out count-fail? obj-form form2)
   (let* ((pref-valuation *screamer-valuation*)   
          (compiled-p-vars ;==> PROPAGATION VARIABLES
           (if (functionp p-vars) 
               (fdefinition (compile-screamer-constraint p-vars))
               (mapcar #'(lambda (x) (fdefinition (compile-screamer-constraint x))) p-vars)))			   
          (compiled-constraints ;==> COMPILED CONSTRAINTS TO VARIABLES
           (if (functionp cs) 
               (fdefinition (compile-screamer-constraint cs))
               (mapcar #'(lambda (x) (fdefinition (compile-screamer-constraint x))) cs)))			   
          (compiled-p-constraints ;==> COMPILED CONSTRAINTS PROPAGATION VARIABLES
           (if p-cs 
              (if (functionp p-cs) 
               (fdefinition (compile-screamer-constraint p-cs))
               (mapcar #'(lambda (x) (fdefinition (compile-screamer-constraint x))) p-cs))))			   
         (compiled-forms ;==> BEST-VALUE FORMS
		 (if obj-form
		     (if form2 
		         (mapcar #'(lambda (x) (fdefinition (compile-screamer-constraint x))) (list obj-form form2))
		   	 (fdefinition (compile-screamer-constraint obj-form)))
		      nil))
		 (vars-name (intern (string (gensym)) :s)) ;==> NAME FOR VARIABLES (SYMBOL) 		 
		 (p-vars-name (intern (string (gensym)) :s)) ;==> NAME FOR PROPAGATION VARIABLES (SYMBOL)
		 (constraints ;==> CONSTRAINTS CODE
		    (if compiled-constraints
		     (if (functionp compiled-constraints) 
		         `(apply ',compiled-constraints (list ,vars-name))
		         `(mapcar #'(lambda (cs) (apply cs (list ,vars-name))) ',compiled-constraints))  
		        nil ))
	    (p-constraints ;==> P-CONSTRAINTS CODE
	      (if compiled-p-constraints
	         (if (functionp compiled-p-constraints) 
	          `(apply ',compiled-p-constraints (list ,p-vars-name))
	          `(mapcar #'(lambda (cs) (apply cs (list ,p-vars-name))) ',compiled-p-constraints))
	        nil ))			
	    (val (cond ((equal valuation "one-value") 0) 
	                 ((equal valuation "all-values") 1)
	                 ((equal valuation "listener") 2)
	                 ((equal valuation "best-value") 5)
	                 ((listp valuation)
	                  (cond  ((equal (first valuation) "n-values") 3)
	                         ((equal (first valuation) "ith-value") 4)
	                         (t (om-message-dialog "UNKNOWN VALUATION OPTION!") (om-abort))))		  		
			 (t (progn (om-message-dialog "UNKNOWN VALUATION OPTION!") (om-abort)))))
	   (ordering ;==> ORDERING
		(let ((ord (first (string-to-list ordering-force))))
	     (cond ((equal "static-ordering" ord) 0)
		       ((equal "reorder" (first ord)) 1)
		       (t 0))))
	  (force-function ;==> FORCE-FUNCTION FOR STATIC-ORDERING
	   (if (= 1 ordering) 
		    nil
		   (let ((force (second (string-to-list ordering-force))))
		    (cond ((equal "linear-force" force) 0)
		          ((equal "divide-and-conquer-force" force) 1)
		          (t 0)))))				  
          (cost-function (if (= ordering 1) ;==>  REORDER COST-FUNCTION 
                             (cond ((equal "domain-size" (second ordering-force)) 0)
 	                           ((equal "range-size" (second ordering-force)) 1)
                                   (t 0)) nil))
          (terminate? (if (= ordering 1) ;==> REORDER TERMINATE? 
		          (cond ((equal "(declare (ignore x))" (third ordering-force)) 0)
                                ((equal "(< x 1e-6)" (third ordering-force)) 1)
			        ((functionp (third ordering-force)) 2) 
                                (t 0)) nil)) 
          (order (if (= ordering 1) ;==> REORDER ORDER 
		     (cond ((equal ">" (fourth ordering-force)) 0)
			   ((equal "<" (fourth ordering-force)) 1)
	                   (t 0)) nil))					   
          (reorder-force (if (= ordering 1) ;==> REORDER FORCE-FUNCTION
	                     (cond ((equal "linear-force" (fifth ordering-force)) 0)
				   ((equal "divide-and-conquer-force" (fifth ordering-force)) 1)
	                           (t 0)) nil))
          (terminate?-fn (if (and (= ordering 1) (functionp (third ordering-force))) ;==> REORDER COMPILED TERMINATE?-FUNCTION
			          (fdefinition (compile-screamer-constraint (third ordering-force)))
			   	   nil))
          (solution-code ;==> CODE FOR SCREAMER VALUATION AND SOLUTION                     
	   (case ordering
	    (0 ;==>  STATIC-ORDERING
	     (case val
	      (0 `(s::one-value
	      (select-solution ',out
	       (s::solution (list ,vars-name ,p-vars-name)
	      (s::static-ordering ,(case force-function 
	                                     (0 `#'s::linear-force)
	                                     (1 `#'s::divide-and-conquer-force)))))))
	      (1 `(s::all-values
	             (select-solution ',out
	              (s::solution (list ,vars-name ,p-vars-name)
	               (s::static-ordering ,(case force-function 
	                                             (0 `#'s::linear-force)
	                                             (1 `#'s::divide-and-conquer-force)))))))
	      (2  `(s::print-values
	              (select-solution ',out
	               (s::solution (list ,vars-name ,p-vars-name)
	                (s::static-ordering ,(case force-function 
	                                              (0 `#'s::linear-force)
	                                              (1 `#'s::divide-and-conquer-force)))))))
	      (3  `(om?::n-values ,(second valuation)
	              (select-solution ',out
	               (s::solution (list ,vars-name ,p-vars-name)
	                (s::static-ordering ,(case force-function 
	                                              (0 `#'s::linear-force)
	                                              (1 `#'s::divide-and-conquer-force)))))))																  
	      (4  `(s::ith-value ,(second valuation)
	              (select-solution ',out
	               (s::solution (list ,vars-name ,p-vars-name)
	                (s::static-ordering ,(case force-function 
	                                              (0 `#'s::linear-force)
	                                              (1 `#'s::divide-and-conquer-force)))))))
	      (5  `(s::best-value
	            (s::solution (list ,vars-name ,p-vars-name)
	             (s::static-ordering ,(case force-function 
	                                   (0 `#'s::linear-force)
	                                   (1 `#'s::divide-and-conquer-force))))
			   ,(cond ((not (null form2)) 
	  			      `(apply ',(first compiled-forms) (list ,vars-name ,p-vars-name)) 
	                  `(apply ',(second compiled-forms) (list ,vars-name ,p-vars-name)))
					  (t `(apply ',compiled-forms (list ,vars-name ,p-vars-name))))))																	  
		))      
	  (1 ;==> REORDER
	     (case val
	      (0  `(s::one-value 
	       (select-solution ',out
	       (s::solution (list ,vars-name ,p-vars-name)
	      (s::reorder ,(case cost-function
	                            (0 `#'s::domain-size)
	                            (1 `#'s::range-size))
	                       ,(case terminate?
	                            (0 `#'(lambda (x) (declare (ignore x)) nil))
	                            (1 `#'(lambda (x) (< x 1e-6)))
				    (2 `#'(lambda (x) (apply ',terminate?-fn (list x)))))
	                       ,(case order
	                            (0 `#'>)
	                            (1 `#'<))
	                       ,(case reorder-force
	                            (0 `#'s::linear-force)
	                            (1 `#'s::divide-and-conquer-force)))))))
	      (1 `(s::all-values 
	           (select-solution ',out                           
	            (s::solution (list ,vars-name ,p-vars-name)
	      (s::reorder ,(case cost-function
	                            (0 `#'s::domain-size)
	                            (1 `#'s::range-size))
	                       ,(case terminate?
	                            (0 `#'(lambda (x) (declare (ignore x)) nil))
	                            (1 `#'(lambda (x) (< x 1e-6)))
				    (2 `#'(lambda (x) (apply ',terminate?-fn (list x)))))
	                       ,(case order 
	                            (0 `#'>)
	                            (1 `#'<))
	                       ,(case reorder-force 
	                            (0 `#'s::linear-force)
	                            (1 `#'s::divide-and-conquer-force)))))))
	      (2  `(s::print-values 
	             (select-solution ',out
	             (s::solution (list ,vars-name ,p-vars-name)
	             (s::reorder ,(case cost-function
	                                (0 `#'s::domain-size)
	                                (1 `#'s::range-size))
	                       ,(case terminate?
	                            (0 `#'(lambda (x) (declare (ignore x)) nil))
	                            (1 `#'(lambda (x) (< x 1e-6)))
				    (2 `#'(lambda (x) (apply ',terminate?-fn (list x)))))
	                       ,(case order 
	                            (0 `#'>)
	                            (1 `#'<))
	                       ,(case reorder-force 
	                            (0 `#'s::linear-force)
	                            (1 `#'s::divide-and-conquer-force)))))))

	      (3  `(om?::n-values ,(second valuation)
	              (select-solution ',out
	               (s::solution (list ,vars-name ,p-vars-name)
	             (s::reorder ,(case cost-function
	                                (0 `#'s::domain-size)
	                                (1 `#'s::range-size))
	                       ,(case terminate?
	                            (0 `#'(lambda (x) (declare (ignore x)) nil))
	                            (1 `#'(lambda (x) (< x 1e-6)))
				    (2 `#'(lambda (x) (apply ',terminate?-fn (list x)))))
	                       ,(case order 
	                            (0 `#'>)
	                            (1 `#'<))
	                       ,(case reorder-force 
	                            (0 `#'s::linear-force)
	                            (1 `#'s::divide-and-conquer-force)))))))

	      (4  `(s::ith-value ,(second valuation)
	              (select-solution ',out
	               (s::solution (list ,vars-name ,p-vars-name)
	             (s::reorder ,(case cost-function
	                                (0 `#'s::domain-size)
	                                (1 `#'s::range-size))
	                       ,(case terminate?
	                            (0 `#'(lambda (x) (declare (ignore x)) nil))
	                            (1 `#'(lambda (x) (< x 1e-6)))
				    (2 `#'(lambda (x) (apply ',terminate?-fn (list x)))))
	                       ,(case order 
	                            (0 `#'>)
	                            (1 `#'<))
	                       ,(case reorder-force 
	                            (0 `#'s::linear-force)
	                            (1 `#'s::divide-and-conquer-force)))))))
	      (5  `(s::best-value
	            (s::solution (list ,vars-name ,p-vars-name)
	             (s::reorder ,(case cost-function
	                                (0 `#'s::domain-size)
	                                (1 `#'s::range-size))
	                       ,(case terminate?
	                            (0 `#'(lambda (x) (declare (ignore x)) nil))
	                            (1 `#'(lambda (x) (< x 1e-6)))
				    (2 `#'(lambda (x) (apply ',terminate?-fn (list x)))))
	                       ,(case order 
	                            (0 `#'>)
	                            (1 `#'<))
	                       ,(case reorder-force 
	                            (0 `#'s::linear-force)
	                            (1 `#'s::divide-and-conquer-force))))
			   ,(cond ((not (null form2)) 
	  			      `(apply ',(first compiled-forms) (list ,vars-name ,p-vars-name)) 
	                             `(apply ',(second compiled-forms) (list ,vars-name ,p-vars-name)))
		              (t `(apply ',compiled-forms (list ,vars-name ,p-vars-name))))))
	       ))))
	(scode ;==> SOLVER CODE
	 (if count-fail? ;===> COUNT FAILURES ON
	 `(s::count-scs-failures
	   (let* ((,vars-name ,(reclist-vars vars))

	            (,p-vars-name ,(if (functionp compiled-p-vars) 
	                                       `(apply ',compiled-p-vars (list ,vars-name))
	                                       `(mapcar #'(lambda (cs) (apply cs (list ,vars-name))) ',compiled-p-vars))))
	         ,constraints 

	         ,p-constraints

	         ,solution-code))

	   `(let* ((,vars-name ,(reclist-vars vars));===> COUNT FAILURES OFF

	            (,p-vars-name ,(if (functionp compiled-p-vars) 
	                                       `(apply ',compiled-p-vars (list ,vars-name))
	                                       `(mapcar #'(lambda (cs) (apply cs (list ,vars-name))) ',compiled-p-vars))))
	         ,constraints 

	         ,p-constraints

	         ,solution-code)))
		)

 (case val 
  (2 (setf *screamer-valuation* 2)))

  (let ((scs-function (compile (eval `(screamer::defun ,(intern (string (gensym)) :s) () ,scode)))) ;==> COMPILED SOLVER FUNCTION
        (scs-time (list (get-internal-run-time) (get-internal-real-time))))

 (print "Timing evaluation of screamer-solver...")

  (let ((solution (funcall scs-function))) ;==> GET THE SOLUTION
   (print-scs-time scs-time)

   (progn (setf *screamer-valuation* pref-valuation)
          (cond ((atom solution) solution)
                ((= val 5) ;==> BEST-VALUE
                 (x-append (select-solution out (first solution))
                                  (second solution))) 
                ((listp out) 
                 (if (third out) (funcall (third out) solution) solution)) ;==> APPLIES AN OM-FUNCTION TO SOLUTION
               (t solution)))
	)
   )			   
  )
 )

(defun reclist-vars (vars)
 (labels ((reclist (x)
           (cond ((atom x) x)  
                 ((and (listp x) (every #'atom x))
		         `(list ,.x))
		         (t `(list ,.(mapcar #'reclist x))))))
  (reclist vars)))
		   
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

(defun select-solution (out solution)
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
                      (t nil))))
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
;;; SCREAMER-DOCUMENTATION

(defmethod! screamer-doc ((function t))
  :initvals '("solution")
  :indoc '("package::function-name")
  :doc "Shows Screamer's documentation of a function." 
  :menuins '((0 (("solution" "solution") 
                        ("static-ordering" "static-ordering") 
                        ("reorder" "reorder" )
                        ("linear-force" "linear-force" )
                        ("divide-and-conquer-force" "divide-and-conquer-force" )
                        ("domain-size" "domain-size")
                        ("range-size" "range-size" )))
                   )
  :icon 487 
(om-show-reference-doc 
 (cond  
  ((equal function "solution") 's::solution)
  ((equal function "static-ordering") 's::static-ordering) 
  ((equal function "reorder") 's::reorder )
  ((equal function "linear-force") 's::linear-force )
  ((equal function "divide-and-conquer-force") 's::divide-and-conquer-force )
  ((equal function "domain-size") 's::domain-size)
  ((equal function "range-size") 's::range-size )
  ((equal function "assert!") 's::assert!)
  ((equal function "best-value") 's::best-value)
(t function))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VARIABLES

(defmethod! screamer-variable ((var string) &optional args) 
  :initvals '("an-integer-betweenv" (0 10))
  :indoc '("screamer-variable" "list")
  :doc "Screamer variable" 
  :menuins '((0 (("an-integer-betweenv" "an-integer-betweenv") 
                         ("a-member-ofv" "a-member-ofv")
                         ("a-random-member-ofv" "a-random-member-ofv")
                         ("an-integerv" "an-integerv") 
                         ("an-integer-abovev" "an-integer-abovev")
                         ("an-integer-belowv" "an-integer-belowv") 
                         ("a-realv" "a-realv") 
                         ("a-real-abovev" "a-real-abovev")									
                         ("a-real-belowv" "a-real-belowv")  
                         ("a-real-betweenv" "a-real-betweenv")
                         ("a-numberv" "a-numberv")
                         ("a-booleanv" "a-booleanv")
                  ))
                   )
  :icon 487
  (cond 
    ((equal var "an-integer-betweenv") (s::an-integer-betweenv (first args) (second args)))
    ((equal var "a-member-ofv") (s::a-member-ofv args))
    ((equal var "a-random-member-ofv") (om?::a-random-member-ofv args))
    ((equal var "a-booleanv") (s::a-booleanv))
    ((equal var "an-integerv") (s::an-integerv))
    ((equal var "an-integer-abovev") (s::an-integer-abovev (first args)))
    ((equal var "an-integer-belowv") (s::an-integer-belowv (first args)))
    ((equal var "a-realv") (s::a-realv))
    ((equal var "a-real-abovev")  (s::a-real-abovev (first args)))					
    ((equal var "a-real-belowv") (s::a-real-belowv (first args)))
    ((equal var "a-real-betweenv") (s::a-real-betweenv (first args) (second args)))
    ((equal var "a-numberv") (s::a-numberv))))

(defmethod! list-ofvs ((n-vars integer) (variables string) &optional args) 
  :initvals '(3 "an-integer-betweenv" (0 10))
  :indoc '("integer" "screamer-variable" "list")
  :doc "List of screamer variables" 
  :menuins '((1 (("an-integer-betweenv" "an-integer-betweenv") 
                         ("a-member-ofv" "a-member-ofv")
                         ("a-random-member-ofv" "a-random-member-ofv")
                         ("an-integerv" "an-integerv") 
                         ("an-integer-abovev" "an-integer-abovev")
                         ("an-integer-belowv" "an-integer-belowv") 
                         ("a-realv" "a-realv") 
                         ("a-real-abovev" "a-real-abovev")									
                         ("a-real-belowv" "a-real-belowv")  
                         ("a-real-betweenv" "a-real-betweenv")
                         ("a-numberv" "a-numberv")
                         ("a-booleanv" "a-booleanv")
                  ))
                   )
  :icon 487
(make-lists-ofv n-vars variables args))

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
                         ("a-real-belowv" "a-real-belowv")  
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
   ((equal var "a-real-belowv") (om?::list-of-reals-belowv n (first args)))
   ((equal var "a-real-betweenv") (om?::list-of-reals-betweenv n (first args) (second args)))
   ((equal var "a-numberv") (om?::list-of-numbersv n))))

(defmethod! list-of-chords-inv ((n-chords list) (domain list) &optional random?) 
  :initvals '((3 4 3) (6000 6200 6400 6500 6700 6900 7100 7200) nil)
  :indoc '("list" "list" "t or nil")
  :menuins '((2 (("nil" 'nil)  ("t" 't))))
  :doc "Generates a list of screamer variables interpeted as CHORDS, that is a list with members of domain with no duplications and in ascending order." 
  :icon 487
 (om?::list-of-chords-inv n-chords domain random?))

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
         (pitch-domain (om?::list-of-random-members-ofv n-notes (reverse domain)))) ;RANDOM? OR om?::list-of-members-ofv

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
