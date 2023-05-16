(in-package :om)

(defmethod! screamer-solver ((screamer-val integer)(const t) &rest opp-cons)
  :numouts 1 
  :initvals '(0 nil nil) 
  :indoc '("screamer-valuation-mode" "variable" "constraint")
  :doc "Solve a csp"
  :menuins '((0 (("One-value" 0) ("All-values" 1) ("Listener" 2)))) 
  :icon 486	  
  (setf *screamer-valuation* screamer-val)

;;; COLLECT VARS - EXPRS
;;;STRANSLATE-CSP
;;; SOLVE-CSP ;; ONE-VALUE OR ALL-VALUES

nil)
  
(defmethod translate-csp-test ((vars t) (constraintes t) (n number) (ordering t) (force-function t))
`(let* ((variable ,vars))
  (list (s::assert! (funcall ,constraintes variable ,n)))
(s::solution
variable
  (funcall ,ordering ,force-function))))

(defmethod translate-csp-2 ((expr t) (ordering t) (force-function t))
`(let* ,(second expr)
  ,(third expr)
(s::solution (list ,. (mapcar #'(lambda (input) (first input)) (second expr)))
  (funcall ,ordering ,force-function)))) 

(defmethod solve-csp-all ((exp list))
(eval `(s::all-values ,exp)))

(defmethod solve-csp-one ((exp list))
(eval `(s::one-value ,exp)))
	 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VARIABLES

(defmethod! an-integerv (&rest n)
(declare (ignore n))
(s::an-integerv))

(defmethod! an-integer-betweenv ((n1 number) (n2 number))
(s::an-integer-betweenv n1 n2))

(defmethod! a-member-ofv ((lst list))
(s::a-member-ofv list))

; ================================================================= ;
;adapted from OM-Math-Tools

(defun make-n-realsv (nbVar)
(loop for k from 1 to nbVar
   collect (s::a-realv)))

(defun make-n-integers-betweenv (nbVar min max)
(loop for k from 1 to nbVar
   collect (s::an-integer-betweenv min max)))

(defun make-n-members-ofv (nbVar lst)
(loop for k from 1 to nbVar
   collect (s::a-member-ofv lst)))

; =================================================================;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PREDICATES

(defmethod! >v ((var screamer::variable) (arg number) &rest vars)
(if (null vars)

`(let* ((variable ,var))
  (list (s::assert! (funcall 's::>v variable ,arg))))
  
  `(let* ((variable ,var))
    (list (s::assert! (funcall 's::>v variable ,arg ,(first vars)))))))

(defmethod! >v ((arg number) (var screamer::variable) &rest vars) 
(if (null vars)

`(let* ((variable ,var))
  (list (s::assert! (funcall 's::>v ,arg variable))))

`(let* ((variable ,var))
  (list (s::assert! (funcall 's::>v ,arg variable ,(first vars)))))))

(defmethod! >v ((var1 screamer::variable) (var2 screamer::variable) &rest vars) 
(if (null vars)

`(let* ((variable1 ,var1) (variable2 ,var2))
  (list (s::assert! (funcall 's::>v  variable1 variable2))))

`(let* ((VarArray (list ,var1 ,var2 ,.vars)))
  (list (eval `(s::assert! (funcall 's::>v ,. VarArray)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ORDERING

(defmethod! static-ordering (&rest n)
(declare (ignore n))
(quote 's::static-ordering))

(defmethod! reorder (&rest n)
(declare (ignore n))
(quote 's::reorder))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FORCE-FUNCTION

(defmethod! linear-force (&rest n)
(declare (ignore n))
'#'s::linear-force)

(defmethod! divide-and-conquer-force (&rest n)
(declare (ignore n))
'#'s::divide-and-conquer-force)


