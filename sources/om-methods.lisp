(in-package :om-screamer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ADAPTED FROM SCREAMER PLUS 

(defun list-elements-ofv (x)
  (let (
        (z (make-variable))
        )

     (?::attach-noticer!
       #'(lambda()
           (when (and (bound? x) (every #'bound? (value-of x)))
              (do* (
                    (dec (?::apply-substitution x) (cdr dec))
                    (curr (car dec) (car dec))
                    (vals nil)
                    )
                  ((endp dec) 
                   (assert! (equalv z vals))
                   )
                 (push (value-of curr) vals)
                 )
              )
           )
       x)
    (funcallv #'reverse z)
  )
)

(defun get-variable-type (var)
  (declare (ignore print-level))
  (let ((x (value-of var)))
    (cond
      ((?::variable? x) 
        (if (and (not (equal (s::variable-enumerated-domain x) t))
                           (not (null (s::variable-enumerated-antidomain x))))
                       (error "This shouldn't happen"))     
          (cond ((?::variable-type-known? x) (?::variable-get-type x))
            ((screamer::variable-boolean? x) 'boolean)
            ((screamer::variable-integer? x) 'integer)
            ((screamer::variable-real? x) (if (screamer::variable-noninteger? x)
                                            'noninteger-real
                                            'real))            
            ((screamer::variable-number? x)
              (cond ((screamer::variable-nonreal? x) 'nonreal-number)
                ((screamer::variable-noninteger? x)  'noninteger-number)
                (t 'number)))            
            ((screamer::variable-nonnumber? x) (intern "nonnumber" :om))
            ((screamer::variable-nonreal? x) (intern "nonreal" :om))
            ((screamer::variable-noninteger? x) (intern"noninteger" :om))
            (t 'variable))))))
			
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	    
(defun reclistv-vars (vars)
 (labels ((reclistv (x)
           (cond ((atom x) x)  
                     ((and (listp x) (every #'atom x))
		       (list-elements-ofv x))
		     (t  (mapcar #'reclistv x)))))
  (reclistv vars)))	
	  					
(defun absv (k)
   (maxv k (*v k -1)))

(defun modv (n d) 
 (funcallv #'mod n d))
 
(defun all-membersv (list sequence)
 (labels ((all-members (x seq)
          (if (null x)
              t
              (andv (memberv (carv x) seq)
                    (all-members (cdrv x) seq)))))
  (all-members list sequence)))
 
 (defun x->dxv (list)
  (mapcar #'(lambda (x y) (-v y x)) list (cdr list)))

 (defun dx->xv (start list)
  (dx->xv-internal start list nil))

 (defun dx->xv-internal (start list accumul)
      (let ((sum (if accumul 
                      (+v (first list) (first accumul))
                      (+v start (first list)))))
 (if (cdr list)
     (dx->xv-internal start (cdr list) (om::x-append sum accumul))
     (om::x-append start (reverse accumul) sum))))

 (defun dx->xv-listv (start list)
  (dx->xv-internal-listv start list nil))
 	
 (defun dx->xv-internal-listv (start list accumul)
      (let ((sum (ifv accumul 
                      (+v (firstv list) (firstv accumul))
                      (+v start (firstv list)))))
 (ifv (cdrv list)
     (dx->xv-internal-listv start (cdrv list) (consv sum accumul))
     (consv start (appendv (funcallv #'reverse accumul) (list sum))))))

 (defun x->dx-absv (list)
  (mapcar #'(lambda (x y) (absv (-v y x))) list (cdr list)))
 
 ;;;DEEP-MAPCAR - FROM OM AND ESQUISSE

 (defun deep-mapcar (fun fun1 list? &rest args)
   "Mapcars <fun> or applies <fun1> to <list?> <args> whether <list?> is a list or not."
   (cond
     ((null list?) ())
     ((not (consp list?)) (apply fun1 list? args))
     (t (cons (apply #'deep-mapcar fun fun1 (car list?) args)
 	     (apply #'deep-mapcar fun fun1 (cdr list?) args)))))
		 
 (defun car-mapcar (fun list?  &rest args)
    "Mapcars <fun> if list? is a list or applies <fun> if it is an atom or
  a one-element list"
    (cond  ((atom list?) (apply fun list? args))
           ((= (length list?) 1) (apply fun (car list?) args))
           (t (mapcar #'(lambda (x) (apply fun x  args ))  list? ))))

 (defun less-deep-mapcar (fun  list? &rest args)
    "Applies <fun> to <list?> <args> if <list?> is a one-level list .
     Mapcars <fun> to <list?> <args> if <list?> is a multi-level list. "
    (cond
      ((null list?) ())
      ((atom (car list?)) (apply fun list? args))
      ((atom (car (car list?))) 
       (cons (apply fun (car list?)  args ) (apply #'less-deep-mapcar fun (cdr list?) args)))
      (t (cons (apply #'less-deep-mapcar fun  (car list?) args)
               (apply #'less-deep-mapcar fun  (cdr list?) args)))))
 
(defun apply-rec (fn list) (apply-rec-internal fn list nil))

(defun apply-rec-internal (fn list accumul)
 (let* ((fn-inputs (length (om::function-lambda-list fn)))
        (list-inputs (all-values (an-integer-between 0 (1- fn-inputs)))))
 (if (null (nth (1- fn-inputs) list))
      accumul
      (let ((one-result (apply fn (mapcar #'(lambda (n) 
			  	                 (nth n list)) list-inputs)))) 
	(apply-rec-internal fn (cdr list) (om::x-append accumul one-result))
 ))))

(defun funcallv-rec-car-cdr (fn list)
   (labels ((funcall-car-cdr (x xs)
                (ifv (null xs)
                 t
                 (andv (funcallv fn x xs)
                           (funcall-car-cdr (carv xs) (cdrv xs))))))
    (funcall-car-cdr (carv list) (cdrv list))))
					
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OM METHODS 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :om)

; APPLY-CONTV

(om::defmethod! apply-contv ((cs function) (mode string) (recursive? string) (vars t))  
:initvals '(nil "atom" "off" nil) 
:indoc '("patch in lambda mode" "string" "string" "list of variables" ) 
:menuins '((1 (("atom" "atom") ("list" "list")))
                 (2 (("off" "off") ("n-inputs" "n-inputs") ("car-cdr" "car-cdr")))
                )
:doc "Applies constraint recursively to list of variables."
:icon 487

(cond ((equal mode "atom") 
        (om?::deep-mapcar cs cs vars))
           
          ((equal mode "list") 
           (cond 

           ((equal recursive? "n-inputs")
           (om?::apply-rec cs vars))

           ((equal recursive? "car-cdr") 
           (om?::funcallv-rec-car-cdr cs vars))

           (t (om?::less-deep-mapcar cs vars))
          ))

         (t (progn (om-message-dialog "ERROR!") (om-abort)))))

 (om::defmethod! x->dxv ((list list))
 :initvals '((1 2 3 4 5)) :indoc '("variable or list of variables") 
 :doc ""
 :icon 485
 (om?::x->dxv list))

 (om::defmethod! x->dxv ((listv screamer+::variable+))
 :initvals '((1 2 3 4 5)) :indoc '("variable or list of variables") 
 :doc ""
 :icon 485
(?::mapcarv #'s::-v (?::cdrv listv) listv))

 (om::defmethod! x->dx-absv ((list list))
 :initvals '((1 2 3 4 5)) :indoc '("variable or list of variables") 
 :doc ""
 :icon 478
 (om?::x->dx-absv list))

 (om::defmethod! x->dx-absv ((listv screamer+::variable+))
 :initvals '((1 2 3 4 5)) :indoc '("variable or list of variables") 
 :doc ""
 :icon 478
 (?::mapcarv #'(lambda (x y) (om?::absv (s::-v y x))) listv (?::cdrv listv)))

 (om::defmethod! dx->xv ((start number) (list list))
 :initvals '(0 (1 2 3 4 5)) :indoc '("variable or number" "variable, list of variables or list") 
 :doc ""
 :icon 485
 (om?::dx->xv start list))

 (om::defmethod! dx->xv ((start number) (listv screamer+::variable+))
 :initvals '(0 (1 2 3 4 5)) :indoc '("variable or number" "variable, list of variables or list") 
 :doc ""
 :icon 485
 (om?::dx->xv-listv start listv))
 
 (om::defmethod! not-intersectionv ((list1 list) (list2 list))
 :initvals '((0 2 4) (1 3 5)) :indoc '("list" "list") 
 :doc ""
 :icon 477
 (s::=v (?::lengthv (?::intersectionv list1 list2)) 0))
 
; -----------------------------------------
; OM+V / OM-V / OM*V / OM/V / MOD12V / MC->PCV / OM-ABSV

(om::defmethod! om-absv ((n number))
:initvals '(-8) :indoc '("variable, number or list") 
:icon 480
(om?::absv n))

(om::defmethod! om-absv ((lst list))
(mapcar #'om?::absv lst))

(om::defmethod! om-absv ((var screamer+::variable+))
:icon 480
(if (s::variable-number? var)
    (om?::absv var)
    (?::mapcarv #'(lambda (x) (om?::absv x)) var)))
	
(om::defmethod! mod12v ((n integer))
:initvals '(-8) :indoc '("variable, number or list") 
:icon 480
(s::funcallv #'mod n 12))

(om::defmethod! mod12v ((lst list))
(mapcar #'mod12v lst))

(om::defmethod! mod12v ((var screamer+::variable+))
:icon 480
(if (s::variable-number? var)
    (s::funcallv #'mod var 12)
    (?::mapcarv #'(lambda (x) (s::funcallv #'mod x 12)) var)))

(om::defmethod! mc->pcv ((n t))
:initvals '(6000) :indoc '("variable, number or list") 
:icon 479
 (s::/v (om?::modv n 1200) 100))

(om::defmethod! mc->pcv ((n list))
:initvals '((6000 6400 6700)) :indoc '("variable, number or list") 
:icon 479
(mapcar #'mc->pcv n))

(om::defmethod! mc->pcv ((var screamer+::variable+))
:initvals '(6000) :indoc '("variable, number or list") 
:icon 479
 (if (s::variable-number? var)
    (s::/v (om?::modv var 1200) 100)
   (?::mapcarv #'(lambda (x) (s::/v (om?::modv x 1200) 100)) var)))

(om::defmethod! all-membersv ((list list) (sequence list))
:initvals '((11 5 2) (0 2 4 5 7 9 11)) :indoc '("variable or list" "variable or list") 
:icon 477
(om?::all-membersv list sequence))

(om::defmethod! all-membersv ((listv screamer+::variable+) (sequence list))
:initvals '((11 5 2) (0 2 4 5 7 9 11)) :indoc '("variable or list" "variable or list") 
:icon 477
 (?::everyv #'(lambda (r)(s::equalv r 't)) 
  (?::mapcarv #'(lambda (x) 
                (s::memberv x sequence))
   listv)))

(om::defmethod! all-membersv ((list screamer+::variable+) (sequence screamer+::variable+))
:initvals '((11 5 2) (0 2 4 5 7 9 11)) :indoc '("variable or list" "variable or list") 
:icon 477
 (?::everyv #'(lambda (r)(s::equalv r 't)) 
  (?::mapcarv #'(lambda (x) 
                (s::memberv x sequence))
   list)))

(om::defmethod! all-diffv ((list t))
:initvals '(nil) :indoc '("list") 
:icon 477
(if (listp list)
(apply 's::/=v list)
(s::applyv 's::/=v list)))

; -----------------------------------------

(om::defmethod! om*v ((arg1 t) (arg2 t))  
:initvals '(0 0) :indoc '("variable, number or list" "variable, number or list") 
:doc ""
:icon 483
(s::*v arg1 arg2))

(om::defmethod! om*v ((arg1 t) (arg2 list))
(mapcar #'(lambda (input)
            (om*v arg1 input)) arg2))

(om::defmethod! om*v ((arg1 list) (arg2 t)) 
(mapcar #'(lambda (input)
            (om*v  input arg2)) arg1))

; -----------------------------------------

(om::defmethod! om+v ((arg1 t) (arg2 t))  
:initvals '(0 0) :indoc '("variable, number or list" "variable, number or list") 
:doc ""
:icon 481
(s::+v arg1 arg2))

(om::defmethod! om+v ((arg1 t) (arg2 list))
(mapcar #'(lambda (input)
          (om+v arg1 input)) arg2))

(om::defmethod! om+v ((arg1 list) (arg2 t)) 
(mapcar #'(lambda (input)
          (om+v  input arg2)) arg1))

; -----------------------------------------

(om::defmethod! om-v ((arg1 t) (arg2 t))  
:initvals '(0 0) :indoc '("variable, number or list" "variable, number or list") 
:doc ""
:icon 484
(s::-v arg1 arg2))

(om::defmethod! om-v ((arg1 t) (arg2 list))
(mapcar #'(lambda (input)
          (om-v arg1 input)) arg2))

(om::defmethod! om-v ((arg1 list) (arg2 t)) 
(mapcar #'(lambda (input)
          (om-v  input arg2)) arg1))

; -----------------------------------------

(om::defmethod! om/v ((arg1 t) (arg2 t))  
:initvals '(1 1) :indoc '("variable, number or list" "variable, number or list") 
:doc ""
:icon 482
(s::/v arg1 arg2))

(om::defmethod! om/v ((arg1 t) (arg2 list))
(mapcar #'(lambda (input)
          (om/v arg1 input)) arg2))

(om::defmethod! om/v ((arg1 list) (arg2 t)) 
(mapcar #'(lambda (input)
          (om/v  input arg2)) arg1))

; -----------------------------------------		

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;; UPDATE FUNCTIONS-WITHOUT-NAME
 
(setf *function-without-name* (let ((defaults *function-without-name*)) (x-append (list 'om*v 'om-v 'om+v 'om/v) defaults)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;