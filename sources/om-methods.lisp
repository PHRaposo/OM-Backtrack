;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :om-screamer)

(defun a-mc->pcv (x)
 (if (bound? x)
     (let ((pc (/ (mod (value-of x) 1200) 100)))      
      pc)

     (let ((pcv (an-integer-betweenv 0 11))
            valx)
      (screamer::attach-noticer!
        #'(lambda ()
           (when (bound? x)
            (setq valx (apply-substitution x))
             (assert! (=v pcv (/v (funcallv #'mod (value-of valx) 1200) 100)))            
           )
         )
       x)
        
       pcv)
  )
 )
	    	  					
(defun absv (k)
(maxv k (*v k -1)))

(defun modv (n d) 
 (funcallv #'mod n d))
 
(defun all-membersv (list sequence)
 (labels ((all-members (x seq)
          (if (null x)
              t
              (andv (memberv (car x) seq)
                    (all-members (cdr x) seq)))))
  (all-members list sequence)))
  
(defun all-membersv-alt (e sequence)
 (let ((sequence-flat (om::flat sequence)))
  (cond ((listp e) (reduce-chunks #'andv (mapcar #'(lambda (x) (memberv x sequence-flat)) (om::flat e))))
         (t (memberv e sequence-flat)))))
 
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

 (om::defmethod! rx->dxv ((listv screamer+::variable+))
 :initvals '((1 2 3 4 5)) :indoc '("variable or list of variables") 
 :doc "Intervals in reverse order (from last to first)."
 :icon 485
(?::mapcarv #'s::-v (?::cdrv (s::funcallv #'reverse listv)) (s::funcallv #'reverse listv)))

 (om::defmethod! rx->dxv ((list list))
 :initvals '((1 2 3 4 5)) :indoc '("variable or list of variables") 
 :doc  "Intervals in reverse order (from last to first)."
 :icon 485
 (om?::x->dxv (reverse list)))

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

(om::defmethod! modv ((n integer) (d integer))
:initvals '(-8 12) :indoc '("variable, number or list" "integer") 
:icon 480
(s::funcallv #'mod n d))

(om::defmethod! modv ((lst list) (d integer))
(mapcar #'modv lst))

(om::defmethod! modv ((var screamer+::variable+)(d integer))
:icon 480
(if (s::variable-number? var)
    (s::funcallv #'mod var d)
    (?::mapcarv #'(lambda (x) (s::funcallv #'mod x d)) var)))
	
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

(om::defmethod! mc->pcv ((n integer))
:initvals '(6000) :indoc '("variable, number or list") 
:icon 479
(om?::a-mc->pcv n))
;(s::/v (om?::modv n 1200) 100))

(om::defmethod! mc->pcv ((n list))
:initvals '((6000 6400 6700)) :indoc '("variable, number or list") 
:icon 479
(mapcar #'mc->pcv n))

(om::defmethod! mc->pcv ((var screamer+::variable+))
:initvals '(6000) :indoc '("variable, number or list") 
:icon 479
 (if (s::variable-number? var)
     (om?::a-mc->pcv var)
     ;(s::/v (om?::modv var 1200) 100)
 (?::mapcarv #'om?::a-mc->pcv var)))
;(?::mapcarv #'(lambda (x) (s::/v (s::funcallv #'mod x 1200) 100) var))))


(om::defmethod! all-membersv ((list list) (sequence list))
:initvals '((11 5 2) (0 2 4 5 7 9 11)) :indoc '("variable or list" "variable or list") 
:icon 477
;(apply #'s::andv (mapcar #'(lambda (x) (s::memberv x sequence)) list)))
(om?::all-membersv-alt list sequence))

(om::defmethod! all-membersv ((listv screamer+::variable+) (sequence list))
:initvals '((11 5 2) (0 2 4 5 7 9 11)) :indoc '("variable or list" "variable or list") 
:icon 477
 (?::everyv #'(lambda (r)(s::equalv r 't)) 
  (?::mapcarv #'(lambda (x) 
                (s::memberv x sequence))
   listv)))

(om::defmethod! all-membersv ((list list) (sequence screamer+::variable+))
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

(om::defmethod! sort-listv ((list t) (direction string))
:initvals '(nil "<") :indoc '("list" "string")
:menuins '((1 (("<" "<") (">" ">"))))
:icon 474
(if (not (s::variable? list))
    (if  (not (some #'s::variable? list))
         (sort list (if (equal direction "<") #'< #'>))
 (if (equal direction "<")
     (s::funcallv #'sort list #'s::<v)
     (s::funcallv #'sort list #'s::>v)))))

(om::defmethod! quadratic-bezier ((p0 number) (p1 number) (p2 number) (steps integer))
 :initvals '(6000 4800 7400 20) 
 :indoc '("number" "number" "number" "integer")
:doc "Solve a quadratic Bezier curve with three points in a given number of steps."
 :icon 473
 (let* ((t-var (interpolation 0 1 steps 0.0))
        (q0 (mapcar #'(lambda (z) 
                                (+ (* (expt (- 1 z) 2) p0) 
                                    (* (- 1 z) (* 2  z) p1) 
                                    (* (expt z 2) p2)))
                t-var)))
(simple-bpf-from-list (om* t-var 1000)
                                  q0)))

(om::defmethod! cubic-bezier ((p0 number) (p1 number) (p2 number) (p3 number) (steps integer)) 
 :initvals '(3600 2100 8400 6000 20) 
 :indoc '("number" "number" "number" "number" "integer")
:doc "Solve a cubic Bezier curve with four points in a given number of steps."
 :icon 473
 (let* ((t-var (interpolation 0 1 steps 0.0))
        (c0 (mapcar #'(lambda (z) 
                                (+ (* (expt (- 1 z) 3) p0) 
                                    (* 3 (expt (- 1 z) 2) z p1) 
                                    (* 3 (- 1 z) (expt z 2) p2) 
                                    (* (expt z 3) p3))) 
                t-var)))
(simple-bpf-from-list (om* t-var 1000)
                                  c0)))

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
 
(setf *function-without-name* 
 (let ((defaults *function-without-name*)) 
  (x-append (list 'om*v 'om-v 'om+v 'om/v) defaults)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; -----------------------------------------		

; UTILS

;;;RHYTHMIC CONSTRUCTOR

(defun measure-tree2ratio (mesure)
 (let* ((signature (car mesure))
          (vals (cadr mesure))
          (ratios (mesure-ratios vals)))
          (om/
           (om* ratios (car signature))
           (cadr signature))))

(defun group-ratios (timesig puls ratios)
 (let* ((tree (mktree ratios timesig))
        (tree-groups (mapcar #'second (second tree))))
  (mapcar #'(lambda (groups pulses)
	        (group-list groups pulses 'linear))	
	tree-groups puls)
  )
 )

(defmethod! cons-tree ((timesig list) (puls list) (subdiv list) (mode string))	
   :initvals '( ( (5 8) (6 8) (6 8)) ((2 3) (2 2 2) (1)) (((1 1) (1 1 1)) ((1 1) (1 1) (1 1)) ((1.0))) "tree")
   :indoc '( "list" "list" "list" "string") 
   :menuins '((3 (("tree" "tree") ("ratio" "ratio"))))
   :doc
"Constructs a rhythmic tree from three arguments: 
(1) A list of time signatures;
(2) A list of lists of pulses subdivisions;
(3) A list of lists of beats subdivisions or a list of ratios.
"   	                   
   :icon 254
 (cond ((equal mode "tree") 
        (list '?
              (mapcar #'(lambda (tim p s)
                         (list tim
                              (mapcar #'list p s)))
               timesig puls subdiv)))
			   
		((equal mode "ratio")	  
	     (let ((ratio-subdiv (group-ratios timesig puls subdiv))) 
         (list '?
               (mapcar #'(lambda (tim p s)
                          (list tim
                               (mapcar #'list p s)))
                timesig puls ratio-subdiv))))
		 
	    (t nil)
  )	   
 )
 
(defun tree-rotations (tree)
 (let* ((ratios (tree2ratio tree))
        (timesig (get-time-sig tree))
        (rot-positions (om?::all-rotations (arithm-ser 0 (1- (length ratios)) 1)))
        (rotations (mapcar #'(lambda (pos)
                         (posn-match ratios pos)) rot-positions)))
 (mapcar #'(lambda (ratios-list)
                (mktree ratios-list timesig)) 
  rotations)))

(defmethod! cons-subdiv ((subdiv list) &optional rest-pos tie-pos)
   :initvals '( (4 3 4 6) () ())
   :indoc '( "list" "list" "list") 
   :doc
"Constructs the subdivisions of a measure.
"   	                   
   :icon 254
 (let ((beats (mapcar #'(lambda (x) (repeat-n 1 x)) subdiv))
        (rests (if (null rest-pos) (repeat-n nil (length subdiv)) rest-pos))
        (ties (if (null tie-pos) (repeat-n nil (length subdiv)) tie-pos)))
(mapcar #'(lambda (beat rests ties)
 (loop for n in beat 
          for x from 0 to (1- (length beat))
          collect (cond ((member x rests) (* -1 n))
                               ((member x ties) (float n))
                               (t n))))
beats rests ties)))

; VOICE-MERGER

(defun voice-merger-internal (list accumul)
(cond  ((null list) accumul)
           ((null accumul) (voice-merger-internal (cdr list) (car list)))
           (t  (voice-merger-internal (cdr list) (merger (car list) accumul)))))

(defmethod! voice-merger ((objs list)) 
   :initvals '(nil) 
   :indoc '("list of voices")
   :icon 253
   :doc "Merges a list of voices into a new voice object."
(voice-merger-internal objs nil))

(defmethod! voice-merger ((self om::poly)) 
   :initvals '(nil) 
   :indoc '("poly object")
   :icon 253
   :doc "Merges a list of voices into a new voice object."
(voice-merger-internal (om::voices self) nil))
