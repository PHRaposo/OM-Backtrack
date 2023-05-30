(in-package :om)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	  
;;;;;;;;;;; OM-MODIFS ADAPTED TO OM 7.2

(defmethod non-deter-patch? ((self OMPatch)) 
(let ((record (s::get-function-record (intern (string (car (list! (code self)))) :om))))
(not (s::function-record-deterministic? record))))

(defmethod omNG-box-value ((self OMBoxPatch) &optional (num-out 0))
(handler-bind ((error #'(lambda (c)
                          (when *msg-error-label-on*
                            (om-message-dialog (string+ "Error while evaluating the box " (string (name self)) " : " 
                                                     (om-report-condition c))
                                               :size (om-make-point 300 200))
                            (clear-after-error self)
                            (om-abort)))))
   (cond
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
			,.(cdr (get-lisp-exp self)))))						
  (let* ((boxes (boxes self))
     (vars (find-class-boxes boxes 'screamerboxes))
     (temp-out-box (find-class-boxes boxes 'OMtempOut))
     (self-boxes (patch-has-temp-in-p self)) 
     (out-box (find-class-boxes boxes 'OMout))
     (in-boxes (find-class-boxes boxes 'OMin))
     (out-symb (code self))
     (oldletlist *let-list*)
     (oldlambdacontext *lambda-context*)		 
	  symbols body)
(setf out-box (sort out-box '< :key 'indice))
(setf in-boxes (sort in-boxes '< :key 'indice))
(setf symbols (mapcar #'(lambda (thein)
			  (setf (in-symbol thein) (gensym))) in-boxes))
(setf *let-list* nil)
(mapc #'(lambda (thevar) (gen-code thevar 0)) vars)
(setf body `(values ,.(mapcar #'(lambda (theout)
				  (gen-code theout 0)) out-box)))
    (setf *let-list* oldletlist)
	(setf *lambda-context* oldlambdacontext)
				 
         (eval  `(screamer::defun ,(intern (string out-symb) :om)  (,.symbols)
		 				 (let* ,*let-list* ,body)))					 
				 				 
       (setf *let-list* oldletlist)
       (setf *lambda-context* oldlambdacontext)
	   ))	
(setf (compiled? self) t)))	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;;; TODO - DRAW-AFTER-BOX - OMLOOP - LAMBDA-LISP-PATCH

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DRAW-AFTER-BOX

(defmethod draw-after-box ((self patchboxframe)) ; POSSIBLE OM 7 VERSION 	
 (setf non-deter (non-deter-patch? (reference (object self))))
 (eval `(print ,non-deter))		
;(om-with-fg-color self *om-pink-color*
;(when (non-deter-patch? (reference (object self)))
 ;   (om-draw-char  (- (round (w self) 2) 8) (+ (round (h self) 2) 6) #\?))) 
 ) 
 
 #|
 ; OM 4
 DRAW-AFTER-BOX 
 => BOXFRAME.LISP => MAQUETTEFRAME.LISP = >
 ;screamer
 (defmethod draw-after-box ((self patchboxFrame))
   (with-fore-color 16719095
     (when (non-deter-patch? (reference (object self)))
       (#_Textsize 28)
       (draw-char (- (round (w self) 2) 8) (+ (round (h self) 2) 6) #\? ))))

 (defmethod draw-after-box ((self loopboxframe))
 |#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		 

