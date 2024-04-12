;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; REVISED VERSION
;;; Copyright 2024 PAULO HENRIQUE RAPOSO AND KARIM HADDAD

(in-package :om)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; "?" - NONDETERMINISTIC PATCH

(defmethod non-deter-patch? ((self OMPatch)) 
 (let* ((boxes (boxes self))
	    (screamerboxes (find-class-boxes boxes 'screamerboxes))
		(boxcalls (find-class-boxes boxes 'omboxcall))
		(lispfuns (find-class-boxes boxes 'omboxlispcall))
		(symbols  (loop for x in (remove nil (flat (mapcar (lambda (x) (gen-code x 0)) boxcalls))) 
		                                if (symbolp x) collect x))
		(apply-cont?  (remove-if-not (lambda (x) (equal x 'apply-cont)) symbols)))
  (if (or screamerboxes apply-cont? (some #'non-deter-lispfun? lispfuns)) t nil)))
  
(defun non-deter-lispfun? (omlispfun)
 (let ((record (screamer::get-function-record (car (gen-code omlispfun 0)))))
 (not (screamer::function-record-deterministic? record))))

(defmethod om-draw-contents :after ((self patch-icon-box))
  (when (non-deter-patch? (reference (object (om-view-container self))))
   (om-with-fg-color self *om-pink-color*
    (om-with-font (om-make-font "Courier" (* *icon-size-factor* 34))
      (om-draw-char (- (round (w self) 2) (* *icon-size-factor* 10)) 
		            (+ (round (h self) 2) (* *icon-size-factor* 10)) 
	    #\?)))))

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
	     (non-deter-patch? (reference self)))		 	 		 	 
	      (om-message-dialog "Nondeterministic patches in lambda mode has not been implemented yet.")
              (clear-after-error self)
              (om-abort))	
				  				             				 	 			
    ((and (equal (allow-lock self) "x") (value self))
     (nth num-out (value self)))	 
		 
	((and (equal (allow-lock self) "o")
	 (setf (value self) (list (reference self))) (car (value self))))
	 	 
    ((equal (allow-lock self) "l")
     (unless (compiled? (reference self))
       (if (and (lisp-exp-p (reference self)) (editorframe self))
         (compile-without-close (editorframe self))          
          (compile-patch (reference self))))				 	 				  
         (setf (value self) ;;; test ...  => OM 7.2
                (list (special-lambda-value self (intern (string (code (reference self))) :om))))
         (car (value self)))
		 		  
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COMPILE-PATCH

(defmethod compile-patch ((self OMPatch)) 
 "Generation of lisp code from the graphic boxes."
	(unless (compiled? self)
	(if (lisp-exp-p self)		
	  (compile (eval `(defun ,(intern (string (code self)) :om)
	            ,.(cdr (get-lisp-exp (lisp-exp self)))))) 					
	(let* ((boxes (boxes self))
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
	  (setf body `(values ,.(mapcar #'(lambda (theout)
					               (gen-code theout 0)) out-box)))
	  (if (non-deter-patch? self) 								   
	      (eval `(screamer::defun ,(intern (string out-symb) :om)  (,.symbols)
	              (let* ,(reverse *let-list*) ,body)))
	      (eval `(defun ,(intern (string out-symb) :om)  (,.symbols)
	              (let* ,(reverse *let-list*) ,body)))
	  )		  	  				 						 						 		 	 
	  (setf *let-list* oldletlist)
	  (setf *lambda-context* oldlambdacontext)
		   ))	
	(setf (compiled? self) t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
;;;TODO (IF NEEDED)
;;; => OMLISPPATCH 
;;; => NONDETERMINISTIC PATCHES IN LAMBDA MODE
;;; => OMLOOP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

