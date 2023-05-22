(in-package :om)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	  
;;;;;;;;;;; OM-MODIFS ADAPTED TO OM 7.2


(defmethod non-deter-patch? ((self OMPatch)) 
 (let ((record (s::get-function-record (intern (string (car (list! (gcode self)))) :om))))
    (not (s::function-record-deterministic? record))))
	
(defmethod non-deter-patch? ((self OMPatchAbs)) 
 (let ((record (s::get-function-record (intern (string (car (list! (code self)))) :om))))
    (not (s::function-record-deterministic? record))))
	
(defmethod non-deter-patch? ((self OMBoxPatch)) 
 (let ((record (s::get-function-record (intern (string (car (list! (code self)))) :om))))
    (not (s::function-record-deterministic? record))))

#|		
(defmethod draw-after-box ((self boxframe))
 (om-with-fg-color self *om-pink-color*
  (when (non-deter-patch? (reference (object self)))
        (om-draw-string (- (round (w self) 2) 8) (+ (round (h self) 2) 6) "?" ))))

|#

(defmethod load-obj-from-obj ((object t))
  (if (or (loaded? object) (member object *loaading-stack* :test 'equal)) object
      (om-with-cursor *om-wait-cursor* 
        (push object *loaading-stack*)
        (om-print (string+ "Loading..." (namestring (mypathname object))))
        (eval-non-text-file (mypathname object))
        (if *om-current-persistent*
           (progn
             (setf (inside object) (inside *om-current-persistent*))
             (setf (params object) (params *om-current-persistent*))
             (setf (mus-patch object) (mus-patch *om-current-persistent*))
	  	   (setf (lisp-exp-p object) (lisp-exp-p *om-current-persistent*))
	  	   (when (lisp-exp-p object)
	  	     (eval `(screamer::defun ,(intern (string (code object)) :om)
	  				     ,.(cdr (eval (lisp-exp-p object))))))
             (setf *om-current-persistent* nil)
            object)
          'dead))))

(defmethod compile-patch ((self OMPatch)) 
 "Generation of lisp code from the graphic boxes."
 (unless (compiled? self)
   (if (lisp-exp-p self)
     (compile (eval `(screamer::defun ,(intern (string (code self)) :om)
                   ,.(cdr (get-lisp-exp (lisp-exp-p self))))))
     (let* ((boxes (boxes self))
            (out-box (find-class-boxes boxes 'OMout))
            (temp-out-box (find-class-boxes boxes 'OMtempOut))
            (self-boxes (patch-has-temp-in-p self))
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

       (eval `(screamer::defun ,(intern (string out-symb) :om)  (,.symbols)
                 (let* ,(reverse *let-list*) ,body)))

       (setf *let-list* oldletlist)
       (setf *lambda-context* oldlambdacontext)
       ))
   (setf (compiled? self) t)))
   
(defmethod compile-patch ((self OMPatchAbs)) 
 "Generation of lisp code from the graphic boxes."
 (unless (compiled? self)
   (if (lisp-exp-p self)
     (compile (eval `(screamer::defun ,(intern (string (code self)) :om)
                   ,.(cdr (get-lisp-exp (lisp-exp-p self))))))
     (let* ((boxes (boxes self))
            (out-box (find-class-boxes boxes 'OMout))
            (temp-out-box (find-class-boxes boxes 'OMtempOut))
            (self-boxes (patch-has-temp-in-p self))
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

       (eval `(screamer::defun ,(intern (string out-symb) :om)  (,.symbols)
                 (let* ,(reverse *let-list*) ,body)))

       (setf *let-list* oldletlist)
       (setf *lambda-context* oldlambdacontext)
       ))
   (setf (compiled? self) t)))
   
(defmethod compile-patch ((self OMBoxPatch)) 
"Generation of lisp code from the graphic boxes."
(unless (compiled? self)
  (if (lisp-exp-p self)
    (compile (eval `(screamer::defun ,(intern (string (code self)) :om)
                  ,.(cdr (get-lisp-exp (lisp-exp-p self))))))
    (let* ((boxes (boxes self))
           (out-box (find-class-boxes boxes 'OMout))
           (temp-out-box (find-class-boxes boxes 'OMtempOut))
           (self-boxes (patch-has-temp-in-p self))
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

      (eval `(screamer::defun ,(intern (string out-symb) :om)  (,.symbols)
                (let* ,(reverse *let-list*) ,body)))

      (setf *let-list* oldletlist)
      (setf *lambda-context* oldlambdacontext)
      ))
  (setf (compiled? self) t)))
   
  (defmethod omNG-box-value ((self OMPatch) &optional (num-out 0))
    (handler-bind ((error #'(lambda (c) 
                              (when *msg-error-label-on*
                                (om-message-dialog (string+ "Error while evaluating the box " (string (name self)) " : " 
                                                         (om-report-condition c ))
                                                :size (om-make-point 300 200))
                                (clear-after-error self)
                                (om-abort)
                                ))))
      (cond
       ((and (equal (allow-lock self) "x") (value self))
        (nth num-out (value self)))
       ((and (equal (allow-lock self) "o") (setf (value self) (list (reference self))) (car (value self))))
       ((equal (allow-lock self) "l")
        (unless (compiled? (reference self))
          (if (and (lisp-exp-p (reference self)) (editorframe self))
            (compile-without-close (editorframe self))
            (compile-patch (reference self))))
        (setf (value self) ;;; test ...
              (list (special-lambda-value self (intern (string (code (reference self))) :om))))
        (car (value self)))
       ((and (equal (allow-lock self) "&") (ev-once-p self)) 
        (nth num-out (value self)))
       (t (let* ((args  (mapcar #'(lambda (input) 
                                 (omNG-box-value  input)) (inputs self)))
              rep non-deter?)
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
            (setf rep (multiple-value-list (apply (intern (string (code (reference self))) :om) args)))      
            (when (equal (allow-lock self) "&")
              (setf (ev-once-p self) t)
              (setf (value self) rep))
            (when (equal (allow-lock self) "x")
              (setf (value self) rep))
            ;;;; TEST
            (when (equal (allow-lock self) nil)
              (setf (value self) rep))
            ;;;;
                (nth num-out rep)))))) 
 
(defmethod omNG-box-value ((self OMPatchAbs) &optional (num-out 0))
  (handler-bind ((error #'(lambda (c) 
                            (when *msg-error-label-on*
                              (om-message-dialog (string+ "Error while evaluating the box " (string (name self)) " : " 
                                                       (om-report-condition c ))
                                              :size (om-make-point 300 200))
                              (clear-after-error self)
                              (om-abort)
                              ))))
    (cond
     ((and (equal (allow-lock self) "x") (value self))
      (nth num-out (value self)))
     ((and (equal (allow-lock self) "o") (setf (value self) (list (reference self))) (car (value self))))
     ((equal (allow-lock self) "l")
      (unless (compiled? (reference self))
        (if (and (lisp-exp-p (reference self)) (editorframe self))
          (compile-without-close (editorframe self))
          (compile-patch (reference self))))
      (setf (value self) ;;; test ...
            (list (special-lambda-value self (intern (string (code (reference self))) :om))))
      (car (value self)))
     ((and (equal (allow-lock self) "&") (ev-once-p self)) 
      (nth num-out (value self)))
     (t (let* ((args  (mapcar #'(lambda (input) 
                               (omNG-box-value  input)) (inputs self)))
            rep non-deter?)
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
          (setf rep (multiple-value-list (apply (intern (string (code (reference self))) :om) args)))      
          (when (equal (allow-lock self) "&")
            (setf (ev-once-p self) t)
            (setf (value self) rep))
          (when (equal (allow-lock self) "x")
            (setf (value self) rep))
          ;;;; TEST
          (when (equal (allow-lock self) nil)
            (setf (value self) rep))
          ;;;;
              (nth num-out rep))))))  

(defmethod omNG-box-value ((self OMBoxPatch) &optional (num-out 0))
  (handler-bind ((error #'(lambda (c) 
                            (when *msg-error-label-on*
                              (om-message-dialog (string+ "Error while evaluating the box " (string (name self)) " : " 
                                                       (om-report-condition c ))
                                              :size (om-make-point 300 200))
                              (clear-after-error self)
                              (om-abort)
                              ))))
    (cond
     ((and (equal (allow-lock self) "x") (value self))
      (nth num-out (value self)))
     ((and (equal (allow-lock self) "o") (setf (value self) (list (reference self))) (car (value self))))
     ((equal (allow-lock self) "l")
      (unless (compiled? (reference self))
        (if (and (lisp-exp-p (reference self)) (editorframe self))
          (compile-without-close (editorframe self))
          (compile-patch (reference self))))
      (setf (value self) ;;; test ...
            (list (special-lambda-value self (intern (string (code (reference self))) :om))))
      (car (value self)))
     ((and (equal (allow-lock self) "&") (ev-once-p self)) 
      (nth num-out (value self)))
     (t (let* ((args  (mapcar #'(lambda (input) 
                               (omNG-box-value  input)) (inputs self)))
            rep non-deter?)
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
          (setf rep (multiple-value-list (apply (intern (string (code (reference self))) :om) args)))      
          (when (equal (allow-lock self) "&")
            (setf (ev-once-p self) t)
            (setf (value self) rep))
          (when (equal (allow-lock self) "x")
            (setf (value self) rep))
          ;;;; TEST
          (when (equal (allow-lock self) nil)
            (setf (value self) rep))
          ;;;;
              (nth num-out rep))))))
	