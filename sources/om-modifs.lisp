(in-package :om)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FROM OM 4.71b ;;;;;;;
;;; save.lisp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OM 7

(defun load-obj-from-obj (object)
  (if (or (loaded? object)
	  (member object *loaading-stack* :test 'equal)) object
    (with-cursor
     *watch-cursor* 
     (push object *loaading-stack*)
     (print (string+ "Loading..." (mac-namestring (mypathname object))))
     (eval-non-text-file (mypathname object))
     (if *om-current-persistent*
	 (progn
	   (setf (boxes object) nil)
	   (setf (connec object) (connec *om-current-persistent*))
	   (mapc #'(lambda (box) (omNG-add-element object (eval box)))
		 (boxes *om-current-persistent*))
	   (setf (boxes object) (reverse (boxes object)))
	   (load-picts object)
	   (setf (lisp-exp-p object) (lisp-exp-p *om-current-persistent*))
	   (when (lisp-exp-p object)
	     (eval `(screamer::defun ,(intern (string (code object)) :om)
				     ,.(cdr (eval (lisp-exp-p object))))))
	   (setf *om-current-persistent* nil)
	   object) 
       'dead))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	   
;;; OM 4

(defun load-obj-from-obj (object)
  (if (or (loaded? object) (member object *loaading-stack* :test 'equal)) object
      (with-cursor *watch-cursor* 
        (push object *loaading-stack*)
        (print (string+ "Loading..." (mac-namestring (mypathname object))))
        (eval-non-text-file (mypathname object))
        (if *om-current-persistent*
          (progn
            (setf (boxes object) nil)
            (setf (connec object) (connec *om-current-persistent*))
            (mapc #'(lambda (box) (omNG-add-element object box)) (reverse (boxes *om-current-persistent*)))
            (load-picts object)
            (setf (lisp-exp-p object) (lisp-exp-p *om-current-persistent*))
            (when (lisp-exp-p object)
              (eval `(screamer::defun ,(intern (string (code object)) :om)
                                      ,.(cdr (eval (lisp-exp-p object))))))
            (setf *om-current-persistent* nil)
            object) 
          'dead))))
	   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PatchContainer.lisp

;;; OM 4
(defmethod window-close ((self patch-lambda-exp-window))
   (handler-bind ((error #'(lambda (c) (declare (ignore c))
                            (window-select self)
                            (let ((rep  (y-or-n-dialog "Patch Definition Error. 
   Do you want to close the window?
   If yes no modification will be make to the patch")))
                              (when rep
                                (setf *abort-definition* t)
                                (window-close self))
                              (abort)))))
     (unless *abort-definition*
       (let* ((size (buffer-size (fred-buffer self)))
              (pos (buffer-skip-fwd-wsp&comments (fred-buffer self) 0 size))
              (expression (buffer-current-sexp (fred-buffer self) pos)))
         (unless (ccl::lambda-expression-p expression)
           (beep-msg "Error! this is not a lambda expression. Lambda expressions are of the form '(lambda <param-list> <body>)")
           (error "error"))
         (unless (equal (lisp-exp-p (patchref self)) expression)
           (setf (lisp-exp-p (patchref self)) expression)
           (eval `(screamer::defun ,(intern (string (code (patchref self))) :om)
                                   ,.(cdr (lisp-exp-p (patchref self)))))
           
           (loop for item in (attached-objs (patchref self)) do
                 (update-from-reference item)))))
     (setf (compiled? (patchref self)) t)
     (setf (editorframe (patchref self)) nil)
     (setf *abort-definition* nil)
     (call-next-method)))
	 
(defmethod compile-without-close ((self patch-lambda-exp-window))
   (let* ((size (buffer-size (fred-buffer self)))
         (pos (buffer-skip-fwd-wsp&comments (fred-buffer self) 0 size))
         (expression (buffer-current-sexp (fred-buffer self) pos)))
    (unless (ccl::lambda-expression-p expression)
      (beep-msg "Error! Expression in Patch definition is not a lambda expression. 
Lambda expression are the form '(lambda <param-list> <body>")
      (abort))
    (unless (equal (lisp-exp-p (patchref self)) expression)
      (setf (lisp-exp-p (patchref self)) expression)
      (eval `(screamer::defun ,(intern (string (code (patchref self))) :om)
                              ,.(cdr (lisp-exp-p (patchref self)))))
      (loop for item in (attached-objs (patchref self)) do
            (update-from-reference item)))
    (setf (compiled? (patchref self)) t)))
	  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Events.lisp

;;; OM 4
(defmethod window-close ((self patch-lambda-exp-window))
  (handler-bind
   ((error #'(lambda (c) (declare (ignore c))
	       (window-select self)
	       (let ((rep  (y-or-n-dialog "Patch Definition Error. Do you want to close the window? If yes no modification will be make to the patch")))
		 (when rep
		   (setf *abort-definition* t)
		   (window-close self))
		 (abort)))))
   (unless *abort-definition*
     (let* ((size (buffer-size (fred-buffer self)))
	    (pos (buffer-skip-fwd-wsp&comments (fred-buffer self) 0 size))
	    (expression (buffer-current-sexp (fred-buffer self) pos)))
       (unless (ccl::lambda-expression-p expression)
	 (beep-msg "Error! this is not a lambda expression. Lambda expressions are of the form '(lambda <param-list> <body>)")
	 (error "error"))
       (unless (equal (lisp-exp-p (patchref self)) expression)
	 (setf (lisp-exp-p (patchref self)) expression)
	 (eval `(screamer::defun ,(intern (string (code (patchref self))) :om)
				 ,.(cdr (lisp-exp-p (patchref self)))))
	 
	 (loop for item in (attached-objs (patchref self)) do
	       (update-from-reference item)))))
   (setf (compiled? (patchref self)) t)
   (setf (editorframe (patchref self)) nil)
   (setf *abort-definition* nil)
   (call-next-method)))
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OM 7   
   (defmethod om-window-close-event ((self patch-lambda-exp-window))
     (handler-bind ((error #'(lambda (c) 
                               (om-message-dialog (format nil "Patch Definition Error: ~S. ~%No modification will be make to the patch." (om-report-condition c)))
                               (om-kill-window-buffer self)
                               (when (patchref self)
                                 (setf (editorframe (patchref self)) nil))
                               (setf *patch-abort-definition* nil)
                               (om-abort))))
       (loop for item in (attached-objs (patchref self)) do
             (update-from-reference item))
       (setf (compiled? (patchref self)) t)
       (setf (editorframe (patchref self)) nil)
       (setf *patch-abort-definition* nil)
       (om-kill-window-buffer self)
       (call-next-method)
       ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OM 7   
(defmethod compile-before-close ((self patch-lambda-exp-window))
 (handler-bind ((error #'(lambda (c) 
                           (declare (ignore c))
                           (setf *patch-definition-aborted* 
                                 (string+ "Error in function definition: " 
                                          (om-report-condition c) 
                                          ".~%~%Close editor anyway?~%(No modification will be made to the patch)."))
                           )))
 (let* ((expression (om-get-lisp-expression self)))
   (unless (lambda-expression-p expression)
     (setf *patch-abort-definition* "Error in lambda expression.~%Lambda expressions are of the form (lambda <param-list> <body>).~%~%Close editor anyway?~%(No modification will be made to the patch)."))
   (unless *patch-abort-definition*
     ;(setf (lisp-exp-p (patchref self)) expression)
     (setf (lisp-exp (patchref self)) (om-get-text self))
     (eval `(screamer::defun ,(intern (string (code (patchref self))) :om)
                   ,.(cdr (get-lisp-exp (lisp-exp (patchref self))))))
     ))))
	 
(defmethod compile-without-close ((self patch-lambda-exp-window))
(let* ((expression (om-get-lisp-expression self)))
 (unless (lambda-expression-p expression)
   (om-message-dialog (string+ "Error! The expression in the Lisp patch" (name (patchref self)) " is not a valid lambda expression. 
Lambda expression are of the form (lambda <param-list> <body>)"))
   (om-abort))
 (unless (equal (get-lisp-exp (lisp-exp (patchref self))) expression)
   ;;;(setf (lisp-exp-p (patchref self)) expression)
   (setf (lisp-exp (patchref self)) (om-get-text self))
   (compile-lisp-patch-fun (patchref self))
   (loop for item in (attached-objs (patchref self)) do
         (update-from-reference item)))
 (setf (compiled? (patchref self)) t)))	 
	 
(defun compile-lisp-patch-fun (patch)
(if (get-lisp-exp (lisp-exp patch))
   (eval `(defun ,(intern (string (code patch)) :om)
                 ,.(cdr (get-lisp-exp (lisp-exp patch)))))
 (eval `(defun ,(intern (string (code patch)) :om) () nil))))


(defmethod compile-patch ((self OMLispPatch)) 
"Generation of lisp code from the graphic boxes."
(unless (compiled? self)
 (handler-bind 
     ((error #'(lambda (err)
                 (capi::display-message "An error of type ~a occurred: ~a" (type-of err) (format nil "~A" err))
                 (abort err))))
   (compile-lisp-patch-fun self)
   (setf (compiled? self) t))
 ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; omloop.lisp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OM 7

(defmethod compile-patch ((self patchForLoop))
   "Code generates by Loop patches is generate by this method."
   (let* ((boxes (boxes self))
          (oldletlist *let-list*)
          (*let-list* nil)
          (oldlambdacontext *lambda-context*)
          (do-box (car (find-class-boxes boxes 'OMLoopDo)))
          (init-box (car (find-class-boxes boxes 'OMinitDo)))
          (final-box (car (find-class-boxes boxes 'OMFinalDo)))
          (in-boxes (sort (find-class-boxes boxes 'OMin) '< :key 'indice))
          (symbols (mapcar #'(lambda (thein) (setf (in-symbol thein) (gensym))) in-boxes))
          (loop-boxes (find-loop-boxes boxes))
          (loop-code (mapcan #'(lambda (theout)
                                 (loop-gen-code theout 0)) loop-boxes))
          (acum-boxes (find-acum-boxes boxes))
          (acum-declaration (mapcar #'(lambda (acumm)
                                        (declare-closure acumm)) acum-boxes))
          (acum-inits (mapcar #'(lambda (acumm)
                                  (gen-code acumm -1)) acum-boxes))
          body init)
     
     (setf init (gen-code init-box 0))
     (setf body (gen-code do-box 0))	 
     ;(setf final (loop for i from 0 to (1- (length (inputs final-box))) collect (gen-code final-box i)))  ;;; OM 7
     ;(eval (gen-loop-function (intern (string (first (code self))) :om) 
     ;                     symbols
     ;                     acum-declaration acum-inits init loop-code final 
     ;                     (reverse *let-list*) 
     ;                     body))				  
      (eval `(screamer::defun ,(intern (string (first (code self))) :om)  (,.symbols)  ;;; OM 4 ;;; TEST	
               (let (,.acum-declaration) ,.acum-inits
                    (loop ,.loop-code 
                          finally (return (values ,.(loop for i from 0 to (- (length (inputs final-box)) 1)
                                                          collect  (gen-code final-box i)))) do
                          (let* ,*let-list*
                            ,body)))))					  						  
     (setf *lambda-context* oldlambdacontext)
     (setf *let-list* oldletlist)))
	 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	 
;;; OM 4

;screamer 
(defmethod compile-patch ((self patchForLoop))
   "Code generates by Loop patches is generate by this method."
   (let* ((boxes (boxes self))
          (oldletlist *let-list*)
          (*let-list* nil)
          (do-box (car (find-class-boxes boxes 'OMLoopDo)))
          (final-box (car (find-class-boxes boxes 'OMFinalDo)))
          (in-boxes (sort (find-class-boxes boxes 'OMin) '< :key 'indice))
          (symbols (mapcar #'(lambda (thein) (setf (in-symbol thein) (gensym))) in-boxes))
          (loop-boxes (find-loop-boxes boxes))
          (loop-code (mapcan #'(lambda (theout)
                                 (loop-gen-code theout 0)) loop-boxes))
          (acum-boxes (find-acum-boxes boxes))
          (acum-declaration (mapcar #'(lambda (acumm)
                                        (declare-closure acumm)) acum-boxes))
          (acum-inits (mapcar #'(lambda (acumm)
                                  (gen-code acumm -1)) acum-boxes))
          body)
     (setf body (gen-code do-box 0))
     (eval `(screamer::defun ,(intern (string (first (code self))) :om)  (,.symbols) 
              (let (,.acum-declaration) ,.acum-inits
                   (loop ,.loop-code 
                         finally (return (values ,.(loop for i from 0 to (- (length (inputs final-box)) 1)
                                                         collect  (gen-code final-box i)))) do
                         (let* ,*let-list*
                           ,body)))))
     (setf *let-list* oldletlist)))

;;screamer
(defmethod curry-lambda-code ((self OMLoop-Box) symbol)
  "Lisp code generation for a loop box in lambda mode."
  (let* ((nesymbs nil)
         (args  (mapcar #'(lambda (input)
                            (if (connected? input)
				(gen-code input 0)
                              (let ((newsymbol (gensym)))
                                (push newsymbol nesymbs)
                                newsymbol))) (inputs self))))
    (if (null nesymbs)
	symbol
      `#'(lambda ,(reverse nesymbs)
	   (apply (fdefinition ',(intern (string (first (code (patch self)))) :om)) (list ,.args))))))

;;screamer
(defmethod draw-after-box ((self loopBoxFrame))
   (with-fore-color 16719095
     (when (non-deter-patch? (patch (object self)))
       (#_Textsize 28)
       (draw-char (- (round (w self) 2) 8) (+ (round (h self) 2) 6) #\? ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


