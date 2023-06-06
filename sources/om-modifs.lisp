(in-package :om)

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


