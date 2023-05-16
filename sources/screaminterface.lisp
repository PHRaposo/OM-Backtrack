(in-package :om)

(defvar *screamer-listener-size* (om-make-point 400 200))

(defclass non-deter-window (EditorWindow)
  ((value-item :initform nil :accessor value-item)))

(defmethod om-set-view-size ((self non-deter-window) size) 
   (declare (ignore size))
   (call-next-method) 
   (setf *screamer-listener-size* (om-view-size self))
   (when (value-item self)
     (om-set-view-size (value-item self) (om-make-point (- (w self) 25) (- (h self) 50)))))
	 

(defun non-determinise-listener (value)
  (let (dialog value-item-view)
    (setf value-item-view
          (cond 
           ((omclass-p (class-of (class-of value)))
            (setf dialog (om-make-window 'non-deter-window 
                                         :window-title "Non deterministc listener"
                                         :position :centered 
                                        ; :window-show t ;nil
                                         :size *screamer-listener-size*
                           ;:font (om-make-font "Arial" 12 :mode :srcor :style :plain)
                                         :bg-color (om-make-color 0.875 0.875 0.875)))
            (cond
             ((Class-has-editor-p value)
              (setf (value-item dialog) 
                    (setf (editor dialog) (om-make-view (get-editor-class value)
                                                        :ref nil
                                                        :container dialog
                                                        :object value
                                                        :position (om-make-point 25 35) 
                                                        :size (om-make-point (- (w dialog) 25) (- (h dialog) 50))))))
             
             (t (setf (value-item dialog) 
                      (setf (editor dialog) 
                            (let* ((instance (omNG-make-new-instance value "instance"))
                                   (editor (om-make-view 'InstanceEditor
                                                         :ref nil
                                                         :container dialog
                                                         :object instance
                                                         :position (om-make-point 25 35) 
                                                         :size (om-make-point (- (w dialog) 25) (- (h dialog) 50))))
                                   (slot-boxes (make-slots-ins-boxes instance)))
                              (setf (presentation editor) 0)
                              (mapc #'(lambda (frame)
                                        (omG-add-element (panel editor) frame)) slot-boxes)
                              editor))))))
           (t (setf dialog (om-make-window 'om-window 
                                           :window-title "Non deterministc listener"
                                           :position :centered 
                                          ; :window-show t ;nil
                                           :size *screamer-listener-size*
                            ; :font (om-make-font "Arial" 12 :mode :srcor :style :plain)
                                           :bg-color (om-make-color 0.875 0.875 0.875)))
              (om-make-view 'om-text-edit-view
                            :save-buffer-p t
                            :scrollbars :v
                            :text (format nil "~D" value)
                            :wrap-p t
                            :size (om-make-point (- (om-point-h (om-interior-size dialog)) 40) (- (om-point-v (om-interior-size dialog)) 50))
                            :position (om-make-point 25 35)))))
    (om-add-subviews dialog value-item-view  
                     (om-make-dialog-item 'om-static-text (om-make-point 10 5) (om-make-point 200 16) "Do you want another solution? "
                                          :bg-color (om-make-color 0.624 0.624 0.624))
                     (om-make-dialog-item 'om-button (om-make-point 220 5) (om-make-point 62 20) "No" 
                                          :di-action (om-dialog-item-act item
                                                       (om-return-from-modal-dialog dialog nil)))
                     (om-make-dialog-item 'om-button (om-make-point 290 5) (om-make-point 58 18) "Yes" 
                                          :di-action (om-dialog-item-act item
                                                       (om-return-from-modal-dialog dialog t))
                                          :default-button t))
    dialog
    ))

;;;(non-determinise-listener 0)


;;;============================================

(in-package :s)
(defmacro-compile-time print-values (&body forms)
  `(catch 'succeed
     (for-effects
       (let ((value (progn ,@forms)))
         
         (if (= om::*screamer-valuation* 2)
           (unless (om::non-determinise-listener value)
             (throw 'succeed value))
           (progn (throw 'succeed value) (print value)))))))		   
(in-package :om)


