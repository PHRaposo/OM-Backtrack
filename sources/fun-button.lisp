(in-package :om)

(defun get-icon-lock (str)
  (cond
   ((string= str "x") 167)
   ((string= str "&") 168)
   ((string= str "l") 145)
   ((string= str "o") 166)
   ((string= str "#") 170)   
   ))

(defun get-str-lock (num)
  (case num
    (167 "x") (168 "&")  (145 "l") (166 "o") (170 "#")))


(defmethod allowed-lock-modes ((self omboxcall)) '("x" "&" "l" "o" "#"))

(defmethod add-rem-fun-button ((self omboxframe))
   (cond ((and (lock-button self) (string-equal (allow-lock (object self)) "#")
               (mode-allowed-p (object self) "#"))
          (remove-lock-button self))
         ((and (lock-button self) (mode-allowed-p (object self) "#"))
          (remove-lock-button self)
          (add-lock-button self "#"))
         (t (add-lock-button self "#"))))
		 
(defmethod handle-key-event ((self patchPanel) char) 
  (modify-patch self)
  (let* ((actives (get-actives self))
         (activeboxes (mapcar 'object actives)))
    
    (when (and (char-num-p char) actives (not (equal char #\0)))
      (let ((boxes
             (remove-if-not #'(lambda (item) (or (boxframe-p item) (boxeditorframe-p item))) actives)))
        (loop for i in boxes
              do (let ((input (nth (1- (digit-char-p char)) (inputframes i))))
                   (when input
                     (if (connected? (object input))
                         (unless (member *target-out* (outframes i))
                           (disconnect-box i input))
                       (connect-box *target-out* input))))) 
        ))
    
    (case char
      (:om-key-delete (delete-general self))
      (#\f (make-undefined-funct-box self (om-mouse-position self)))
      (#\c (if actives (edit-comment-box actives)
             (make-comment-box self (om-mouse-position self))))
      (#\x (create-comment-box self (om-mouse-position self)))
      (#\d  (mapc 'show-big-doc actives))
      (#\D (mapc 'update-doc actives))
      (#\C  (patch-color self))
      (#\F (font-comments self))
      (#\e (mapc 'show-fun-code actives))
      (#\g (getdacode self))
      (#\v  (om-eval-enqueue 
             `(progn
                (setf *cur-eval-panel* ,self)
                (mapc 'eval-box ',actives)
                (clear-ev-once ,self))
             ))
      (#\h  (show-help-window (format nil "Commands for ~A Editor" 
                                      (string-upcase (class-name (class-of (object (editor self))))))
                              (get-help-list (editor self)) 410)) 
      (#\H (mapc 'open-helpfile actives))
      ;;; in the menu
      (#\k (mapc 'add-keywords actives))
      (#\K (mapc 'erase-keywords actives))
      (#\I (mapc 'reinit-size actives) 
           (reinit-connections self)
           (reinit-bg-picts self))
      (#\R (mapc 'reinit-contents actives))
      
      (#\b (mapc 'add-rem-lock-button actives))
      (#\l (mapc 'add-rem-lambda-button actives))
      (#\o (mapc 'add-rem-evonce-button actives))
      (#\i (mapc 'add-rem-itself-button actives))
      (#\# (mapc 'add-rem-fun-button actives))
      (#\a (mapc 'internalize-patch actives))
      (#\A (mapc 'align-one-boxframe actives)
           (make-move-after self actives))

      (#\p (play-boxes activeboxes) (mapcar 'om-invalidate-view actives))
      (#\s (stop-boxes activeboxes) (mapcar 'om-invalidate-view actives))      
      (#\Space (if (idle-p *general-player*)
                   (play-boxes activeboxes)
                 (stop-all-boxes))
               (mapcar 'om-invalidate-view actives))
      
      (#\t (mapc 'show-online-tutorial actives))
      
      (#\m (mapc 'change-edit-mode actives))
      (#\n (mapc 'set-show-box-name actives))	  
      (#\M (change-edit-mode-all (get-subframes self)))
      (#\z (if *curved-connections*
               (setf *curved-connections* nil)
             (setf *curved-connections* t)))
      
      (:om-key-up 
       (if (and (om-command-key-p) (om-shift-key-p))
           (mapc 'box-resize-y-minus actives)
         (progn
           (mapc #'(lambda (item) (move-frame-delta item 0)) actives)
           (make-move-after self actives))))
      (:om-key-down 
       (if (and (om-command-key-p) (om-shift-key-p))
           (mapc 'box-resize-y-plus actives)
         (progn
           (mapc #'(lambda (item) (move-frame-delta item 1)) actives)
           (make-move-after self actives))))
      (:om-key-left
       (cond 
        ((om-option-key-p)
         (mapc #'(lambda (item) (delete-all-inputs item) t) actives))
        ((and (om-command-key-p) (om-shift-key-p))
         (mapc 'box-resize-x-minus actives))
        (t
         (progn
           (mapc #'(lambda (item) (move-frame-delta item 3)) actives)
           (make-move-after self actives)
           ))
        ))
      (:om-key-right 
       (cond 
        ((om-option-key-p)
         (mapc #'(lambda (item) (add-all-inputs item)) actives))
        ((and (om-command-key-p) (om-shift-key-p))
         (mapc 'box-resize-x-plus actives))
        (t
         (progn (mapc #'(lambda (item) (move-frame-delta item 2)) actives)
           (make-move-after self actives)))))

      (#\< (mapc #'(lambda (item) (delete-one-input item)) actives))
      (#\> (mapc #'(lambda (item) (add-one-input item)) actives))
      
      (#\r #+om-reactive(mapc #'(lambda (boxframe) 
                                  (set-active (object boxframe) (not (active (object boxframe))))
                                  (om-invalidate-view boxframe))
                              actives))
      
      (#\E (om-encapsulate self actives))
      (#\U (om-unencapsulate self actives))
      
      (otherwise (loop
		    with hotbox = nil
		    for box in activeboxes
                           
                    do (when (find-method #'handle-key-event '() (list (class-of box)
                                                                       (find-class t)) nil)
                         (setf hotbox t)  
                         (handle-key-event box char))
                    finally
                    ;do 
                      (unless hotbox 
                        (om-beep)			    ;no boxes have specialized handle-key-event methods
                        ))))))
