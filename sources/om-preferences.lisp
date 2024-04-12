;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; REVISED VERSION
;;; PAULO HENRIQUE RAPOSO AND KARIM HADDAD

(in-package :om)

;=================================================
; OM-BACKTRACK PREFERENCES MODULE
;=================================================

(defvar *screamer-valuation* 0)

(defmethod put-preferences ((iconID (eql :backtrack)))
   (let ((modulepref (find-pref-module iconID)))
   (setf *screamer-valuation* (get-pref modulepref :scream-val))
   ))

(defmethod get-def-vals ((ID (eql :backtrack)))
    (list 
          :scream-val *screamer-valuation*
		  ))
(defmethod save-pref-module ((iconID (eql :backtrack)) item)
   (list iconID `(list 
                  :scream-val , *screamer-valuation*  
                       ) *om-version*))

(defmethod make-new-pref-scroll ((num (eql :backtrack)) modulepref)
  (let ((thescroll (om-make-view 'preference-pane
                                 :pref-id num
                                 :name "Backtrack" 
                                 :size (get-pref-scroll-size)
                                 :position (om-make-point 66 0)
                                 :font *controls-font* 
                                 :bg-color *om-light-gray-color*
                                  ))
                                       
        ;(l1 20) (l2 (round (om-point-h (get-pref-scroll-size)) 2)) (i 0)
        ;(dy #-linux 30 #+linux 33) sf2txt
		pos)
    
	    (om-add-subviews thescroll
	                     (om-make-dialog-item 'om-static-text (om-make-point 20 (setf pos 15)) (om-make-point 200 30) "Screamer Valuation"
	                                          :font *om-default-font3b*)
	                     (om-make-dialog-item 'om-radio-button (om-make-point 20 pos) (om-make-point 100 80) "One Value" 
	                                          :di-action (om-dialog-item-act item
	                                                       (declare (ignore item))
	                                                       (set-pref modulepref :scream-val 0))
	                                          :checked-p (= (get-pref modulepref :scream-val) 0)  
	                                          :radio-button-cluster 'scream-val
	                                          :font *om-default-font2*)                    
	                     (om-make-dialog-item 'om-radio-button (om-make-point 120 pos) (om-make-point 100 80) "All Values" 
	                                          :di-action (om-dialog-item-act item
	                                                       (declare (ignore item))
	                                                       (set-pref modulepref :scream-val 1))
	                                          :checked-p (= (get-pref modulepref :scream-val) 1)  
	                                          :radio-button-cluster 'scream-val
	                                          :font *om-default-font2*)                    
	                     (om-make-dialog-item 'om-radio-button (om-make-point 220 pos) (om-make-point 100 80) "Listener" 
	                                          :di-action (om-dialog-item-act item
	                                                       (declare (ignore item))
	                                                       (set-pref modulepref :scream-val 2))
	                                          :checked-p (= (get-pref modulepref :scream-val) 2)  
	                                          :radio-button-cluster 'scream-val
	                                          :font *om-default-font2*)
					  														    								  
		 )
	    thescroll))
		
(defun add-backtrack-preferences ()
  (push-pref-module (list :backtrack (get-def-vals :backtrack))))

(progn
(add-backtrack-preferences)
(add-init-user-func 'add-backtrack-preferences))


