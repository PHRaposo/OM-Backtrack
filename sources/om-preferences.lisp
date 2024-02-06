(in-package :om)

;;;================================;;;
;;;OM-SCREAMER-PRESENTATION-WINDOW
;;;================================;;;

(defun show-screamer-about-win ()
  (show-screamer-kero-pict t))

(defvar *screamer-graph-pres* nil)
(defvar *screamer-graph-path* (make-pathname :directory (append (pathname-directory *load-pathname*) (list "resources/pict"))))
(setf *screamer-graph-pres* (om::om-load-and-store-picture "screamer" "om-screamer" *screamer-graph-path*)) 

(defvar *om-screamer-version-string* nil) 
(setf *om-screamer-version-string* "1.0")

(defparameter *screamer-splash-screen* nil)

(defparameter *screamer-credits* 
"OM-SCREAMER LIBRARY includes:

* OM-BACKTRACK based on original version for OM 4
   by Gerard Assayag and Augusto Agon
   Copyright (C) 1997-2003 by IRCAM-Centre Georges Pompidou, Paris, France.
	   
Adapted to OM 7.2 by Paulo Raposo and Karim Haddad

* SCREAMER-CONSTRAINT-SOLVER and SCREAMER-SCORE by Paulo Raposo

* PC-SET-THEORY from PW-CONSTRAINTS
  by Mikael Laurson (1995) - Ported to OpenMusic by Orjan Sandred (1999) 
  Adapted to OM-Screamer by Paulo Raposo  
	   
  LISP LIBRARIES:
 
* SCREAMER 4.0.0
  Based on original version 3.20 by Jeffrey Mark Siskind and David Allen McAllester
  Copyright 1991 Massachusetts Institute of Technology. All rights reserved.
  Copyright 1992, 1993 University of Pennsylvania. All rights reserved.
  Copyright 1993 University of Toronto. All rights reserved.
    
  Maintaner: Nikodemus Siivola <https://github.com/nikodemus/screamer>
  
* SCREAMER-PLUS 0.1 by Simon White
  Copyright 1998-2000 University of Aberdeen 
  
* CLOSER-MOP by Pascal Costanza 
  Copyright (c) 2005 - 2016 Pascal Costanza

* Code excerpts from t2l-screamer by Killian Sprotte")

(defclass about-screamer-window (om-window) ())

(defmethod om-window-close-event ((self about-screamer-window))
  (setf *screamer-splash-screen* nil)
  (call-next-method))

(defun make-screamer-splash-view (&optional (credits nil))
  (let* ((name (string+ "OM-SCREAMER " *om-screamer-version-string*))
         (textcolor *om-white-color*)
         (mainfont  (om-make-font "Verdana" (nth 0 *om-def-font-sizes*)))
         ;(boldfont  (om-make-font "Verdana" (nth 0 *om-def-font-sizes*) :style '(:bold)))
         (view (om-make-view 'splash-screen
                             :thepict *screamer-graph-pres*
                             :size (om-add-points 
                                    (or (om-get-picture-size *screamer-graph-pres*) (om-make-point 20 20)) (om-make-point 0 0))
                                    ;(if credits (om-make-point 320 #-linux 0 #+linux 30) (om-make-point 0 0)))
                             :subviews (list (om-make-dialog-item 'om-static-text  
                                                                  (om-make-point 140 6) (om-make-point 400 36)
                                                                  name
                                                                  :font (om-make-font "Arial" (nth 4 *om-def-font-sizes*) :style '(:bold))
                                                                  :fg-color textcolor
                                                                  )
																 
                                             (om-make-dialog-item 'om-static-text  
                                                                  (om-make-point 130 47) (om-make-point 300 18)
                                                                  (string+ "Development: Paulo Raposo and K. Haddad")
                                                                  :font mainfont
                                                                  :fg-color textcolor
                                                                  )
                                             )
                             )))
    (when credits 
      (om-add-subviews view
                       (om-make-dialog-item 'om-static-text  
                                            (om-make-point 20 127)
                                            (om-make-point 1000 600)
                                            *screamer-credits*
                                            :font mainfont
                                            :fg-color textcolor
											)
                       ))
    view))

(defun show-screamer-kero-pict (&optional (credits nil))
  (if *screamer-splash-screen* (om-select-window *screamer-splash-screen*)
    (let* ((view (make-screamer-splash-view credits))
           (win (om-make-window 'about-screamer-window 
                                :window-title "About OM-SCREAMER"
                                :close t
                                :minimize nil
                                :maximize nil
                                :resizable nil
                                :position :centered 
                                :size (om-view-size view))))
	(om-add-subviews win view)
	(setf *screamer-splash-screen* win)
	(om-show-window win)
        win
        )))

; (show-screamer-kero-pict *screamer-credits*)

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
                         (om-make-dialog-item 'om-button (om-make-point 10 100) (om-make-point 200 30) 
                                              "About OM-Screamer" :font *om-default-font1*
                                              :di-action #'(lambda (item) (declare (ignore item)) (show-screamer-about-win)))					  														    								  
		 )
	    thescroll))
		
(defun add-backtrack-preferences ()
  (push-pref-module (list :backtrack (get-def-vals :backtrack))))

(progn
(add-backtrack-preferences)
(add-init-user-func 'add-backtrack-preferences))


