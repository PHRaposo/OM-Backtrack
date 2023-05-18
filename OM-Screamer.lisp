;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OM-SCREAMER
;;; 
;;; 

(in-package :om)


;--------------------------------------------------
;Variable definiton with files to load 
;--------------------------------------------------

(defvar *screamer-files* nil)
(setf  *screamer-files* (list 
                         (om::om-relative-path '("sources") "om-preferences")
			 (om::om-relative-path '("sources") "screamboxes")
                         (om::om-relative-path '("sources") "screamfuns")
                         (om::om-relative-path '("sources") "screaminterface")
                         ;(om::om-relative-path '("sources" "constraints") "constraint")
                         ;(om::om-relative-path '("sources" "constraints") "constraint-boxes")
                         ;(om::om-relative-path '("sources") "screamer-constraint-solver")	  
                         ;(om::om-relative-path '("sources" "closer-mop") "closer-mop-packages")
                         ;(om::om-relative-path '("sources" "closer-mop") "closer-mop-shared")
                         ;(om::om-relative-path '("sources" "closer-mop") "closer-mop-lispworks")
                         ;(om::om-relative-path '("sources" "screamer-plus") "screamer-plus")
                         ))



;--------------------------------------------------
;Loading files 
;--------------------------------------------------
(mapc #'compile&load *screamer-files*)

;--------------------------------------------------
;Fill library 
;--------------------------------------------------


(fill-library '( ("Backtrack" Nil Nil (an-integer-between
                                       a-member-of
                                       apply-cont
                                       list-of-members-of 
                                       list-of-integers-between 
                                       a-chord-in
                                       list-of-chords-in
                                       alldiff?
                                       growing? ) Nil)
                
	           ;("Solver" Nil Nil (screamer-solver) Nil)
	           ;("Variables" Nil Nil (an-integer-betweenv a-member-ofv an-integerv) Nil)			   
			;       ("Predicates" Nil Nil (>v ) Nil) ;+v -v *v /v <v =v /=v etc...) Nil)
			 ;      ("Ordering" Nil Nil (static-ordering reorder) Nil)
			  ;     ("Force-function" Nil Nil (linear-force divide-and-conquer-force) Nil) 

                ;("FOLDER2" Nil Nil (package::FUNCTION) Nil)

                 ))
(print 
"
OM-SCREAMER Library

")



