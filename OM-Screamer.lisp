;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OM-SCREAMER LIBRARY
;;; Includes:
;;; * OM-BACKTRACK based on original version for OM 4 by Gerard Assayag and Augusto Agon
;;;   Copyright (C) 1997-2003 by IRCAM-Centre Georges Pompidou, Paris, France.
;;;   Adapted to OM 7.2 by Paulo Raposo and Karim Haddad
;;; 
;;; * SCREAMER-CONSTRAINT-SOLVER
;;;
;;; * SCREAMER by Jeffrey Mark Siskind and David Allen McAllester
;;;   Copyright 1991 Massachusetts Institute of Technology. All rights reserved.
;;;   Copyright 1992, 1993 University of Pennsylvania. All rights reserved.
;;;   Copyright 1993 University of Toronto. All rights reserved.
;;;
;;;* SCREAMER-PLUS by Simon White
;;;  Copyright 1998-2000 University of Aberdeen
;;;
;;;* Dependencies of SCREAMER-PLUS:
;;;* CLOSER-MOP by Pascal Costanza
;;;  Copyright (c) 2005 - 2016 Pascal Costanza
;;;

(in-package :om)


;--------------------------------------------------
;Variable definiton with files to load 
;--------------------------------------------------

(defvar *screamer-files* nil)
(setf  *screamer-files* (list 
                         (om::om-relative-path '("sources") "om-preferences")
                         (om::om-relative-path '("sources" "closer-mop") "closer-mop-packages")
                         (om::om-relative-path '("sources" "closer-mop") "closer-mop-shared")
                         (om::om-relative-path '("sources" "closer-mop") "closer-lispworks")
                         (om::om-relative-path '("sources" "screamer-plus") "screamer-plus")			 
			 (om::om-relative-path '("sources") "screamboxes")
                         (om::om-relative-path '("sources") "screamfuns")			 
                         (om::om-relative-path '("sources") "screaminterface")
			 (om::om-relative-path '("sources") "non-deter-patch")
			 (om::om-relative-path '("sources") "package")
			 (om::om-relative-path '("sources") "om-screamerfuns")						 	 
			 (om::om-relative-path '("sources") "screamer-solver")			 
                         ))



;--------------------------------------------------
;Loading files 
;--------------------------------------------------
(mapc #'compile&load *screamer-files*)

;--------------------------------------------------
;Fill library 
;--------------------------------------------------


(fill-library '( ("Backtrack" Nil Nil (an-integer-between a-member-of apply-cont list-of-members-of list-of-integers-between 
                                       a-chord-in list-of-chords-in alldiff? growing? ) Nil)
 		("SCS"
 	          (("solver" nil nil (screamer-solver force-function) nil)
		   ("om-methods" nil nil (om?::om+v om?::om-v om?::om*v om?::om/v om?::mc->pcv om?::mod12v om?::om-absv) nil)				   ("variables" nil nil (list-of-lists-ofv list-of-chords-inv om?::a-list-member-of-listsv om?::a-random-member-ofv om?::list-of-random-members-ofv om?::list-of-members-ofv om?::list-of-integers-betweenv om?::list-of-integers-abovev om?::list-of-integers-belowv 
	   	                         om?::list-of-integersv om?::list-of-realsv om?::list-of-reals-betweenv om?::list-of-reals-abovev om?::list-of-reals-belowv 
					 om?::list-of-numbersv om?::list-of-booleansv ) nil)
  		   ("constraints" nil nil (om?::apply-contv om?::assert!-all-differentv om?::pc-setpv? om?::sub-setpv? om?::all-membersv) nil)					 
		   ("utils" nil nil (om?::x->dxv om?::dx->xv om?::smat-trans om?::all-rotations om?::mc->pcv om?::modv om?::assert!-apply-rec om?::apply-rec) nil)
 					 ) Nil Nil Nil)
									   
	   ("Screamer"
		  (("primitives" nil nil (s::an-integer-between s::a-member-of s::fail) nil)
		    ("variables" nil nil (s::a-member-ofv s::an-integerv s::an-integer-abovev s::an-integer-belowv s::an-integer-betweenv							  s::a-realv s::a-real-abovev s::a-real-belowv s::a-real-betweenv s::a-numberv s::a-booleanv s::make-variable) nil)
		    ("assert!" nil nil (s::assert! s::assert!-integerpv s::assert!-notv-integerpv s::assert!-realpv s::assert!-notv-realpv
					s::assert!-numberpv s::assert!-notv-numberpv s::assert!-booleanpv s::assert!-notv-booleanpv
					s::assert!-=v2 s::assert!-<=v2 s::assert!-<v2 s::assert!-/=v2 s::assert!-memberv s::assert!-notv-memberv
					s::assert!-equalv s::assert!-notv-equalv s::assert!-=v s::assert!-<v s::assert!-<=v s::assert!->v
					s::assert!->=v s::assert!-/=v) nil)
		  ("type-restrictions" nil nil (s::numberpv s::realpv s::integerpv s::booleanpv s::memberv) nil)
		   ("boolean" nil nil (s::andv s::orv s::notv) nil)
		   ("numeric" nil nil (s::<v s::<=v s::>v s::>=v s::=v s::/=v s::+v s::-v s::*v s::/v s::minv s::maxv s::=v2 s::<=v2 s::<v2 s::/=v2 ) nil)
		   ("expression" nil nil (s::equalv) nil)
		   ("functions" nil nil (s::funcallv s::applyv) nil) ) Nil Nil Nil)
					
	  ("Screamer-Plus"
	    (("variables" nil nil (screamer+::a-listv screamer+::a-consv screamer+::a-symbolv screamer+::a-stringv 
   	                           screamer+::a-typed-varv) nil)
	     ("type-restrictions" nil nil (screamer+::listpv screamer+::conspv screamer+::symbolpv screamer+::stringpv screamer+::typepv) nil)
	     ("boolean" nil nil (screamer+::impliesv) nil)
	     ("expression" nil nil (screamer+::ifv screamer+::make-equal) nil)
    	     ("lists" nil nil (screamer+::carv screamer+::cdrv screamer+::consv screamer+::firstv screamer+::secondv screamer+::thirdv
			       screamer+::fourthv screamer+::nthv screamer+::subseqv screamer+::lengthv screamer+::appendv screamer+::make-listv screamer+::all-differentv) nil)
	     ("sets-and-bags" nil nil (screamer+::set-equalv screamer+::subsetpv screamer+::intersectionv screamer+::unionv screamer+::bag-equalv) nil)							
	     ("arrays" nil nil (screamer+::make-arrayv screamer+::arefv) nil) 
	     ;("objects" nil nil (screamer+::make-instancev screamer+::classpv screamer+::slot-valuev screamer+::class-ofv screamer+::class-namev
	     ;		 screamer+::slot-exists-pv screamer+::reconcile) nil)
	     ("high-order-fns" nil nil (screamer+::funcallinv screamer+::mapcarv screamer+::maplistv screamer+::everyv screamer+::somev
					screamer+::noteveryv screamer+::notanyv screamer+::at-leastv screamer+::at-mostv screamer+::exactlyv screamer+::constraint-fn) nil)
	     ;("stream-output" nil nil (screamer+::formatv) nil)		
	     ("functions" nil nil (s::funcallgv) nil)
	 					) Nil Nil Nil)
		
 
                ;("FOLDER2" Nil Nil (package::FUNCTION) Nil)

                 ))
(print"
OM-SCREAMER LIBRARY
Includes:
* OM-BACKTRACK based on original version for OM 4 by Gerard Assayag and Augusto Agon
  Copyright (C) 1997-2003 by IRCAM-Centre Georges Pompidou, Paris, France.
  Adapted to OM 7.2 by Paulo Raposo and Karim Haddad

* SCREAMER-CONSTRAINT-SOLVER

* SCREAMER by Jeffrey Mark Siskind and David Allen McAllester
  Copyright 1991 Massachusetts Institute of Technology. All rights reserved.
  Copyright 1992, 1993 University of Pennsylvania. All rights reserved.
  Copyright 1993 University of Toronto. All rights reserved.

* SCREAMER-PLUS by Simon White
  Copyright 1998-2000 University of Aberdeen
  
* Dependencies of SCREAMER-PLUS:
* CLOSER-MOP by Pascal Costanza 
  Copyright (c) 2005 - 2016 Pascal Costanza
  ")
(print (concatenate 'string "Current SCREAMER Version: " s::*screamer-version*))
(print (concatenate 'string "Current SCREAMER-PLUS Version: " ?::*screamer+-version*))



