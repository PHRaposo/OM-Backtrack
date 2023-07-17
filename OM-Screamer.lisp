;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OM-SCREAMER LIBRARY
;;; Includes:
;;; * OM-BACKTRACK based on original version for OM 4 by Gerard Assayag and Augusto Agon
;;;   Copyright (C) 1997-2003 by IRCAM-Centre Georges Pompidou, Paris, France.
;;;   Adapted to OM 7.2 by Paulo Raposo and Karim Haddad
;;; 
;;; * SCREAMER-CONSTRAINT-SOLVER by Paulo Raposo
;;;
;;; * PC-SET-THEORY from PW-CONSTRAINTS and OMCS 
;;;   by Mikael Laurson (1995) - Ported to OpenMusic by Orjan Sandred (1999) 
;;;   Adapted to OM-Screamer by Paulo Raposo 
;;;
;;;   LISP LIBRARIES:
;;;   
;;; * SCREAMER 4.0.0
;;;   Based on original version 3.20 by:
;;;
;;;   Jeffrey Mark Siskind (Department of Computer Science, University of Toronto)
;;;   David Allen McAllester (MIT Artificial Intelligence Laboratory)
;;;
;;;   Copyright 1991 Massachusetts Institute of Technology. All rights reserved.
;;;   Copyright 1992, 1993 University of Pennsylvania. All rights reserved.
;;;   Copyright 1993 University of Toronto. All rights reserved.
;;;
;;;   Maintaner: Nikodemus Siivola <https://github.com/nikodemus/screamer>
;;;
;;; * SCREAMER-PLUS 0.1 by Simon White
;;;  Copyright 1998-2000 University of Aberdeen
;;;
;;; * CLOSER-MOP by Pascal Costanza
;;;   Copyright (c) 2005 - 2016 Pascal Costanza
;;;
;;; * ITERATE by Jonathan Amsterdam
;;;	  Adapted to ANSI Common Lisp in 2003 by Andreas Fuchs
;;;	  Copyright 1989, Jonathan Amsterdam.
;;;
;;; * Code excerpts from t2l-screamer by Killian Sprotte
;;;
;;; * Retract (extension to Screamer) by Buddy Kresge 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
(in-package :om)


;--------------------------------------------------
;Variable definiton with files to load 
;--------------------------------------------------

(defvar *screamer-files* nil)
(setf  *screamer-files* (list	
                         (om::om-relative-path '("sources" "screamer 4.0.0") "package")
                         (om::om-relative-path '("sources" "screamer 4.0.0") "screamer")
                         (om::om-relative-path '("sources" "iterate") "package")	
                         (om::om-relative-path '("sources" "iterate") "iterate")
                         (om::om-relative-path '("sources" "retract") "retract")
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
                         (om::om-relative-path '("sources" "pc-set-theory") "SCs-data")
                         (om::om-relative-path '("sources" "pc-set-theory") "all-SCs")						 						 
                         (om::om-relative-path '("sources" "pc-set-theory") "pc-set-theory")						 						  
                         ))
						 
;--------------------------------------------------
;Loading files 
;--------------------------------------------------
(mapc #'compile&load *screamer-files*)

;--------------------------------------------------
;Fill library 
;--------------------------------------------------


(fill-library '( ("Backtrack" Nil Nil (an-integer-between a-member-of apply-cont list-of-members-of list-of-integers-between a-chord-in 
	                                   list-of-chords-in alldiff? growing?) Nil)
 		       ("SCS"
 		          (("solver" nil nil (screamer-solver force-function screamer-doc) nil)
				   ("om-methods" nil nil (om?::om+v om?::om-v om?::om*v om?::om/v om?::mc->pcv om?::mod12v om?::om-absv) nil)
				   ("variables" nil nil (screamer-variable list-ofvs list-of-lists-ofv list-of-chords-inv) nil)
				   ("functions" nil nil (om?::apply-contv om?::assert!-apply-rec om?::apply-rec om?::funcallv-rec om?::funcallv-rec-car-cdr) nil)
				   ("pc-set-theory" 
					(("constraints" nil nil (om?::pc-setpv? om?::sub-setpv? om?::member-of-scv?) nil)
					 ("SCs" nil nil (om?::SC-name om?::SC+off om?::SCs-card om?::SC-info om?::sub/supersets om?::SC-subsets) nil))
					  nil nil nil)   				 
  				   ("constraints" 					   
					    (("melody" nil nil (om?::all-ascendingv om?::all-descendingv om?::alowed-melodic-intervals om?::nor-allowed-melodic-intervals
							               om?::no-repeated-melodic-intervals om?::ballistic?) nil)
					    ("general" nil nil (om?::assert!-all-differentv om?::all-membersv) nil)) nil nil nil)					 
				   ("utils" nil nil (om?::x->dxv om?::dx->xv om?::smat-trans om?::all-rotations om?::mc->pcv om?::modv) nil)
 					 ) Nil Nil Nil)
									   
		       ("Screamer"
		           (("primitives" nil nil (s::an-integer-between s::a-member-of s::fail) nil)
				    ("variables" nil nil (s::a-member-ofv s::an-integerv s::an-integer-abovev s::an-integer-belowv s::an-integer-betweenv
										  s::a-realv s::a-real-abovev s::a-real-belowv s::a-real-betweenv s::a-numberv s::a-booleanv s::make-variable) nil)
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
	 		           (("primitives" nil nil (screamer+::a-subset-of screamer+::a-partition-of screamer+::members-ofv) nil)
					    ("variables" nil nil (screamer+::a-listv screamer+::a-consv screamer+::a-symbolv screamer+::a-stringv 
						                      screamer+::a-typed-varv) nil)
	 				    ("type-restrictions" nil nil (screamer+::listpv screamer+::conspv screamer+::symbolpv screamer+::stringpv screamer+::typepv) nil)
	 		            ("boolean" nil nil (screamer+::impliesv) nil)
	 		            ("expression" nil nil (screamer+::ifv screamer+::make-equal) nil)
						("lists" nil nil (screamer+::carv screamer+::cdrv screamer+::consv screamer+::firstv screamer+::secondv screamer+::thirdv
							screamer+::fourthv screamer+::nthv screamer+::subseqv screamer+::lengthv screamer+::appendv screamer+::make-listv screamer+::all-differentv) nil)
	 		            ("sets-and-bags" nil nil (screamer+::set-equalv screamer+::subsetpv screamer+::intersectionv screamer+::unionv screamer+::bag-equalv) nil)							
	 		            ("arrays" nil nil (screamer+::make-arrayv screamer+::arefv) nil)
						;("objects" nil nil (screamer+::make-instancev screamer+::classpv screamer+::slot-valuev screamer+::class-ofv screamer+::class-namev
							;screamer+::slot-exists-pv screamer+::reconcile) nil)
						("high-order-fns" nil nil (screamer+::funcallinv screamer+::mapcarv screamer+::maplistv screamer+::everyv screamer+::somev
							screamer+::noteveryv screamer+::notanyv screamer+::at-leastv screamer+::at-mostv screamer+::exactlyv screamer+::constraint-fn) nil)
						;("stream-output" nil nil (screamer+::formatv) nil)		
						("functions" nil nil (s::funcallgv) nil)
	 					) Nil Nil Nil)

                ;("FOLDER" Nil Nil (package::FUNCTION) Nil)

                 ))
				 
				
(print (format nil "
OM-SCREAMER LIBRARY
Includes:

* OM-BACKTRACK based on original version for OM 4
   by Gerard Assayag and Augusto Agon
   Copyright (C) 1997-2003 by IRCAM-Centre Georges Pompidou, Paris, France.
	   
Adapted to OM 7.2 by Paulo Raposo and Karim Haddad

* SCREAMER-CONSTRAINT-SOLVER by Paulo Raposo

* PC-SET-THEORY from PW-CONSTRAINTS and OMCS 
  by Mikael Laurson (1995) - Ported to OpenMusic by Orjan Sandred (1999) 
  Adapted to OM-Screamer by Paulo Raposo
	   
  LISP LIBRARIES:
 
* SCREAMER ~A
  Based on original version 3.20 by Jeffrey Mark Siskind and David Allen McAllester
  Copyright 1991 Massachusetts Institute of Technology. All rights reserved.
  Copyright 1992, 1993 University of Pennsylvania. All rights reserved.
  Copyright 1993 University of Toronto. All rights reserved.
    
  Maintaner: Nikodemus Siivola <https://github.com/nikodemus/screamer>
  
* SCREAMER-PLUS ~A by Simon White
  Copyright 1998-2000 University of Aberdeen 
  
* CLOSER-MOP by Pascal Costanza 
  Copyright (c) 2005 - 2016 Pascal Costanza
	  
* ITERATE by Jonathan Amsterdam
  Adapted to ANSI Common Lisp in 2003 by Andreas Fuchs
  Copyright 1989, Jonathan Amsterdam.
  
* Code excerpts from t2l-screamer by Killian Sprotte

* Retract (extension to Screamer) by Buddy Kresge" 
s::*screamer-version* ?::*screamer+-version*))
