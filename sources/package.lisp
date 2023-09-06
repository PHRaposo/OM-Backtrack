(IN-PACKAGE :CL-USER)
(SCREAMER:DEFINE-SCREAMER-PACKAGE :om-screamer
 (:nicknames :om?)
 (:use :screamer+)
 )
(IN-PACKAGE :om-screamer)

(in-package :om)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CONSTRAINT AND DOMAIN CLASSES FOR SCREAMER-SCORE

(om::defclass* score-constraint ()
 ((constraint-parameters :initform nil :initarg nil :reader get-constraint-parameters :writer set-constraint-parameters :documentation "list of screamer score constraint parameters"))
 (:documentation "A simple container for screamer-score constraint."))
 
(om::defmethod screamer-score-constraint-p ((self score-constraint)) t)
(om::defmethod screamer-score-constraint-p ((self t )) nil ) 
	
(om::defmethod* constraint-parameters ((self score-constraint))
 (get-constraint-parameters self))

(om::defclass* score-domain ()
 ((domain-parameters :initform nil :initarg nil :reader get-domain-parameters :writer set-domain-parameters :documentation "list of screamer score domain parameters"))
 (:documentation "A simple container for screamer-score domain."))
 
(om::defmethod screamer-score-domain-p ((self score-domain)) t)
(om::defmethod screamer-score-domain-p ((self t )) nil )
 
(om::defmethod* domain-parameters ((self score-domain))
 (get-domain-parameters self))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

