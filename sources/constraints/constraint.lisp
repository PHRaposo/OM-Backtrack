;OpenMusic
;
;Copyright (C) 1997-2003 by IRCAM-Centre Georges Pompidou, Paris, France.
; 
;This program is free software; you can redistribute it and/or
;modify it under the terms of the GNU General Public License
;as published by the Free Software Foundation; either version 2
;of the License, or (at your option) any later version.
;
;See file LICENSE for further informations on licensing terms.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with this program; if not, write to the Free Software
;Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;
;Authors: Gerard Assayag and Augusto Agon

;Modified for linux by: Gerardo M. Sarria M. and Jose Fernando Diago
;                       Pontificia Universidad Javeriana - Cali, Colombia

;DocFile
;
;Last Modifications :
;18/10/1997 First date.
;DocFile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :om)

;;-----------------------------------------------------------------------------

(defmethod str-object ((self t)) self)
(defmethod extract-variables ((self t)) nil)
(defmethod new-const-replace-vars ((self t) duplicatas) (list self))
(defmethod translate-expr ((self t) vars) self)

;;=============================================================================
;; Variables
;;=============================================================================

(defclass* c-variable ()
  ((name :initform nil :initarg :name :accessor name)
   (obj-ref :initform nil :initarg :obj-ref :accessor obj-ref)
   (domaine :initform nil :initarg :domaine :accessor domaine)
   (slot-ref :initform nil :initarg :slot-ref :accessor slot-ref))
  ;(:metaclass OMStandardClass)
  )

(defmethod str-object ((self c-variable)) (name self))
(defmethod print-object ((self c-variable) x) (format x (name self)))
(defmethod extract-variables ((self c-variable)) (list self))

(defmethod new-const-replace-vars ((self c-variable) duplicatas)
  (let ((newvar (find-if #'(lambda (x) (equal self (car x) )) duplicatas)))
    (if newvar (list (second newvar)) (list self))))

;;-----------------------------------------------------------------------------

(defclass* permut-variable (c-variable) ())

(defmethod permut-var-p ((self permut-variable)) t)
(defmethod permut-var-p ((self t)) nil)

;;=============================================================================
;; Expression
;;=============================================================================

(defclass* cons-expr ()
  ((rel :initform nil :initarg :rel :accessor rel)
   (exprs :initform nil :initarg :exprs :accessor exprs)))

(defmethod print-object ((self cons-expr) x) (format x (str-object self)))
(defmethod str-object ((self cons-expr)) 
  (format nil "<~D ~{~D ~}>" (rel self) (mapcar #'str-object (exprs self))))

(defmethod new-const-replace-vars ((self cons-expr) duplicatas)
  (list (make-instance 'cons-expr
		       :rel (rel self)
		       :exprs (loop for item in (exprs self)
				    append (new-const-replace-vars item duplicatas)))))

(defmethod extract-variables ((self cons-expr))
  (remove-duplicates
   (loop for item in (exprs self)
	 append (extract-variables item)) :test 'equal))

;;=============================================================================
;; Constraint
;;=============================================================================

(defclass* constraint ()
  ((name :initform nil :initarg :name :accessor name)
   (exprs :initform nil :initarg :exprs :accessor exprs)))

(defmethod constraint-p ((self constraint)) t)
(defmethod constraint-p ((self t)) nil)

(defmethod print-object ((self constraint) x) (format x (str-object self)))
(defmethod str-object ((self constraint)) 
  (format nil "< ~D ~{~D ~} >" (name self) (mapcar #'str-object (exprs self))))

(defmethod build-csp ((self constraint))
  (let ((variables (loop for item in (exprs self)
			 append (extract-variables item)))
	duplicatas realvars)
    (setf duplicatas (remove-duplicates variables :test 'equal :key 'obj-ref))
    (setf realvars duplicatas)
    (setf duplicatas (set-difference  variables duplicatas :test 'equal))
    (setf duplicatas (loop for item in duplicatas collect
			   (list item (find-if #'(lambda (x) 
						   (equal (obj-ref x) (obj-ref item))) realvars))))
    (make-instance 'csp
		   :vars realvars
		   :constraints (list (new-const-replace-vars self duplicatas)))))

(defmethod new-const-replace-vars ((self constraint) duplicatas)
  (make-instance 'constraint
		 :name (name self)
		 :exprs (loop for item in (exprs self)
			      append (new-const-replace-vars item duplicatas))))

(defmethod extract-variables ((self constraint))
  (remove-duplicates
   (loop for item in (exprs self)
	 append (extract-variables item)) :test 'equal))

(defmethod extract-variables ((self list)) 
  (remove-duplicates
   (loop for item in self
	 append (extract-variables item)) :test 'equal))

;;=============================================================================
;; CSP
;;=============================================================================

(defclass* csp ()
  ((vars :initform nil :initarg :vars :accessor vars)
   (constraints :initform nil :initarg :constraints :accessor constraints)))

(defmethod unify-csp ((csp1 csp) (csp2 csp))
  (let ((var1 (vars csp1))
	(var2 (vars csp2))
	duplicatas realvars)
    (setf duplicatas (append var1 var2))
    (setf duplicatas (remove-duplicates duplicatas :test 'equal :key 'obj-ref :from-end t))
    (setf realvars duplicatas)
    (setf duplicatas (set-difference  (append var1 var2) duplicatas :test 'equal))
    (setf duplicatas (loop for item in duplicatas collect
			   (list item (find-if #'(lambda (x) 
						   (equal (obj-ref x) (obj-ref item))) realvars))))
    (make-instance 'csp
		   :vars realvars
		   :constraints (append (constraints csp1)
					(loop for item in (constraints csp2)
					      collect (new-const-replace-vars item duplicatas))))))

;;-EOF-------------------------------------------------------------------------
