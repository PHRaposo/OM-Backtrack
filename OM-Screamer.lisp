;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OM-SCREAMER
;;; 
;;; 

(in-package :om)

(mapc 'compile&load (list
                         ;(make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "om-modifs" :type "lisp")	
                         (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "screamboxes" :type "lisp")
                         (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "screamfuns" :type "lisp") 
                         (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "screaminterface" :type "lisp")
						 (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "examples")) :name "scream-ais" :type "lisp")	
                         ;(make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources/closer-mop")) :name "closer-mop-packages" :type "lisp")						 
                         ;(make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources/closer-mop")) :name "closer-mop-shared" :type "lisp")
						 ;(make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources/closer-mop")) :name "closer-lispworks" :type "lisp")							 
                         ;(make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources/screamer-plus")) :name "screamer-plus" :type "lisp")						  						 
                    )
)

(fill-library '( ("Backtrack" Nil Nil (an-integer-between
									   a-member-of
									   apply-cont
									   list-of-members-of 
                                       list-of-integers-between 
									   a-chord-in
									   list-of-chords-in
									   alldiff?
									   growing? ) Nil)

                ;("FOLDER2" Nil Nil (package::FUNCTION) Nil)

))
(print 
"
OM-SCREAMER Library

")



