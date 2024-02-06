(in-package :om?)

(defvar *guitar-number-of-strings* nil)
(defvar *guitar-tuning* nil)
(defvar *guitar-string-notes* nil)	
(defvar *guitar-fret-notes* nil)
(defvar *maximum-fret-distance* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DEFAULT TEXT-EDITOR

(defvar *default-text-editor* nil)

(defun set-default-text-editor ()
 (let ((app (om::om-choose-file-dialog :directory om::*om-root*)))
 (if app
    (setf *default-text-editor* 
		 (file-namestring (pathname (string-trim #+win32 ".exe" #+macosx ".app" (string-trim "/" (namestring app))))))
		 
    (om::om-abort))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GUITAR-SETUP

(defun set-guitar-fret-string-notes (&optional frets-list)
(when frets-list
    (unless (= (length frets-list) *guitar-number-of-strings*)
                (error "The length of the list of frets (~S) do not corresponds to the number of strings (~S)." (length frets-list) *guitar-number-of-strings*)))

 (let* ((frets (if frets-list 
                      (om::arithm-ser 0  (om::list-max frets-list) 1)
                      (om::arithm-ser 0 19 1)))
       (strings (om::arithm-ser 1 (length *guitar-tuning*) 1))
       (string-notes (if frets-list 
                               (mapcar #'(lambda (open-string fret)
		                        (om::arithm-ser open-string (+ open-string (* 100 fret)) 100))
				      *guitar-tuning* frets-list)
                                      (mapcar #'(lambda (open-string)
		                        (om::arithm-ser open-string (+ open-string 1900) 100))
				      *guitar-tuning*)))     
	)

(setf *guitar-string-notes* (mapcar #'list strings string-notes))
			 
(setf *guitar-fret-notes* 
 (mapcar #'(lambda (fret-notes)
  (list (first fret-notes) (remove nil (second fret-notes))))
 (om::mat-trans (list frets (om::mat-trans string-notes)))))
))

(defvar *all-guitar-notes-frets-strings* nil)
(setf *all-guitar-notes-frets-strings* nil)

(defvar *allnotes* nil)
(setf *allnotes* nil)

(defvar *note-fret-string* nil)
(setf *note-fret-string* nil)

;(defvar *note-fret-string-hash* (make-hash-table :test #'equalv))
 	
;(defun get-fret-stringv (var)
; (funcallv #'gethash var *note-fret-string-hash*))
	
 (defvar *fret-string-and-note* nil)

 (defun set-fret-string-and-note ()
 (setf *fret-string-and-note* nil)
  (dolist (nfs *note-fret-string*)
   (dolist (fs (second nfs)) 
    (setf *fret-string-and-note* 
 	     (om::x-append *fret-string-and-note* (list (list fs (first nfs))))))))
		 
 (defvar *fret-string-and-note-hash* (make-hash-table :test #'equalv))

 (defun fill-fret-string-and-note-hash ()
   (let ((fret-string-note *fret-string-and-note*)) 
     (clrhash *fret-string-and-note-hash*)
     (dolist (f-s-n fret-string-note) 
       (setf (gethash (first f-s-n) *fret-string-and-note-hash*)  
             (second f-s-n)))					
 	))

 (defun get-note-from-fret-string (f-s)
  (s::funcallv #'gethash f-s *fret-string-and-note-hash*))
	
(defun set-guitar-tuning (type &optional open-strings frets-list)
 (cond ((equal type :standard)
        (progn (setf *guitar-tuning* '(6400 5900 5500 5000 4500 4000)) 
			   (set-guitar-fret-string-notes frets-list)
			   (set-all-guitar-notes-frets-strings)
			   ;(fill-guitar-note-fret-string-hash)
			   (set-fret-string-and-note)
			   (fill-fret-string-and-note-hash)
		))
		
	   ((equal type :scordatura)
	    (progn (setf *guitar-tuning* open-strings)  
			   (set-guitar-fret-string-notes frets-list)
			   (set-all-guitar-notes-frets-strings)			   
			   ;(fill-guitar-note-fret-string-hash)
			   (set-fret-string-and-note)
			   (fill-fret-string-and-note-hash)	   
		))
		
		(t (error "~S is not a valid argument to type (:standard or :scordatura)." type)))
 )

(cl::defun set-all-guitar-notes-frets-strings ()

 (setf *all-guitar-notes-frets-strings* nil)
 (setf *allnotes* nil)

 (dolist (fret *guitar-fret-notes*)
  (dolist (string *guitar-string-notes*) 
             (when (member (nth (first fret) (second string)) (second fret))
                           (setf *all-guitar-notes-frets-strings* 
	 					         (om::x-append *all-guitar-notes-frets-strings* 
	  							               (list (list (nth (first fret) (second string)) 
										   	               (first fret) (first string))))))))
			  
 (setf *allnotes* (om::sort-list (remove-duplicates (om::flat (mapcar #'second *guitar-string-notes*)))))

 (setf *note-fret-string* (append '(nil (nil))
  (loop for note in *allnotes* 
        collect (list note (loop for nfs in *all-guitar-notes-frets-strings*
                                 when (= note (first nfs))
                                 collect (cdr nfs))))))
 )
 
#|(defun fill-guitar-note-fret-string-hash ()
(let ((note-fret-string *note-fret-string*)) 
 (clrhash *note-fret-string-hash*)
 (dolist (n-f-s note-fret-string) 
   (setf (gethash (first n-f-s) *note-fret-string-hash*)  
         (second n-f-s)))					
))|#

(defun get-open-string-number (open-strings)
 (mapcar #'(lambda (open-string) 
	 (cond ((= open-string (first *guitar-tuning*)) 1)
   		   ((= open-string (second *guitar-tuning*)) 2)
   		   ((= open-string (third *guitar-tuning*)) 3)
   		   ((= open-string (fourth *guitar-tuning*)) 4)
   		   ((= open-string (fifth *guitar-tuning*)) 5)
   		   ((= open-string (sixth *guitar-tuning*)) 6)
           ((= open-string (seventh *guitar-tuning*)) 7)
		   (t nil)))
  open-strings))
				   
(defun guitar-setup (n-strings tuning max-fret-dist &optional open-strings frets-list print?)
 (setf *maximum-fret-distance* max-fret-dist)
 (if (equal tuning :standard) 
	 (setf *guitar-number-of-strings* 6)
	 (setf *guitar-number-of-strings* 
		   (if n-strings 
		       n-strings 
			  (error "Please insert the number of strings."))))
 (set-guitar-tuning tuning open-strings frets-list)
 (if print?
	(om::om-message-dialog 
	 (format nil 
"GUITAR PARAMETERS
NUMBER OF STRINGS: ~A
TUNING: ~A
MAXIMUM FRET DISTANCE: ~A
OPTIONAL:
OPEN-STRINGS: ~A
FRETS-LIST: ~A"
	*guitar-number-of-strings* tuning *maximum-fret-distance* 
	(let ((os-numbers (get-open-string-number *guitar-tuning*)))
	(mapcar #'list os-numbers *guitar-tuning*))
	 frets-list))))

(guitar-setup nil :standard 4 nil '(12 12 12 12 12 12) nil)
  

   #|(defun a-fret-and-stringv (x)
     (let ((fretv (an-integer-betweenv 0 (1- (length *guitar-fret-notes*))))
           (open-stringv (a-member-ofv *guitar-tuning*))
           (string-lengths (om::om- (mapcar #'length (mapcar #'second *guitar-string-notes*)) 1))
          )

         (assert! (apply #'orv (mapcar #'(lambda (os sl)
                                           (andv (=v open-stringv os)
                                                 (<=v fretv sl)))
                                *guitar-tuning* string-lengths)))
  
	 (assert! (=v x (+v (*v fretv 100) open-stringv)))
 	  		  		    
        (list x fretv open-stringv)
      ))|#
	
;;; END-OF-GUITAR-SETUP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GUITAR TABLATURE
	 								
(defvar *guitar-tablature* nil)
(setf *guitar-tablature* nil)
 
(defvar *guitar-tablature-temp* nil)
(setf *guitar-tablature-temp* nil)

(defvar *latest-tabfile* nil)
(setf *latest-tabfile* nil)

(defun reset-guitar-tablature ()
(setf *guitar-tablature* nil)
(setf *guitar-tablature-temp* nil)
(setf *latest-tabfile* nil)
)

(defun make-tablature (values)
(let* ((spaces (list nil nil nil nil nil nil))
      (strings (get-open-string-number (mapcar #'third values)))
	  (frets (mapcar #'second values))
      (frets-positions (om::subs-posn spaces (om::om- strings 1) frets))
	  (tablature-temp *guitar-tablature-temp*))

 (setf *guitar-tablature-temp* (om::x-append tablature-temp (list frets-positions)))
 ))

(defun concatString (list) ;by Vijay Mathew - stackoverflow
  "A non-recursive function that concatenates a list of strings."
  (if (listp list)
      (with-output-to-string (s)
         (dolist (item list)
           (if (stringp item)
             (format s "~a" item))))))

(cl::defun guitar-tablature ()
 (cond ((and *guitar-tablature* (null *guitar-tablature-temp*)) 
	     (if (probe-file *latest-tabfile*)
		 (cond ((equal om::*om-os* :win)
                    (sys:call-system (format nil "~A" (om::om-path2cmdpath *latest-tabfile*))))
                 (t   
		  (if *default-text-editor* 
			  (om::om-cmd-line (format nil "open -a ~s ~s" *default-text-editor* (om::om-path2cmdpath *latest-tabfile*)))
			  (om::om-cmd-line (format nil "open ~s" (om::om-path2cmdpath *latest-tabfile*)))
		   )))
		   (if *default-text-editor*	
			 (om::om-cmd-line (format nil "open -a ~s ~s" *default-text-editor* (om::om-path2cmdpath 
				                                    (let ((path (om::om-choose-file-dialog))) 
								     (if path
                                                                         path 
                                                                        (om::om-abort))))))
   			 (om::om-cmd-line (format nil "open ~s" (om::om-path2cmdpath 
   				                                    (let ((path (om::om-choose-file-dialog))) 
   								     (if path
                                                                            path 
                                                                           (om::om-abort))))))																		
																		
																		))
            nil)

       ((and *guitar-tablature-temp* (null *guitar-tablature*)) 

 (let* ((chord-tabs *guitar-tablature-temp*)
          (string-names (om::mc->n *guitar-tuning*))
          (tab-matrix (om::mat-trans chord-tabs))
		  (pages (om::mat-trans (mapcar #'(lambda (x) 
			                      (om::group-list x 8 'linear)) tab-matrix)))		  
     	    (subst-nil-from-strings (loop for strings in pages
				                     collect (loop for string in strings
     			                          for string-n in string-names 
     			                  collect (om::flat (om::x-append string-n "||-  "
     								                   (loop for fret in string 
     								                         collect (om::x-append (if fret fret "-") "|")
     									   ) )))))
     	(template (concatstring (om::flat (loop for strings in subst-nil-from-strings
			collect (om::x-append (concatstring (om::x-append "~%" (om::flat (loop for string in strings
     		                  collect (om::x-append (loop for x from 1 to (length string)
     					                              collect "~4A")
     							"~&"))))) "~|" "~&"))))) 									   	
            )

					
 (setf *guitar-tablature-temp* nil)
 
 (setf *guitar-tablature* (eval `(format nil ,template ,.(om::flat subst-nil-from-strings)))) 
 (let ((path (pathname (concatenate 'string (namestring (om::def-save-directory)) "guitar-tab.txt")))) ;(symbol-name (gensym "tab")) ".txt"))))
       (setf *latest-tabfile* path)   	 
	 (with-open-file (stream path :direction :output :if-exists :supersede)
      (format stream *guitar-tablature*))

   (cond ((equal om::*om-os* :mac)
          (if *default-text-editor* 
	      (om::om-cmd-line (format nil "open -a ~s ~s" *default-text-editor* (om::om-path2cmdpath path)))
              (om::om-cmd-line (format nil "open ~s" (om::om-path2cmdpath path)))
 	   ))

         ((equal om::*om-os* :win)
          (sys:call-system (format nil "~A"(om::om-path2cmdpath path))))

         ((equal om::*om-os* :linux)
          (progn (om::om-message-dialog "Guitar tablature file has not been implemented in Linux OS yet.") 
                 (om::om-abort)))

         (t nil))
  nil)
 ))

      ((and *guitar-tablature-temp* *guitar-tablature*)
       (setf *guitar-tablature* nil)
	   
 (let* ((chord-tabs *guitar-tablature-temp*)
          (string-names (om::mc->n *guitar-tuning*))
          (tab-matrix (om::mat-trans chord-tabs))
		  (pages (om::mat-trans (mapcar #'(lambda (x) 
			                      (om::group-list x 8 'linear)) tab-matrix)))	
   	    (subst-nil-from-strings (loop for strings in pages
			                    collect (loop for string in strings
   			                          for string-n in string-names 
   			                  collect (om::flat (om::x-append string-n "||-  "
   								                   (loop for fret in string 
   								                         collect (om::x-append (if fret fret "-") "|")
   									   ) )))))
    	(template (concatstring (om::flat (loop for strings in subst-nil-from-strings
		collect (om::x-append (concatstring (om::x-append "~%" (om::flat (loop for string in strings
    		                  collect (om::x-append (loop for x from 1 to (length string)
    					                              collect "~4A")
    							"~&")))))  "~|" "~&")))))									   						  																		  
           )
 				   
 (setf *guitar-tablature-temp* nil)
 
 (setf *guitar-tablature* (eval `(format nil ,template ,.(om::flat subst-nil-from-strings))))
 
 (let ((path (pathname (concatenate 'string (namestring (om::def-save-directory)) "guitar-tab.txt")))) ;(symbol-name (gensym "tab")) ".txt"))))
    (setf *latest-tabfile* path)
	 (with-open-file (stream path :direction :output :if-exists :supersede)
   (format stream *guitar-tablature*))
   
   (cond ((equal om::*om-os* :mac)
          (if *default-text-editor* 
	      (om::om-cmd-line (format nil "open -a ~s ~s" *default-text-editor* (om::om-path2cmdpath path)))
              (om::om-cmd-line (format nil "open ~s" (om::om-path2cmdpath path)))
 	   ))

         ((equal om::*om-os* :win)
          (sys:call-system (format nil "~A" (om::om-path2cmdpath path))))

         ((equal om::*om-os* :linux)
          (progn (om::om-message-dialog "Guitar tablature file has not been implemented in Linux OS yet.") 
                 (om::om-abort)))

         (t nil))
     
   ;(if *default-text-editor* 
;	   (om::om-cmd-line (format nil "open -a ~s ~s" *default-text-editor* (om::om-path2cmdpath path)))
;       (om::om-cmd-line (format nil "open ~s" (om::om-path2cmdpath path)))
;	)

  nil)
 ))
     
       ((and (null *guitar-tablature-temp*) (null *guitar-tablature*))
        (progn (om::om-message-dialog "No guitar tablature found.") (om::om-abort)))

       (t (progn (om::om-message-dialog "No guitar tablature found.") (om::om-abort)))))

(cl::defun open-guitar-tab ()
 (guitar-tablature))
 
;;;END OF GUITAR TABLATURE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CONSTRAINTS
	
(defun guitar-2-note-playable-chord (note-fret-stringv)
 (let* ((fretv1 (second (first note-fret-stringv)))
        (fretv2 (second (second note-fret-stringv)))
		(intervalv (-v fretv2 fretv1))   
        )  
  (assert! 
   (orv (andv (=v fretv1 0)
                      (=v fretv2 0)) 
	             (orv (=v fretv1 0) 
                      (=v fretv2 0))
			(andv (>=v intervalv (* -1 *maximum-fret-distance*))
			      (<=v intervalv  *maximum-fret-distance*))		  	
			;(?::interval-memberv fretv2 fretv1 (om::arithm-ser (* -1 *maximum-fret-distance*) *maximum-fret-distance* 1))			  
	        ) )		         
   ))
   
(defun guitar-3-4-5-6-note-playable-chord (note-fret-stringv)
(let* ((combs-posn (asc-permutations (om::arithm-ser 0 (1- (length note-fret-stringv)) 1) 2)) 
       (combinations (mapcar #'(lambda (posn)
                                (om::posn-match note-fret-stringv posn)) 
				    combs-posn))
   )		
 (mapcar #'guitar-2-note-playable-chord combinations)
 ))

(defun quick-sortv (listv)
 (?::ifv (equalv nil listv) 
	     nil
        (?::appendv (quick-sortv (list<v (carv listv) (cdrv listv)))
        			(funcallv #'cons (carv listv) nil) 
        			(quick-sortv (list>=v (carv listv) (cdrv listv))))))

(defun list<v (a b)
  (?::ifv (orv (equalv nil a) (equalv nil b)) 
	       nil
           (?::ifv (<v a (carv b)) 
		           (list<v a (cdrv b))
                   (funcallv #'cons (carv b) (list<v a (cdrv b))))))

(defun list>=v (a b)
  (?::ifv (orv (equalv nil a) (equalv nil b))
           nil 
           (?::ifv (>=v a (carv b)) 
			       (list>=v a (cdrv b))
                   (funcallv #'cons (carv b) (list>=v a (cdrv b))))))
 			   		  	   
(defun remove-open-stringsv (listv)
(labels ((remove-0 (x)
          (?::ifv (equalv nil (cdrv x))
		           (?::ifv (=v (firstv x) 0) nil x)
	                       (?::ifv (=v (firstv x) 0) 
			                       (remove-0 (cdrv x))
				                   (?::appendv (list (firstv x)) (remove-0 (cdrv x)))))))
				
 (remove-0 listv)))

(defun sumv-listv (listv)
(labels ((sv (x accumul)
         (?::ifv (equalv nil x)
		  accumul
          (sv (cdrv x) (+v (firstv x) accumul))))) 
			
 (sv listv 0)))
 
 (defun count-occurrencesv (elemv listv)
  (apply #'count-truesv (mapcar #'(lambda (list-elemv)(eqv elemv list-elemv)) listv)))
	   
#|(defun gen-2-fret-prime()
     (let ((2-fret-prime (list-of-integers-betweenv 2 1 (1+ *maximum-fret-distance*))))
		
  (assert! (=v (first 2-fret-prime) 1))	
  (assert! (apply #'<=v 2-fret-prime))	
 
  (all-values (solution 2-fret-prime (static-ordering #'linear-force)))))
     
(defun gen-3-fret-prime()
    (let ((3-fret-prime (list-of-integers-betweenv 3 1 (1+ *maximum-fret-distance*))))
		
 (assert! (=v (first 3-fret-prime) 1))	
 (assert! (apply #'<=v 3-fret-prime))	
 (assert! (?::notanyv #'(lambda (x) (equalv 3-fret-prime x)) '((1 4 4) (1 4 5) (1 5 5))))
	 
 (all-values (solution 3-fret-prime (static-ordering #'linear-force)))))

 (defun gen-4-fret-prime()
     (let ((4-fret-prime (list-of-integers-betweenv 4 1 (1+ *maximum-fret-distance*))))
		
  (assert! (=v (first 4-fret-prime) 1))	
  (assert! (apply #'<=v 4-fret-prime))	
  (assert! (?::notanyv #'(lambda (x) 
	                      (equalv 4-fret-prime x))
	       '((1 1 4 4) (1 1 5 5) (1 2 2 5) (1 2 3 5) (1 2 4 5) (1 2 5 5) (1 3 3 5) (1 3 5 5) (1 4 4 4) (1 4 4 5) (1 4 5 5) (1 5 5 5))))	  
 
  (all-values (solution 4-fret-prime (static-ordering #'linear-force)))))|#	 		 
 		
 #|(defvar *2-fret-prime* nil)
 (setf *2-fret-prime* 
	 '((1 1) (1 2) (1 3) (1 4) (1 5)))|#
	
 (defvar *2-fret-prime-intervals* nil)  
 (setf *2-fret-prime-intervals* '((0) (1) (2) (3) (4))) 	 
	 

 #|(defvar *3-fret-prime* nil)
 (setf *3-fret-prime* 
	 '((1 1 1) (1 1 2) (1 1 3) (1 1 4) (1 1 5) (1 2 2) (1 2 3) (1 2 4) (1 2 5) (1 3 3) (1 3 4) (1 3 5)
	   ;(1 4 4) (1 4 5) (1 5 5)
	   )
  )|#

(defvar *3-fret-prime-intervals* nil) 
(setf *3-fret-prime-intervals* '((0 0) (0 1) (0 2) (0 3) (0 4) (1 0) (1 1) (1 2) (1 3) (2 0) (2 1) (2 2) (3 1)))
	 
 #|(defvar *4-fret-prime* nil)
 (setf *4-fret-prime* 
     '((1 1 1 1) (1 1 1 2) (1 1 1 3) (1 1 1 4) (1 1 1 5) (1 1 2 2) (1 1 2 3) (1 1 2 4) (1 1 2 5) (1 1 3 3) (1 1 3 4) (1 1 3 5) ;(1 1 4 4)
	   (1 1 4 5) ;(1 1 5 5)
	 (1 2 2 2) (1 2 2 3) (1 2 2 4) ;(1 2 2 5)
	 (1 2 3 3) (1 2 3 4) ;(1 2 3 5)
	 (1 2 4 4) ;(1 2 4 5)
	 ;(1 2 5 5)
	 (1 3 3 3) (1 3 3 4) ;(1 3 3 5)
	 (1 3 4 4) (1 3 4 5) ;(1 3 5 5)
     ;(1 4 4 4) (1 4 4 5)
	 ;(1 4 5 5) 
	 ;(1 5 5 5)
	 )
 )|#
 
 (defvar *4-fret-prime-intervals* nil) 
 (setf *4-fret-prime-intervals* '((0 0 0) (0 0 1) (0 0 2) (0 0 3) (0 0 4) (0 1 0) (0 1 1) (0 1 2) (0 1 3) (0 2 0) (0 2 1) (0 2 2) (0 3 1)
                                  (1 0 0) (1 0 1) (1 0 2) (1 1 0) (1 1 1) (1 2 0) (2 0 0) (2 0 1) (2 1 0) (2 1 1)))
 
 #|(defvar *5-fret-prime* nil)
 (setf *5-fret-prime* 
	 '((1 1 1 1 1) (1 1 1 1 2) (1 1 1 1 3) (1 1 1 1 4) (1 1 1 1 5) (1 1 1 2 2) (1 1 1 2 3) (1 1 1 2 4) (1 1 1 2 5) (1 1 1 3 3) (1 1 1 3 4) (1 1 1 3 5) 
	   (1 1 1 4 5) (1 1 2 2 2) (1 1 2 2 3) (1 1 2 2 4) (1 1 2 3 3) (1 1 2 3 4) (1 1 2 4 4) (1 1 3 3 3) (1 1 3 3 4) (1 1 3 4 4) (1 1 3 4 5))
 )|#
 
 (defvar *5-fret-prime-intervals* nil) 
 (setf *5-fret-prime-intervals* '((0 0 0 0) (0 0 0 1) (0 0 0 2) (0 0 0 3) (0 0 0 4) (0 0 1 0) (0 0 1 1) (0 0 1 2) (0 0 1 3) (0 0 2 0) (0 0 2 1) (0 0 2 2) 
	 (0 0 3 1) (0 1 0 0) (0 1 0 1) (0 1 0 2) (0 1 1 0) (0 1 1 1) (0 1 2 0) (0 2 0 0) (0 2 0 1) (0 2 1 0) (0 2 1 1))) 
 	 			
#|(defvar *6-fret-prime* nil)
 (setf *6-fret-prime* 
	 '((1 1 1 1 1 1) (1 1 1 1 1 2) (1 1 1 1 1 3) (1 1 1 1 1 4) (1 1 1 1 1 5) (1 1 1 1 2 2) (1 1 1 1 2 3) (1 1 1 1 2 4) 
	   (1 1 1 1 2 5) (1 1 1 1 3 3) (1 1 1 1 3 4) (1 1 1 1 3 5) (1 1 1 1 4 5) (1 1 1 2 2 2) (1 1 1 2 2 3) (1 1 1 2 2 4)
	   (1 1 1 2 3 3) (1 1 1 2 3 4) (1 1 1 2 4 4) (1 1 1 3 3 3) (1 1 1 3 3 4) (1 1 1 3 4 4) (1 1 1 3 4 5))
 )|#

 (defvar *6-fret-prime-intervals* nil) 
 (setf *6-fret-prime-intervals* '((0 0 0 0 0) (0 0 0 0 1) (0 0 0 0 2) (0 0 0 0 3) (0 0 0 0 4) (0 0 0 1 0) (0 0 0 1 1) (0 0 0 1 2) (0 0 0 1 3) 
 (0 0 0 2 0) (0 0 0 2 1) (0 0 0 2 2) (0 0 0 3 1) (0 0 1 0 0) (0 0 1 0 1) (0 0 1 0 2) (0 0 1 1 0) (0 0 1 1 1) (0 0 1 2 0) (0 0 2 0 0) (0 0 2 0 1) 
 (0 0 2 1 0) (0 0 2 1 1)))
   				   					   			
(defun fret-prime-memberv (fret-primev)
 (let ((length-primev (?::lengthv fret-primev))
        (2-prime-intervals *2-fret-prime-intervals*)
        (3-prime-intervals *3-fret-prime-intervals*)		
        (4-prime-intervals *4-fret-prime-intervals*)
        (5-prime-intervals *5-fret-prime-intervals*)
        (6-prime-intervals *6-fret-prime-intervals*)						
		)
 (assert! 
          (?::ifv (=v length-primev 0)
                   t
		  (?::ifv (=v length-primev 1)
		  (fp-memberv fret-primev 2-prime-intervals)
		  
		  (?::ifv (=v length-primev 2)
		  (fp-memberv fret-primev 3-prime-intervals)
		  		  
		  (?::ifv (=v length-primev 3)
		  (fp-memberv fret-primev 4-prime-intervals)		  

		  (?::ifv (=v length-primev 4)
		  (fp-memberv fret-primev 5-prime-intervals)			            
				   
		  (?::ifv (=v length-primev 5)
		  (fp-memberv fret-primev 6-prime-intervals)				   
          )))))) )
 ))
 
 (defun fp-memberv (primev prime-list)
 ;(?::somev #'(lambda (x) (equalv primev x)) prime-list))
 (when prime-list
 (?::ifv (equalv primev (?::carv prime-list))
          t
 (fp-memberv primev (?::cdrv prime-list)))))

(defun gstring-memberv (stringsv strings)
 (?::everyv #'(lambda (x)
               (?::ifv (equalv nil x) 
                        t
		     	       (memberv x strings))) 
  stringsv))
  		 
(defun cs-stringsv (n-min-pressed-fretv n-open-stringsv length-pfv open-stringsv not-open-stringsv)
 (assert! 
  (?::ifv  (orv (equalv length-pfv 0)
                (equalv length-pfv 6)
  	            (equalv length-pfv 2)
			    (equalv length-pfv 1)
  			    (equalv n-open-stringsv 6)
  			    (equalv n-open-stringsv 5)
  			    (equalv n-open-stringsv 4)								 
			    (equalv n-open-stringsv 0)		   
	            (equalv n-min-pressed-fretv 0)
			    (equalv n-min-pressed-fretv 1)) 		   
            t
		   
  (?::ifv (andv (equalv length-pfv 5)
                (>=v n-min-pressed-fretv 2)
	  	 		(equalv n-open-stringsv 1))				
		  (gstring-memberv open-stringsv '(6)) 
			    				   	 
  (?::ifv (andv (equalv length-pfv 4)
                (>=v n-min-pressed-fretv 2)
				(equalv n-open-stringsv 1))				
		  (orv (andv (gstring-memberv open-stringsv (list (sixth *guitar-tuning*)))
		   		     (gstring-memberv not-open-stringsv (om::first-n *guitar-tuning* 5)))
			   (andv (gstring-memberv open-stringsv (om::last-n *guitar-tuning* 2))
			   	     (gstring-memberv not-open-stringsv (om::first-n *guitar-tuning* 4))) )
		   
  (?::ifv (andv (equalv length-pfv 4)
                (>=v n-min-pressed-fretv 2)
 				(equalv n-open-stringsv 2))			   
		   (andv (gstring-memberv open-stringsv (om::last-n *guitar-tuning* 2))
		   	     (gstring-memberv not-open-stringsv (om::first-n *guitar-tuning* 4)) )
				 		   
 (?::ifv (andv (equalv length-pfv 3)
               (>=v n-min-pressed-fretv 2)
			   (equalv n-open-stringsv 1))			   
 		  (orv (andv (gstring-memberv open-stringsv (list (sixth *guitar-tuning*)))
 		   		     (gstring-memberv not-open-stringsv (om::first-n *guitar-tuning* 5)) )
 			   (andv (gstring-memberv open-stringsv (om::last-n *guitar-tuning* 2))
 			   	     (gstring-memberv not-open-stringsv (om::first-n *guitar-tuning* 4)) ) 
   			   (andv (gstring-memberv open-stringsv (om::last-n *guitar-tuning* 3))
   			   	     (gstring-memberv not-open-stringsv (om::first-n *guitar-tuning* 3)) ) )
					 
 (?::ifv (andv (equalv length-pfv 3)
               (>=v n-min-pressed-fretv 2)
			   (equalv n-open-stringsv 2))			   
 		  (orv (andv (gstring-memberv open-stringsv (om::x-append (first *guitar-tuning*) (om::last-n *guitar-tuning* 2)))
 		   		     (gstring-memberv not-open-stringsv (om::first-n (cdr *guitar-tuning*) 3)) ) 		  
			   (andv (gstring-memberv open-stringsv (om::x-append (om::first-n *guitar-tuning* 2) (om::last-elem *guitar-tuning*)))
 		   		     (gstring-memberv not-open-stringsv (om::first-n (cddr *guitar-tuning*) 3)) ) 
 			   (andv (gstring-memberv open-stringsv (om::last-n *guitar-tuning* 3))
 			   	     (gstring-memberv not-open-stringsv (om::first-n *guitar-tuning* 3)) ) )				 
 (?::ifv (andv (equalv length-pfv 3)
               (>=v n-min-pressed-fretv 2)
			   (equalv n-open-stringsv 3))			   
 		 (andv (gstring-memberv open-stringsv (om::last-n *guitar-tuning* 3))
 		   	  	(gstring-memberv not-open-stringsv (om::first-n *guitar-tuning* 3)) )				 		   
		   	   
           nil)))))))))
  								 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;; PLAYABLE CHORDS
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun a-fret-and-stringv (x)
(let* ((open-stringv (a-member-ofv *guitar-tuning*))
      (fretv (an-integer-betweenv 0 (1- (length *guitar-fret-notes*))))
	  (strings-lengths (om::om- (mapcar #'length (mapcar #'second *guitar-string-notes*)) 1))
       valx)
  (assert! (=v x (+v (*v fretv 100) open-stringv)))
  
  (assert! (apply #'orv (mapcar #'(lambda (os string-l)
             (andv (=v open-stringv os)
                   (<=v fretv string-l)))
	*guitar-tuning* strings-lengths)))
	
	(screamer::attach-noticer!
		#'(lambda ()
	 (when (and (bound? x) (not (bound? fretv)) (not (bound? open-stringv)))
	  (setq valx (apply-substitution x))
	   
	   (let ((values (all-values 
		              (solution
		               (let ((midic valx)
		                    (osv (a-member-ofv *guitar-tuning*))
				            (fv (an-integer-betweenv 0 (1- (length *guitar-fret-notes*))))
				            (s-l (om::om- (mapcar #'length (mapcar #'second *guitar-string-notes*)) 1))
				            )
							
			           (assert! (=v midic (+v (*v fv 100) osv)))
  
			           (assert! (apply #'orv (mapcar #'(lambda (os string-l)
			                                            (andv (=v osv os)
			                                            (<=v fv string-l)))
			                                  *guitar-tuning* s-l)))
				
			          (list midic fv osv))
					  
					  (static-ordering #'linear-force)))))
					  
			(assert! (apply #'orv (mapcar #'(lambda (m-f-s)
			          (andv (=v fretv (second m-f-s))
					         (=v open-stringv (third m-f-s))))
					  values)))
		)
	   )
	  )
	 x)	

  (list x fretv open-stringv)
      ))
	  
(defun guitar-playable-chord (vars)
 (let* ((note-fret-stringv (mapcar #'a-fret-and-stringv vars))                           
        (fretsv (mapcar #'second note-fret-stringv))
		(stringsv (mapcar #'third note-fret-stringv))	
		(pressed-fretsv (remove-open-stringsv fretsv)) 
		(sorted-pfv (quick-sortv pressed-fretsv))
		(dist-sort-pfv (?::mapcarv #'(lambda (x y) (-v y x)) sorted-pfv (?::cdrv sorted-pfv)))          		 
		(min-pressed-fretv (firstv sorted-pfv))
		(open-stringsv (mapcar #'(lambda (x) (?::ifv (=v (second x) 0) (third x))) note-fret-stringv))
	    (not-open-stringsv (mapcar #'(lambda (x) (?::ifv (>v (second x) 0) (third x))) note-fret-stringv))
		(length-pfv (lengthv pressed-fretsv))
		(n-open-stringsv (count-occurrencesv 0 fretsv)) 
		(n-min-pressed-fretv (?::ifv (equalv n-open-stringsv 6) 0 (count-occurrencesv min-pressed-fretv fretsv))) 
		)
	
 (if (not (= (length note-fret-stringv) 1))
     (assert! (apply '/=v stringsv)))
	  ;NO REPEATED GUITAR STRING	
	 
  	  (fret-prime-memberv dist-sort-pfv) 
	  ;SORTED PRESSED FRETS MUST BE MEMBERS OF FRET-PRIME-FORMS
	  
	  (cs-stringsv n-min-pressed-fretv n-open-stringsv length-pfv open-stringsv not-open-stringsv)
	  ;CONSTRAINTS FOR BARRE CHORDS AND OPEN STRINGS COMBINATIONS
	   
  (cond ((= (length note-fret-stringv) 2) 
         (guitar-2-note-playable-chord note-fret-stringv)) 
        
 		((member (length note-fret-stringv) '(3 4 5 6))
 		     (guitar-3-4-5-6-note-playable-chord note-fret-stringv))
			 		   					
		(t nil))
  ;EACH PAIR OF FRETS MUST BE SMALLER OR EQUAL THAN *maximum-fret-distance*

 (let ((old-p-variables om::*p-variables*))
  (setf om::*p-variables* 
   	   (om::x-append old-p-variables 
	  	            (list note-fret-stringv
						  fretsv
  					      stringsv
  					      pressed-fretsv 
  					      sorted-pfv
  					      dist-sort-pfv
  					      min-pressed-fretv 
  					      open-stringsv 
  					      not-open-stringsv 
					      length-pfv
  					      n-open-stringsv
  					      n-min-pressed-fretv)))
  )
 )
) 

(defun guitar-posn (vars)
   (let* ((note-fret-stringv (mapcar #'a-fret-and-stringv vars))                 
          (fretsv (mapcar #'second note-fret-stringv))
  		(stringsv (mapcar #'third note-fret-stringv))		
  		(pressed-fretsv (remove-open-stringsv fretsv))
  		(sorted-pfv (quick-sortv pressed-fretsv))
  		(dist-sort-pfv (?::mapcarv #'(lambda (x y) (-v y x)) sorted-pfv (?::cdrv sorted-pfv)))       			
  		(min-pressed-fretv (firstv sorted-pfv))
  		(open-stringsv (mapcar #'(lambda (x) (?::ifv (=v (second x) 0) (third x))) note-fret-stringv))
  	    (not-open-stringsv (mapcar #'(lambda (x) (?::ifv (>v (second x) 0) (third x))) note-fret-stringv))
  		(length-pfv (lengthv pressed-fretsv))
  		(n-open-stringsv (apply #'count-truesv (mapcar #'(lambda (fretv)(=v fretv 0)) fretsv)))
  		(n-min-pressed-fretv (?::ifv (equalv n-open-stringsv 6) 0 (apply #'count-truesv (mapcar #'(lambda (fretv)(=v fretv min-pressed-fretv)) fretsv))))
  		)

   (if (not (= (length note-fret-stringv) 1))	 
       (assert! (apply '/=v stringsv)) ; NO REPEATED STRINGS
   )
                         
   (fret-prime-memberv dist-sort-pfv)

   (cs-stringsv n-min-pressed-fretv n-open-stringsv length-pfv open-stringsv not-open-stringsv)
 
       (cond ((= (length note-fret-stringv) 2)
              (guitar-2-note-playable-chord note-fret-stringv))


     	  ((member (length note-fret-stringv) '(3 4 5 6))
           (guitar-3-4-5-6-note-playable-chord note-fret-stringv))
			 		   					
         (t nil))

(reset-guitar-tablature)

(let ((solutions (all-values 
  	            (solution 
  			     (list note-fret-stringv  					   
  					   fretsv
  					   stringsv
  					   pressed-fretsv 
  					   sorted-pfv
  					   dist-sort-pfv
                       min-pressed-fretv
                       open-stringsv
                       not-open-stringsv
                       length-pfv
                       n-open-stringsv
                       n-min-pressed-fretv
                       ) 
  					   (static-ordering #'linear-force)))))
					   

(if solutions  
    (let ((nfs (mapcar #'first solutions)))
     (mapcar #'make-tablature nfs)
     nfs
      (open-guitar-tab)
      )
          
    nil))))

;;; END OF CONSTRAINTS	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
;;; OM INTERFACE

(in-package :om)

(defmethod! guitar-playable-chord ((vars list))		  
    :initvals '(nil)
    :indoc '("number, variable or list") 
    :doc "Constraint a list of notes (in midicents) to be playable on the guitar." 
    :icon 487 
 (om?::guitar-playable-chord vars))
 
 (defmethod! guitar-fret-string->mc ((fret-string list) &optional vars)		  
     :initvals '(((0 1)) nil)
     :indoc '("list or list-of-lists" "variables") 
     :doc "Return the note (in midicent) that corresponds to the the fret and string on the guitar. If the optional argument is supplied,
	 constraints a pitch variable (or list of variables) to match the guitar fret and string." 
     :icon 487
  (if vars
   (if (atom vars)
        (s::assert! (s::=v vars (om?::get-note-from-fret-string fret-string)))
   (if (and (listp vars) (every #'atom vars))
	   (mapcar #'(lambda (var fs)
	              (s::assert! (s::=v var (om?::get-note-from-fret-string fs))))
		vars fret-string)
    (if (and (listp vars) (every #'listp vars))
 	   (mapcar #'guitar-fret-string->mc fret-string vars))))
(if (and (listp fret-string) (every #'atom fret-string))			
  (om::list! (om?::get-note-from-fret-string fret-string))
  (mapcar #'om?::get-note-from-fret-string fret-string))))

(defmethod! guitar-setup ((n-strings integer) (tuning t) (max-fret-dist integer) &optional open-strings frets-list)		  
    :initvals '(6 :standard 4 (6400 5900 5500 5000 4500 4000) (12 12 12 12 12 12))
    :indoc '("integer" "symbol" "integer" "list" "list") 
    :doc 
"Setup for guitar parameters used in the search:
	<input0> number of strings
	<input1> tuning (:standard or :scordatura)
	<input2> maximum fret distance
	[Optional arguments]
	<input3> list with each note (in midicents) of open strings, starting on string 1. Should be used only with :scordatura as second argument.
	<input4> list of numbers of frets by string, starting on string 1. The length of the list should be the same as the number of strings.	
	" 
	:menuins '((1 (("standard" :standard) ("scordatura" :scordatura)))) 
    :icon 487
 (om?::guitar-setup n-strings tuning max-fret-dist open-strings frets-list t))

;;; END OF OM INTERFACE	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	