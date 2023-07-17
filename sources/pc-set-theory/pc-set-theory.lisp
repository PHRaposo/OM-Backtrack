;===============================================
;===============================================

;;; FROM

;;; PWConstraints by Mikael Laurson (c), 1995
	
;;; AND OMCS 1.4

;===============================================
;===============================================
(in-package :om?)
;===============================================

(defparameter *SC-data-file* (namestring
                              (make-pathname :directory (pathname-directory *load-pathname*)
                                             :name "SCs-data.lisp")))


(defvar *all-possible-chroma-subsets-hash* (make-hash-table :test #'equalv))

(defun fill-possible-chroma-subsets-hash ()
  (let (sets-list) 
    (with-open-file (in *SC-data-file* :direction :input)
      (setq sets-list (eval (read in))))
    ;(in-package :cl-user)
    (clrhash *all-possible-chroma-subsets-hash*)
    (dolist (set sets-list) 
      (setf (gethash (first set) *all-possible-chroma-subsets-hash*)  
            (second set)))
    (dolist (set sets-list) 
      (when (zerop (second (second set)))
      (setf (get  (first (second set)) :prime) (first set))))))  

(fill-possible-chroma-subsets-hash)

;===============================================
;===============================================

;;; OM-SCREAMER FUNCTIONS

;;; by Paulo Raposo

;===============================================

(defun fn (sc) ;;; RESEARCH: N-ORDV => REMOVE-DUPLICATESV
(om?-symb->om (car (gethash (sort  (remove-duplicates sc) #'<) *all-possible-chroma-subsets-hash*))))

(defun prime (fn)
(get (om-symb->om? fn) :prime))

(defun midics->fn (list) 
 (let ((pcs (mapcar #'(lambda (x) (/v (modv x 1200) 100)) list)))
 (fnv pcs)))

(defun fnv (vars)
 (carv (s::funcallgv #'gethash (funcallv #'sort (?::members-ofv vars) #'<) *all-possible-chroma-subsets-hash*)))
 
(defun pcv? (vars fn)
 (equalv (fnv vars) (om-symb->om? fn)))
 
(defun all-subsets (fn card-min card-max forbid)
 (let* ((prime (prime fn))
         (subsets (remove nil (all-values 
                         (apply (lambda (x) 
                                     (if (and (>= (length x) card-min) 
                                                 (<= (length x) card-max))
                                         x
                                        (fail)))
                         (list (?::a-subset-of prime)))))))
(remove-if #'(lambda (item) (member item forbid)) 
                   (remove-duplicates 
                    (mapcar #'fnv subsets)))))

(defun subv? (vars fn card-min card-max forbid)
 (memberv (fnv vars) 
                  (all-subsets (om-symb->om? fn)
                                     (if card-min card-min 1)
                                     (if card-max card-max (digit-char-p (aref (symbol-name fn) 0)))
                                     (if forbid forbid '(nil) ))))

(defun member-of-setclassv (vars list)
 (memberv (fnv vars) (om-symb->om? list)))
 
(defun a-set-complement-ofv (list)
 (let ((v (list-of-integers-betweenv (-v 12 (length list)) 0 11)))
  (assert! (notv (intersectionv v list)))
  (assert! (apply #'<v v))
  (assert!-all-differentv v)
 (value-of v)))
 
(defun set-complement (pcs)
 (one-value (solution (a-set-complement-ofv pcs) (static-ordering #'linear-force))))

(defun supersets (SC card)
 (let* ((prime (prime SC))
        (set-diff (set-complement prime))
        (s-space (list-of-members-ofv (- card (card SC)) set-diff)))
(assert! (apply #'<v s-space))
(assert!-all-differentv s-space)
  (remove-duplicates
   (mapcar #'(lambda (l) (fn (append prime l)))
    (all-values (solution s-space (static-ordering #'linear-force)))))))

(defun list-of-intervals-mod12v (list)
 (mapcarv (lambda (x y)
  (an-integer-modv (-v y x) 12)) (cdr list) list))  

(defun pc-set-transpositions (prime-form)
 (let ((v (list-of-integers-betweenv (length prime-form) 0 11)))
(assert! (equalv (list-of-intervals-mod12v v)
                          (list-of-intervals-mod12v prime-form)))
(all-values (solution v (static-ordering #'linear-force)))))
                                                  
; =============================================================== ;
   ;;; CONVERSION: OM SYMBOL <-> OM? SYMBOL
; =============================================================== ;

(defun om?-symb->om (om?-sym)
   (let ( (write-symbol (lambda (x)  (car (om::list! (intern (string x) :om))))))
    (if (atom om?-sym)
         (funcall write-symbol om?-sym)
    (mapcar write-symbol om?-sym)))) 

(defun om-symb->om? (om-sym)
   (let ( (read-symbol (lambda (x) (car (om::list! (intern (string x) :om?))))))
    (if (atom om-sym)
         (funcall read-symbol om-sym)
    (mapcar read-symbol om-sym)))) 

; =============================================================== ;

;==============
(defun calc-6vect (SC)
  (let ((res (make-list 6 :initial-element 0))
        (prime (prime SC))
        temp int ref)
    (om::while (cdr prime)
      (setq ref (pop prime))
      (setq temp prime)
      (om::while temp
        (setq int (- (first temp) ref))
        (om::when (> int 6) (setq int (- 12 int)))
        (setf (nth (1- int) res) (1+ (nth (1- int) res)))
        (pop temp)))
    res))
	
(defun store-SC-icvectors ()
  (dolist (SCs *all-SC-names*)
    (dolist (SC SCs)
      (setf (get SC :icv) (calc-6vect SC)))))

(store-SC-icvectors)
;==============
(defun card (SC)
"returns the cardinality of SC"
  (length (prime SC)))

;(time (repeat 10000 (card '12-1)))

(defun ICV (SC)
"returns the interval-class vector (ICV) of SC"
  (get (om-symb->om? SC) :icv))

(defun make-set (l)
  (let (lst)
    (om::while l (push (/ (mod (pop l) 1200) 100) lst))
    (sort (delete-duplicates  lst) #'<)))


; =============================================================== ;  
;DEPRECATED FUNCTIONS
#|

(defun pcpv? (list prime-form)
 (let ((pc-sets-list (pc-set-transpositions prime-form)))
(pcpv?-internal list pc-sets-list)))

(defun pcpv?-internal (list pc-sets-list)
 (when pc-sets-list 
  (ifv (andv ;(set-equalv (?::members-ofv list) (carv pc-sets-list)) ;;; SLOW
                   (all-membersv list (carv pc-sets-list))                  
                   (=v (lengthv (intersectionv (carv pc-sets-list) list))
                         (lengthv (carv pc-sets-list))))
             t
            (pcpv?-internal list (cdrv pc-sets-list)))))

(defun a-transposition-ofv (pcset)
(let ((listv (a-listv)))
  (make-equal listv 
   (mapcar #'(lambda (x) (an-integer-modv x 12))
    (om+v (an-integer-betweenv 0 11) pcset)))
(value-of listv)))

(defun assert!-pcs (list pcset)
(assert! (set-equalv (?::members-ofv list) pcset)))

(defun assert!-pc-setv (list pcset) ;;;SLOW
  (assert! (apply #'orv 
                (mapcar #'(lambda (x) (set-equalv (?::members-ofv list) x)) 
                (pc-set-transpositions pcset)))))
                                         
(defun subsetpv? (list prime-form card)
 (let ((pc-sets-list (pc-set-transpositions prime-form)))
  (subsetpv?-internal list pc-sets-list card)))

(defun subsetpv?-internal (list pc-sets-list card)
 (when pc-sets-list 
  (ifv (andv (all-membersv list (carv pc-sets-list))
                      (>=v (lengthv (intersectionv (carv pc-sets-list) list))
                               card))
            t
           (subsetpv?-internal list (cdrv pc-sets-list) card))))
|#
;===============================================
;;; OM METHODS 
;===============================================

(om::defmethod! member-of-scv? ((vars t) (sc-list list) (mode string))  
  :initvals '((nil) (om::3-11a om::3-11b) "pcs") 
:indoc '("list of screamer variables" "list of set-classes<fn>" "pcs or midics") 
  :doc "Constraint a list of Screamer variables to be all members of a list of set-classes in Forte notation."
:menuins '((2 (("pcs" "pcs") ("midics" "midics"))))
    :icon 487
(if (equal mode "midics")
    (member-of-setclassv (mc->pcv vars) sc-list)
    (member-of-setclassv vars sc-list)))

(om::defmethod! pc-setpv? ((vars t) (pc-set list))  
  :initvals '((nil) (0 4 7)) 
:indoc '("list of screamer variables => (integers-betweenv 0 11)" "fn or integers") 
  :doc "Constraint a list of Screamer variables <integers-between 0 11> to be all members of the selected pc-set <list of integers>."
    :icon 487
  (pcv? vars (fn pc-set))) 
 ;(pcpv? vars pc-set))

(om::defmethod! pc-setpv? ((vars t) (pc-set symbol))  
  :initvals '((nil) 'om::|3-11B|) 
:indoc '("list of screamer variables => (integers-betweenv 0 11)" "fn or integers") 
  :doc "Constraint a list of Screamer variables <integers-between 0 11> to be all members of the selected pc-set <list of integers>."
    :icon 487
  (pcv? vars pc-set)) 
 ;(pcpv? vars pc-set))

(om::defmethod! sub-setpv? ((vars t) (pc-set list) &optional (card-min nil) (card-max nil) (forbid nil))  
  :initvals '((nil) (0 1 3 4 6 9) nil nil nil) 
:indoc '("list of screamer variables => (integers-betweenv 0 11)" "fn or integers" "integer" "integer" "list of fns") 
  :doc "Constraint a list of Screamer variables <integers-between 0 11> to be all subsets of the selected pc-set.
Optional arguments: 
 <CARD-MIN> minimun cardinality.
 <CARD-MAX> maximun cardinality.
 <FORBID> list of set classes in Forte notation [ex.: (3-11a 3-11b 3-4b)]."
    :icon 487
(subv? vars (fn pc-set) card-min card-max (om-symb->om? forbid)))   
;(subsetpv? vars pc-set card))

(om::defmethod! sub-setpv? ((vars t) (pc-set symbol) &optional (card-min nil) (card-max nil) (forbid nil))  
  :initvals '((nil) 'om::|6-27A| nil nil nil) 
:indoc '("list of screamer variables => (integers-betweenv 0 11)" "fn or integers" "integer" "integer" "list of fns") 
  :doc "Constraint a list of Screamer variables <integers-between 0 11> to be all subsets of the selected pc-set.
Optional arguments: 
 <CARD-MIN> minimun cardinality.
 <CARD-MAX> maximun cardinality.
 <FORBID> list of set classes in Forte notation."
    :icon 487
(subv? vars pc-set card-min card-max (om-symb->om? forbid)))   
;(subsetpv? vars pc-set card))

(om::defmethod! SC-subsets ((fn symbol) &optional (card-min nil) (card-max nil))
  :initvals '('om::|6-27A| nil nil)
:indoc '("fn symbol" "integer" "integer") 
  :doc "Return all subsets."
    :icon 487
(om?-symb->om  (all-subsets (om-symb->om? fn)
                                     (if card-min card-min 1)
                                     (if card-max card-max (digit-char-p (aref (symbol-name fn) 0)))
                                     '(nil))))

(om::defmethod! SCs-card ((card integer))
  :initvals '(6)
:indoc '("integer" ) 
:menuins '((0 (("1" 1)  ("2" 2) ("3" 3) ("4" 4) ("5" 5) ("6" 6) ("7" 7) ("8" 8) ("9" 9) ("10" 10) ("11" 11) ("12" 12))))
  :doc "Return all fn symbols."
    :icon 487
(om?-symb->om
 (case card 
 (1 card1)
 (2 card2)
 (3 card3 )
 (4 card4 )
 (5 card5 )
 (6 card6 )
 (7 card7 )
 (8 card8 )
 (9 card9 )
 (10 card10 )
 (11 card11 )
 (12 card12))))

(om::defmethod! SC+off ((midics list)) 
  :initvals '((6000 6100))
  :indoc '("midics")
  :icon 487
  :doc  "returns a list containing the SC-name and the offset 
(i.e. the transposition relative to the prime form of the SC) of 
 midis (a list of midic-values), midics can also be a list of lists 
 of midics in which case SC+off returns the SCs with offsets 
 for each midic-value sublist."
  (if (atom (car midics))
      (let ((res (gethash (make-set midics) *all-possible-chroma-subsets-hash*)))
         (om::x-append (om?-symb->om (first res)) (second res)))
    (let (res)
      (dolist (midics-l midics)
        (push (gethash  (make-set midics-l) *all-possible-chroma-subsets-hash*) res))
      (mapcar #'(lambda (x)
       (om::x-append (om?-symb->om (first x)) (second x))) (nreverse res)))))

(om::defmethod! SC-name ((midics list)) 
  :initvals '((6000 6100))
  :indoc '("midics")
  :icon 487
  :doc  "returns a list containing the SC-name and the offset 
(i.e. the transposition relative to the prime form of the SC) of 
 midis (a list of midic-values), midics can also be a list of lists 
 of midics in which case SC+off returns the SCs with offsets 
 for each midic-value sublist."
  (if (atom (car midics))
      (om?-symb->om (car (gethash (make-set midics) *all-possible-chroma-subsets-hash*)))
    (let (res)
      (dolist (midics-l midics)
        (push (car (gethash  (make-set midics-l) *all-possible-chroma-subsets-hash*)) res))
      (mapcar #'om?-symb->om (nreverse res)))))

(om::defmethod! sub/supersets ((SC t) (card number))
  :initvals '('om::4-z15a 9)
  :indoc '("SC" "card")
  :icon 487
  :doc "returns all subset classes of SC (when card is less than the cardinality of SC)
or superset classes (when card is greater than the cardinality of SC) 
of cardinality card."
  (if (= (card (om-symb->om? SC)) card)
    SC
    (if (> (card (om-symb->om? SC)) card)
      (om?-symb->om (all-subsets (om-symb->om? SC) card card nil))
      (supersets (om-symb->om? SC) card))))

(om::defmethod! SC-info ((mode symbol) (SC om::t))
  :initvals '(:prime 'om::4-z15a)
:indoc '("mode" "SC" ) 
:menuins '((0 (("prime" :prime) ("icv" :icv)  ("member-sets" :member-sets) ("complement-pcs" :complement-pcs))))
  :doc "Returns the selected info (prime-form, interval class vector, members-sets or complement) about an SC."
    :icon 487
(cond 
((equal mode :prime) (prime SC))
((equal mode :icv) (icv SC))
((equal mode :member-sets) (pc-set-transpositions (prime SC)))
((equal mode :complement-pcs) (set-complement  (prime SC)))
(t (progn (om::om-message-dialog "Please select a valid mode (:prime, :icv, :member-sets or :complement-pcs).") (om::om-abort)))))

