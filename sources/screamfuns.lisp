(in-package :s)

(setf *compte-bt* 0)

;(defun fail ()
;  (setf *compte-bt* (1+ *compte-bt*))
;  (throw 'fail nil))

(defun l-and (l)
  (cond ((null l) t)
        ((equal (car l) nil) nil)
        (t (l-and (cdr l)))))

(defun list-of-members-of (n dom)
  (if (zerop n) nil
      (cons (a-member-of dom)
            (list-of-members-of (1- n) dom))))


(defun list-of-integers-between (n low high)
  (if (zerop n) nil
      (cons (an-integer-between low high)
            (list-of-integers-between (1- n) low high))))

(defun cont-chord (l)
  (cond ((null l) nil)
        ((null (cdr l)) l)
        ((> (car l) (cadr l)) (fail))
        ((member (car l) (cdr l)) (fail))
        (t l)))

(defun a-chord-in (n dom &optional prov)
  (if (zerop n) prov
      (a-chord-in (1- n) dom
                  (cont-chord 
                   (cons (a-member-of dom)
                         prov)))))

(defun and-aux (conts var)
  (cond ((null conts) t)
        ((not (apply (car conts) (list var))) nil)
        (t (and (apply (car conts) (list var)) 
                (and-aux (cdr conts) var)))))

(defun list-of-chords-in (l dom &optional cont)
  (cond ((null cont) (list-of-chords-in-simple l dom))
        ((functionp cont) (list-of-chords-in-cont l dom cont))
        ((listp cont) (list-of-chords-in-lcont l dom cont))
        (t (print `(La contrainte doit etre function, nil ou list)))))

(defun list-of-chords-in-simple (l dom)
  (if (null l) nil
      (cons (a-chord-in (car l) dom) 
            (list-of-chords-in (cdr l) dom))))

(defun list-of-chords-in-cont (l dom cont)
  (if (null l) nil
      (cons (apply 
             (lambda (var)
               (if (apply cont (list var))
                 var
                 (s::fail)))
             (list (a-chord-in (car l) dom)))
            (list-of-chords-in-cont (cdr l) dom cont))))

(defun list-of-chords-in-lcont (l dom conts)
  (if (null l) nil
      (cons (apply 
             (lambda (var)
               (if (and-aux conts var)
                 var
                 (s::fail)))
             (list (a-chord-in (car l) dom)))
            (list-of-chords-in-lcont (cdr l) dom conts))))



(in-package :om)

(defmethod get-real-funname ((self t)) self)

(defmethod get-boxcallclass-fun ((self (eql 'an-integer-between))) 'screamerboxes)
(defmethod! an-integer-between  ((low integer) (high integer))
   :initvals '(0 10) :indoc '("low value" "high value")
   :doc "Defines a Screamer variable, in the interval [low high]. 
Without constraints, an-integer-between enumerates all the integers between low and high.

Inputs :
low : integer, minimum of the possible values for the variable
high : integer, maximum of the possible values for the variable


Output:
an integer between low and high. The value depends on the backtracking caused by the constraints
" 
   :icon 486 
   (s:an-integer-between low high))

(defmethod get-boxcallclass-fun ((self (eql 'a-member-of))) 'screamerboxes)
(defmethod! a-member-of  ((list list))
   :initvals '(a b c d) :indoc '("list of possible values")
   :doc "Defines a Screamer variable, in the list of values
Without constraints, an-member-of enumerates all the values of the list.

Inputs :
list : list of possible values

Output:
a value of the list. The value depends on the backtracking caused by the constraints
"
   :icon 486
   (s:a-member-of list))

(defun appc (fun variables)
  (apply 
   (lambda (var)
     (if (apply fun (list var))
       var
       (s::fail)))
   (list variables))
  variables)

(defmethod!  apply-cont ((fun function) var)  
  :indoc '("Constraint in lambda-mode" "Variables") 
  :icon 486
  :doc "Applies the constraint (patch in lambda-mode) to the variables

Inputs :

fun : an anonymous function, with outputs t or nil (ie, a predicate)
      or a list of anonymous functions
var : variables defined with Screamer primitives, like an-integer-between, a-member-of, etc.
The type of the predicate's inputs must be the same as the variables'.

Output:

If the predicate is true (if they are all true in case of a list) on the variables, the variables. 
Else, apply-cont causes backtrack.
"
  (appc fun var))

(defmethod!  apply-cont ((funs list) var)  
  :indoc '("List of constraints in lambda-mode" "Variables") :icon 486
  (loop for fun in funs
        do
        (appc fun var))
  var)

(defmethod get-boxcallclass-fun ((self (eql 'list-of-members-of))) 'screamerboxes)
(defmethod get-real-funname ((self (eql 'list-of-members-of))) self)
(defmethod! list-of-members-of  ((n integer) (dom list))
  :initvals '(2 '(1 2 3))
  :indoc '("number of variables" "list of values")
  :doc "Defines a list of Screamer variables.
Each variable is a member of dom. 

Inputs :
n : length of the list
dom : domain for each variable

Output : a list of n variables in dom. 
The value depends on the backtracking caused by the constraints
" 
  :icon 486 
  (s::list-of-members-of n dom))

(defmethod get-boxcallclass-fun ((self (eql 'list-of-integers-between))) 'screamerboxes)
(defmethod get-real-funname ((self (eql 'list-of-integers-between))) self)
(defmethod! list-of-integers-between  ((n integer) (low integer) (high integer))
  :initvals '(2 0 10)
  :indoc '("number of variables" "low" "high")
  :doc "Defines a list of Screamer variables. 
Each variable is an integer between low and high. 

Inputs :
n : length of the list
low : integer, minimum of the possible values for each variable
high : integer, maximum of the possible values for each variable

Output : a list of n integer variables between low and high. 
The value depends on the backtracking caused by the constraints" 
  :icon 486 
  (s::list-of-integers-between n low high))

(defmethod get-boxcallclass-fun ((self (eql 'a-chord-in))) 'screamerboxes)
(defmethod get-real-funname ((self (eql 'a-chord-in))) self)
(defmethod! a-chord-in  ((n integer) (dom list))
   :initvals '(5 '(6000 6200 6400 6600 6800 7000))
   :indoc '("number of variables" "list of values")
   :doc "Defines a list of Screamer variables.
Each variable is a member of dom. a-chord-in contains two predefined constraints, they state that the values in the list are all different (you can't get (1 1 1)), and the list is sorted (you can't get (2 1 3)). 
So the output looks more like a set, which is usefull for chords definition.

Inputs :
n : length of the list
dom : domain for each variable

Output : a list of n sorted and all different variables in dom. 
The value depends on the backtracking caused by the constraints
" 
   :icon 486 
   (s::a-chord-in n dom))

(defmethod get-boxcallclass-fun ((self (eql 'list-of-chords-in))) 'screamerboxes)
(defmethod get-real-funname ((self (eql 'list-of-chords-in))) self)
(defmethod! list-of-chords-in  ((l list) (dom list) &optional prov)
  :initvals '((4 3 2) '(6000 6200 6400 6600 6800 7000 7200))
  :indoc '("number of variables" "domains" "constraints on variables")
  :doc "Defines a list of a-chord-in, ie a list of all-different and sorted lists of Screamer variables. 
Each variable is a member of dom. 

The optional input allows to state constraints on the intermediate lists. 
For instance, when list-of-chords-in is used to define a chordseq,
the constraints state on the chords.

Inputs :
l : length of the intermediate lists. For instance, (1 2 3) will give something like ((0) (0 0) (0 0 0)).
dom : domain, ie possible values for the variables
cont : an anonymous function, outputs nil or t (ie a predicate)
       or a list of such anonymous functions
       The type of the cont's input must be the same as the variables'.

Output : a list of lists of all different, sorted variables in dom. 
The value depends on the backtracking caused by the constraints" 
  :icon 486 
  (s::list-of-chords-in l dom prov))


(defun alldiff2 (l)
  (cond ((null l) t)
        ((member (car l) (cdr l) :test #'equal) nil)
        (t (alldiff2 (cdr l)))))

(defmethod! alldiff? (l)
  :initvals 'nil
  :indoc '("a list")
  :doc "True iff all the values of the list are different
Input : a list

Output : nil if the list contains the same value twice
         t if all the values in the list are differents.
" 
  :icon 486 
  (alldiff2 l))

(defun croissante (l)
  (cond ((null (cdr l)) t)
        ((>= (car l) (cadr l)) nil)
        (t (croissante (cdr l)))))

(defmethod! growing? (l)
  :initvals 'nil
  :indoc '("a list")
  :doc "True iff all the list is growing
Input : a list

Output : t if the list is growing
         nil otherwise
" 
  :icon 486 
  (croissante l))

;(setf *screamer-package* (omNG-protect-object (omNG-make-new-package "Backtrack")))

;(AddPackage2Pack *screamer-package* *constraint-package*)

;(AddGenFun2Pack '(an-integer-between  a-member-of  apply-cont list-of-members-of list-of-integers-between  a-chord-in  list-of-chords-in  alldiff? growing? ) *screamer-package*)
