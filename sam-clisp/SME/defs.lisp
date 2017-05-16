;;; -*- Mode: LISP; Syntax: Common-lisp; Package: SME; Base: 10; Fonts: CPTFONT,TR12I -*-
#-Symbolics
(in-package "SME" :use (list sme::*the-lisp-package*))

;;;; Structure-Mapping Engine  (module defs.lisp: Global Vars, Structure Defs, and Macros)

;;; Copyright (C) 1986,  Brian C. Falkenhainer, University of Illinois
;;; All rights reserved

(Export '(defEntity define-Entity
          defPredicate define-Predicate *user-predicate-init*
          defDescription define-description
	  function? attribute? relation? taxonomy? logical?
	  commutative? n-ary? arg-list numargs expression-type
	  predicate? fetch-predicate-definition predicate-type?
	  expression-functor expression-arguments expression?
	  entity? entity-name? fetch-entity-definition constant-entity? entity-name))

;;; Global Variables

(defvar *sme-parameters* nil "simple list of parameter variables")
(defvar *parameter-menu-options* nil
    "These are the SME system parameter options.  May be added to through defSME-Parameter.")
(defvar *system-utilities-menu* nil
    "These are the SME system utilities. They can be added to through defSME-Utility.")
(defvar *sme-operations-menu* nil
    "These are the main SME operations. They can be added to through defSME-Operation.")

(defvar *sme-predicates* nil "Simple list of defined SME predicates")
(defvar *user-predicate-init* nil
     "Called each time a predicate defined (if given). Takes pred struct as sole arg.")

(defvar *description-groups* nil
     "Simple list of descriptions (names) known about")  ;menu aid
(defvar *description-hash-table* nil "Store dgroups")
(defvar *description-table-size* 2341 "Sure seems like a lot of dgroups, don't it")

(defvar *debug-sme*          nil "simple debug flag")
(defvar *sme-knowledge-flag* nil "trace flag for all knowledge definition routines")

(defvar *initialized?* nil)
(defvar *windowing-active?* nil)

   ;; Initialize the parameter hooks
(setq *sme-parameters* '(*debug-sme* *sme-knowledge-flag* *sme-language-file*
			 *sme-dgroup-pathname* *sme-rules-pathname*))
(setq *parameter-menu-options*
      '((*debug-sme* "SME Debug Flag" :boolean)
	(*sme-knowledge-flag* "Indicate All Changes to the Knowledge Base" :boolean)
	(*sme-language-file* "Default SME Language Definitions File" :expression)
	(*sme-dgroup-pathname* "Default SME Dgroup Pathname" :expression)
	(*sme-rules-pathname* "Default SME Rules Pathname" :expression)))


;;; Defstructs:
;;;
;;;     Dgroup ("Description Group") - represents a packet of expressions
;;;                                    related to a single concept (e.g. solar system...)
;;;     expression  - a single Dgroup expression.
;;;              Each use of a predicate gets its own expression,
;;;              so a higher-order relation gets translated into several
;;;              expressions, with some having expressions as arguments.
;;;     entity-token  - an entity represents the highest level in the
;;;              object hierarchy.  It may be a physical object or some
;;;              form of numerical constant (ordinal, number). 
;;;              Entities are definied using defEntity.
;;;     sme-predicate  - each predicate has associated with it a set of
;;;              defining information (e.g. argument types, etc.)
;;;

;;; Description Groups
;;;    Need to be able to refer to a group of predicates by name
;;;
(defstruct (Dgroup (:predicate Dgroup?)
		   (:conc-name Dgroup-)
		   (:print-function (lambda (dg s ignore)
				      (format s "DG-<~A>" (Dgroup-name dg)))))
    name		;all description groups must be given a name by the user
    expressions		;the statements forming the description group
    entities		;the primitive entities involved.  <<may be unnecessary>>
    roots		;the I-ain't-got-no-parents expressions
    zgraph-instance	;save Zgraph from making a new instance each time it's displayed
    )


;;; Description expressions
;;;
(defstruct (expression (:predicate expression?)
		       (:conc-name expression-)
		       (:print-function (lambda (exp s ignore)
					  (format s "Exp#~A" (expression-name exp)))))
  functor		;the functor this expression uses
  sys-lisp-form		;the lisp form of the expression
  original-lisp-form	;the user-supplied lisp form <may be different>
  name			;the name of the expression <user-given or gensym'd>
  dgroup		;the description group it is a sub-part of
  order			;the order it possesses in the over-all structure
  arguments		;the arguments to this expression (expressions or entities)
  parents		;list of expressions which contain this expression as an argument
  roots			;list of its structural roots (identifies common structure among exps)
  mark			;possibly needed mark field
  )


(defmacro expression-name? (symbol dgroup)
  `(when (symbolp ,symbol)
     (if (dgroup? ,dgroup)
	 (get ,symbol (dgroup-name ,dgroup))
	 (get ,symbol ,dgroup))))


;;; Entities
;;;   All tokens in the representation language must be declared through
;;;   the defEntity form. A token may denote an object(s) (e.g., elephant1)
;;;   or a constant (e.g., zero)
;;;
(defstruct (entity-token (:predicate entity?)
			 (:conc-name entity-)
			 (:print-function (lambda (e s ignore)
					    (format s "E#~A" (entity-name e)))))
  name
  domain-type		;the entity's domain - e.g. {ordinal, linear, person, physob...}
  constant?		;some entity's are constants, others are some form of object
  plist			;used mostly as a-list ((dgroup . parent-expressions) ...)
  )


;;; Predicates
;;;
(defstruct (sme-predicate
		  (:predicate predicate-definition?)
		  (:conc-name sme-predicate-)
		  (:print-function (lambda (p s ignore)
				     (format s "P#~A" (sme-predicate-name p)))))
  name
  type			;the type of predicate - {:relation, :function, :attribute}
  expression-type	;type an expression using this predicate represents {event, linear ..}
  numargs		;length of arg-list
  arg-list		;a-list of predicate's arguments ((arg-name arg-type) ...) -
			;  an entry may be symbol
  equivalent		;equivalent sentential-forms
  commutative?		;
  n-ary?		;takes any number of arguments?
  relgroup?		;the predicate defines a ``relational group"
  plist			;miscellaneous stuff, such as a function's eval form
  )


;;;; Declaring entities, (domain) types, and predicates

;;; Declare a (global) entity type (person, place, thing, ...)
;;;
(defmacro defEntity (name &key type constant?)
  `(define-Entity ',name :type ',type :constant? ',constant?))

(defun define-Entity (name &key type constant?)
  (and *sme-knowledge-flag*
       (format t "~%Defining entity ~A :type ~A :constant?" name type constant?))
  (setf (get name :entity)
	(make-entity-token :name name
			   :domain-type type
			   :constant? constant?)))


(defmacro entity-name? (symbol)
  `(if (numberp ,symbol)
       ,symbol
       (get ,symbol :entity)))

(defmacro fetch-entity-definition (symbol)
  `(get ,symbol :entity))

(defmacro constant-entity? (symbol)
  `(or (numberp ,symbol)
       (let ((e-defn (sme::fetch-entity-definition ,symbol)))
	 (if e-defn (sme::entity-constant? e-defn)))))

(defmacro entity-domain (symbol)
  `(cond ((numberp ,symbol) :numberp)
	 ((symbolp ,symbol)
	  (let ((e-defn (sme::fetch-entity-definition ,symbol)))
	    (if e-defn (sme::entity-domain-type e-defn))))
	 (:not-symbol)))

;;; Locate where the entity structure appears in a particular dgroup
;;;
(defmacro entity-parents (entity dgroup)
  `(getf (sme::entity-plist ,entity) (sme::dgroup-name ,dgroup)))


;;; Declare a predicate.
;;;    A predicate may be 1 of 4 types: {relation, attribute, logical, function}.
;;;
;;;      (defPredicate name  ((arg1-name arg1-type) (arg2-name arg2-type) ...)  type
;;;                      [:expression-type symbol] [relgroup? bool] [:eval eval-form]
;;;                      [:n-ary? bool] [:commutative? bool])
;;;
;;;  The (arg1-name arg1-type) distinction enables us to
;;;   (1) give case-relation information
;;;   (2) partially name arguments (recall argi-name need not be unique among other args)
;;;
;;; Some of these options are currently unused (e.g., :eval and :equivalent)
;;;
(defmacro defPredicate (name arg-list type
			     &key expression-type n-ary? commutative? relgroup?
			          eval equivalent documentation)
  `(define-Predicate ',name ',arg-list ',type
		            :expression-type ',expression-type
			    :n-ary? ,n-ary?
			    :commutative? ,commutative?
			    :relgroup? ,relgroup?
			    :eval ',eval
			    :equivalent ',equivalent
			    :documentation ,documentation))

(defun define-Predicate (name arg-list type
			      &key expression-type n-ary? commutative? relgroup?
			           eval equivalent documentation)
  (let ((predicate (make-sme-predicate  :name name
					:arg-list arg-list
					:numargs (length arg-list)
					:type (intern (symbol-name type) 'keyword)  ;be aware
					:expression-type expression-type
					:equivalent equivalent
					:commutative? commutative?
					:n-ary? n-ary?)))
    (if relgroup?
	(cond ((and commutative? n-ary?)
	       (setf (sme-predicate-relgroup? predicate) T))
	      (T (format t "~%WARNING: Ignoring relgroup declaration for predicate ~S." name)
		 (format t "~%         It must also be commutative and n-ary."))))
    (setf (getf (sme-predicate-plist predicate) :eval-form) eval)
    (if (and (not (eq (sme-predicate-type predicate) :function)) eval)
	(error "defPredicate <~A>: Invalid key -eval- for non functional predicate" name))
    (or (member (sme-predicate-type predicate)
		'(:relation :attribute :function :logical :taxonomy) :test #'eq)
	(error "defPredicate <~A>: Invalid predicate type given <~A>." name type))
    (cond (documentation (setf (get name :documentation) documentation))
	  ((every #'consp arg-list)
	   (setf (get name :documentation) (format nil "~A" (mapcar #'car arg-list)))))
    (push name *sme-predicates*)
    (setf (get name :sme-predicate) predicate)
    (if expression-type
	(pushnew name (get expression-type :subclasses)))
    (if (and (boundp *user-predicate-init*)
	     *user-predicate-init*
	     (functionp *user-predicate-init*))
	(funcall *user-predicate-init* predicate))))

;;; General predicate query macros

(defmacro predicate? (item)
  `(get ,item :sme-predicate))

(defmacro fetch-predicate-definition (predicate-symbol)
  `(get ,predicate-symbol :sme-predicate))

(defmacro predicate-type (predicate-symbol)
  `(let ((pred (fetch-predicate-definition ,predicate-symbol)))
     (if pred (sme-predicate-type pred))))

(defmacro relation? (predicate-symbol)
  `(let ((pred (fetch-predicate-definition ,predicate-symbol)))
     (if pred (eq (sme-predicate-type pred) :relation))))

(defmacro attribute? (predicate-symbol)
  `(let ((pred (fetch-predicate-definition ,predicate-symbol)))
     (if pred (eq (sme-predicate-type pred) :attribute))))

(defmacro taxonomy? (predicate-symbol)
  `(let ((pred (fetch-predicate-definition ,predicate-symbol)))
     (if pred (eq (sme-predicate-type pred) :taxonomy))))

(defmacro logical? (predicate-symbol)
  `(let ((pred (fetch-predicate-definition ,predicate-symbol)))
     (if pred (eq (sme-predicate-type pred) :logical))))

(defmacro function? (predicate-symbol)
  `(let ((pred (fetch-predicate-definition ,predicate-symbol)))
     (if pred (eq (sme-predicate-type pred) :function))))

(defmacro commutative? (predicate-symbol)
  `(let ((pred (fetch-predicate-definition ,predicate-symbol)))
     (if pred (sme-predicate-commutative? pred))))

(defmacro n-ary? (predicate-symbol)
  `(let ((pred (fetch-predicate-definition ,predicate-symbol)))
     (if pred (sme-predicate-n-ary? pred))))

(defmacro relgroup? (predicate-symbol)
  `(let ((pred (fetch-predicate-definition ,predicate-symbol)))
     (if pred (sme-predicate-relgroup? pred))))

(defmacro arg-list (predicate-symbol)
  `(let ((pred (fetch-predicate-definition ,predicate-symbol)))
     (if pred (sme-predicate-arg-list pred))))

(defmacro numargs (predicate-symbol)
  `(let ((pred (fetch-predicate-definition ,predicate-symbol)))
     (if pred (sme-predicate-numargs pred))))

(defmacro expression-type (function-symbol)
  `(let ((pred (fetch-predicate-definition ,function-symbol)))
     (if pred (sme-predicate-expression-type pred))))

(defmacro subclasses (function-symbol)
  `(get ,function-symbol :subclasses))

(defmacro eval-form (function-symbol)
  `(let ((pred (fetch-predicate-definition ,function-symbol)))
     (if pred (getf (sme-predicate-plist pred) :eval-form))))


;;;; Defining description-groups

;;; The format of a defDescription is:
;;;          (defDescription description-name
;;;                   entities (entity1 entity2 ...)
;;;                   expressions ( (expression1 :name expression1-name)
;;;                                 (expressioni :name expressioni-name) ...))
;;;       (expression1 :name expression1-name) could be given as just "expression1".
;;;
(defmacro defDescription (description-name &rest body)  "returns ptr to description record"
  `(define-description ',description-name
         ',(getf body 'entities)
         ',(getf body 'expressions)))
		    

(defun define-description (description-name entities exps)
  (if (null entities) (format t "~%WARNING: Dgroup ~S is being defined with no entities.~
                                ~% This could be a package problem." description-name))
  (if (null exps) (format t "~%WARNING: Dgroup ~S is being defined with no expressions.~
                                ~% This could be a package problem." description-name))
  (let ((dgroup (fetch-Dgroup description-name T))
	exp-name)
    (setf (dgroup-expressions dgroup)           nil   ;clear, in case it already existed
	  (dgroup-entities dgroup)        nil
	  (dgroup-roots dgroup)           nil
	  (dgroup-zgraph-instance dgroup) nil)
    (dolist (e entities)			;add entities to dgroup
      (cond ((numberp e)
	     (push (make-entity-token :name e     ;special case, still need inst. struc
				      :domain-type :numberp)
		   (dgroup-entities dgroup)))
	    ((entity-name? e)
	     (push (fetch-entity-definition e) (dgroup-entities dgroup)))
	    ((error "defDescription <~A>: Entity (~A) given which was not defined"
		    description-name e))))
    (dolist (f exps)				;add expressions to dgroup
      (when *debug-sme*
	(format t "~%Adding expression <~A> to ~A description" f description-name))
      (if (eq (second f) :name) (setq exp-name (third f)) (setq exp-name nil))
      (expression (if exp-name (car f) f) description-name :expression-name exp-name))
    (identify-structure dgroup)
    dgroup))



;;;; Miscellaneous macro support items

;;; Hook for adding new SME system parameters throughout the code
;;;
(defmacro defSME-Parameter (variable-name string-description type &optional type-choices)
  `(let ((dec (assoc ',variable-name sme::*parameter-menu-options* :test #'eq))
	 (rest-dec (if ',type-choices
		       (list ,string-description ',type ',type-choices)
		       (list ,string-description ',type))))
     (cond (dec (setf (cdr dec) rest-dec))
	   (t (push (cons ',variable-name rest-dec) sme::*parameter-menu-options*)
	      (push ',variable-name sme::*sme-parameters*)))))


;;; Hook for adding new SME utilities
(defmacro defSME-Utility (string-name function)
  `(pushnew '(,string-name :eval ,function) sme::*system-utilities-menu* :test #'equal))

;;; Hook for adding new SME operations
(defmacro defSME-Operation (string-name function)
  `(pushnew '(,string-name :eval ,function) sme::*sme-operations-menu* :test #'equal))


;;; Fully-expand an expression form: in the given expression form, recursively replace
;;;   all expression name tokens with their actual form
;;;
(defun fully-expand-expression (exp dgroup)
  (fully-expand-expression-form (expression-original-lisp-form exp) dgroup))


(defun fully-expand-expression-form (exp-form dgroup)
  (labels ((fully-expand (exp-form)
	     (cond ((null exp-form) nil)
		   ((symbolp exp-form)
		    (cond ((expression-name? exp-form dgroup)
			   (fully-expand
			     (expression-original-lisp-form
			       (fetch-expression exp-form dgroup))))
			  ((entity? exp-form)
			   (entity-name exp-form))
			  (T exp-form)))
		   ((mapcar #'fully-expand exp-form)))))
    (if (atom exp-form)
	(fully-expand exp-form)
	(mapcar #'fully-expand exp-form))))
    

;;; Common-lisp's copy-list is really SLOW!
;;;
(defun fcopy (l)
  (if (null l) nil
      (let ((head (cons (car l) nil)))
	(do ((list (cdr l) (cdr list))
	     (tail head (cdr tail)))
	    ((null list) head)
	  (setf (cdr tail) (cons (car list) nil))))))
