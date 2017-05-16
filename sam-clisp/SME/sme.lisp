;;; -*- Mode: LISP; Syntax: Common-lisp; Package: SME; Base: 10; Fonts: CPTFONT,TR12I -*-
#-Symbolics
(in-package "SME" :use (list sme::*the-lisp-package*))

;;; Structure-Mapping Engine (module sme.lisp: top-level, global knowledge maintenance)

;;; Copyright (C) 1986,  Brian C. Falkenhainer, University of Illinois
;;; All rights reserved

;; To Do:
;*  add argument type handling/checking
;*  add ability to specify a predicate is associative, transitive... ?
;*  can have predicate args named, but currently no hooks to really use this info. 
;      Wait till need to see how to hook.

(export '(sme-init expression fetch-dgroup fetch-expression fetch-entity-instance))

;;;; SME Initializations <ignition>


;;; Enable the initialization of SME without the window system being up
;;;
(defun sme-init (&optional (initialize-language? t) (initialize-rules? t))
  (setq *description-groups* nil)
  (setq *description-hash-table* (make-hash-table :test #'equal
						  :size *description-table-size*))
  (format t "~%Initializing SME...")
  (setq *initialized?* t)
  (when initialize-language?
     (dolist (p *sme-predicates*)
        (setf (get p :subclasses) nil)
        (setf (get p :sme-predicate) nil)
        (setf (get p :documentation) nil))
     (initialize-language))
  (when initialize-rules?
    (format t "~%   Loading default rules file: ~A" *sme-default-rules*)
    (tre-init)
    (load *sme-default-rules* :verbose nil))
  (format t "~%Complete.")
  t)

;;; Load the SME language file and convert all the symbols
;;;   in the type hierarchy to their associated structs
;;;
(defun initialize-language ()
  (format t "~%   Loading default language file: ~A" *sme-language-file*)
  (load *sme-language-file* :verbose nil))


;;; DEAD CODE
;;(defun compile-sme-type-hierarchy (&aux a-type type-roots)
;;  (dolist (sme-type *sme-types*)
;;     (cond ((type-isa sme-type)
;;	    (when (symbolp (type-isa sme-type))
;;	      (setq a-type (fetch-type-dec (type-isa sme-type)))	;type-isa is a symbol
;;	      (cond (a-type
;;		     (setf (type-isa sme-type) a-type)	;set type-isa to the actual struct
;;		     (pushnew sme-type (type-subtypes a-type) :test #'eq))
;;		    ((error "The type hierarchy is missing a definition for ~A"
;;			    (type-isa sme-type))))))
;;	   (t (setf (type-level sme-type) 1)
;;	      (push sme-type type-roots))))
;;  (do ((types type-roots (append (type-subtypes (car types)) (cdr types))))
;;      ((null types))
;;    (dolist (sub-type (type-subtypes (car types)))
;;      (setf (type-level sub-type) (1+ (type-level (car types)))))))


;;; sme-assert expressions (functors) about a given situation (description-group)
;;;     The name of the sme-assertion is either given or generated automatically
;;;
(defun expression (form description &key expression-name update-structure?)
  (let ((dgroup (if (dgroup? description) description
		                          (fetch-dgroup description T)))
	sme-assertion)
    (cond ((setq sme-assertion (fetch-expression form dgroup))	;matches an old lisp form
	   (if (and expression-name (not (eq expression-name (expression-name sme-assertion))))
	       (error "sme-assertion <~A> made twice with inconsistent names" form)))
	  ((multiple-value-bind (sys-form children)	;parse it
	       (parse-structure-expression form dgroup)
	     (cond ((setq sme-assertion (fetch-expression sys-form dgroup)) ;parsed version matches
		    (if (and expression-name
			     (not (eq (expression-name sme-assertion) expression-name)))
			(error "sme-assertion <~A> made twice with inconsistent names" form)))
		   (t (setq sme-assertion (make-expression
                                           :original-lisp-form form   ;create new one.
					   :sys-lisp-form sys-form
					   :dgroup dgroup
					   :order 0))
		      (initialize-expression sme-assertion expression-name dgroup)
		      (impose-parental-guidance sme-assertion children dgroup)
		      (if update-structure? (update-identify-structure sme-assertion)))))))
    sme-assertion))


;;; Put the consequences of having a new sme-assertion into their proper places.
;;;
(defun initialize-expression (exp name dgroup)
  (setf (expression-functor exp) (first (expression-original-lisp-form exp)))
  (cond (name
	 (when (sme:predicate? name)
	    (format t "~%WARNING: You're using ~S as an expression name in Dgroup ~S"
		    name dgroup)
	    (format t "~%         It's also a predicate. You may lose!")))
	((setq name (gentemp (format nil "~A-" (expression-functor exp)))))) ;give it a name
  (setf (expression-name exp) name)
  (setf (get name (dgroup-name dgroup)) exp)  ;exp record can be found in dgroup-name field
  (push exp (dgroup-expressions dgroup))
  (push exp (dgroup-roots dgroup)))  ;add to roots.  A parent is responsible for removing it.


;;; Tell the children who their parent is.
;;;
(defun impose-parental-guidance (parent children dgroup)
  (let ((order 0))
    (setf (expression-arguments parent) children)	     ;children static, no push
    (dolist (child children)
      (cond ((expression? child)
	     (setq order (max order (expression-order child))) ;find max order of parent's children
	     (push parent (expression-parents child)) ;parents are dynamic
	     (setf (dgroup-roots dgroup)	;the children are no longer dgroup roots
		   (delete child (dgroup-roots dgroup) :test #'equal)))
	    ((entity? child) (push parent (entity-parents child dgroup)))
	    ((error "Expression item <~A> discovered which is neither an ~
                     expression nor entity" child))))
    (setf (expression-order parent) (1+ order))))


;;; Parse an expression lisp form
;;;    RETURN:  (values sys-lisp-form children-records)
;;;
(defun parse-structure-expression (exp-form dgroup &aux lform children child-item)
  (verify-predicate-use exp-form)	;signal an error if predicate used improperly
  (dolist (child (cdr exp-form) (values (cons (car exp-form) (nreverse lform))
					 (nreverse children)))
    (setq child-item (parse-expression-child child dgroup))
    (push child-item children)
    (if (expression? child-item)
	(push (expression-name child-item) lform)      ;child is an expression
	(push (entity-name child-item) lform))))       ;child is an entity


(defun parse-expression-child (child-form dgroup)   "return entity symbol or exp record"
  (cond ((or (symbolp child-form) (numberp child-form))	     ;entity or expression name
	 (cond ((expression-name? child-form dgroup)
		(fetch-expression child-form (dgroup-name dgroup)))
	       ((entity-name? child-form) (fetch-entity-definition child-form))
	       ((error "Found <~A>.  Must be a expression or an entity." child-form))))
	((fetch-expression child-form dgroup))	     ;already exists
	((expression child-form (dgroup-name dgroup)))))   ;declare new expression


;;; Signal an error if the predicate (car form) is being used
;;;  outside of declared usage.
;;;****  check each argument's expression-type against the predicate's
;;;      argument types <not yet implemented>
;;;
(defun verify-predicate-use (form) "return true if given list is a properly used predicate"
  (cond ((atom form) (error "The expression ~S is an atom, not a list." form))
	((not (predicate? (car form)))
	 (error "~%~S is not a declared predicate (use defPredicate)." (car form)))
	((not (or (n-ary? (car form))
		  (= (length (cdr form)) (numargs (car form)))))
	 (error "Predicate ~S is declared for ~D argument~:P,~%but ~S has ~D."
		(car form) (numargs (car form)) form (length (cdr form)))))
  T)	;if it got this far...


;;; Identify the roots each expression has.  This will make it
;;;    possible to quickly tell if two expressions are members
;;;    of the same structure.
;;;
(defun identify-structure (dgroup)
  (dolist (root (dgroup-roots dgroup))
    (push root (expression-roots root))		      ;make it its own root
    (do ((queue  (expression-arguments root)  (append new (cdr queue)))
	 (new nil nil))
	((null queue))				      ;tell his children about him
      (when (expression? (car queue))
	(pushnew root (expression-roots (car queue)) :test #'eq)
	(setq new (expression-arguments (car queue)))))))

;;; Update the results of the identify roots procedure as a result of a
;;;   new root expression being added after the defDescription-group was
;;;   originally executed.
;;;
(defun update-identify-structure (new-exp)
  (labels ((propagate (current-exp new-root old-roots)
	     (do ((queue  (expression-arguments current-exp)  (append new (cdr queue)))
		  (new nil nil))
		 ((null queue))
	       (when (expression? (car queue))
		 (dolist (old-root old-roots)
		   (setf (expression-roots (car queue))
			 (delete old-root (expression-roots (car queue)))))
		 (pushnew new-root (expression-roots (car queue)) :test #'eq)
		 (cond ((member (car queue)
				(expression-roots (car queue)) :test #'eq)    ;own root?
			(setf (expression-roots (car queue))
			      (delete (car queue) (expression-roots (car queue))))
			(propagate (car queue) new-root (cons (car queue) old-roots)))
		       ((setq new (expression-arguments (car queue)))))))))
    (push new-exp (expression-roots new-exp))	;make him his own
    (propagate new-exp new-exp nil)))



;;;; Fetching description-groups and expressions

;;; In many functions, the refered-to description-group can be
;;;   given as either its name or its structure.
;;;
(defun return-dgroup (description) "return a dgroup if it exists, given name or dgroup"
  (cond ((symbolp description)
	 (or (fetch-dgroup description)
	     (error "Description group name given <~A> which isn't a dgroup" description)))
	((Dgroup? description) description)
	((error "Given <~A> when a description name or record was required" description))))

;;; Description groups are indexed under their name only.
;;;  If create? the dgroup will be created and stored if it didn't exist.
;;;
(defun fetch-dgroup (name &optional create?)
  (or (gethash name *description-hash-table*)
      (and create?
	   (let ((dgroup (make-dgroup :name name)))
	     (setf (gethash name *description-hash-table*) dgroup)
	     (setq *description-groups*		;sort them for the folks
		   (merge 'list (list name) *description-groups* #'string<))
	     dgroup))))


;;; Expressions are found in the property-list of their name under
;;;    their dgroup-name or are found by looking in their parent dgroup.
;;;    Currently, giving the lisp-form means a linear search through the
;;;       dgroup list of expressions.
;;;         Better way??
;;;    Do not need unique sme-assertion names across dgroups.
;;;
(defun fetch-expression (exp description &optional (err? t))
  (let ((dgroup (return-dgroup description)))
    (cond ((symbolp exp)
	   (or (get exp (dgroup-name dgroup))
	       (if err?
		  (error "<~A> was given as an expression name in Dgroup ~A but was not found"
			  exp (dgroup-name dgroup)))))
	  ((find-if #'(lambda (f) (or (equal (expression-original-lisp-form f) exp)
				      (equal (expression-sys-lisp-form f) exp)))
		    (dgroup-expressions dgroup))))))
