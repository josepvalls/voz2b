;;; -*- Mode: LISP; Syntax: Common-lisp; Package: SME; Base: 10; Fonts: CPTFONT,TR12I -*-
#-Symbolics
(in-package "SME" :use (list sme::*the-lisp-package*))

;;;; Structure-Mapping Engine (module match-rules-support.lisp: various rule-file support)

;;; Copyright (C) 1986,  Brian C. Falkenhainer, University of Illinois
;;; All rights reserved

(export '(children-match-potential children-of? defGiven-Mappings paired-item?
          declare-given-mappings clear-given-mappings sanctioned-pairing?
          predicate-type-intersection? map-path-length))


;;; Children have potential match if:
;;;     both are entities
;;;     functors match
;;;      one is logical and the other matches child?
;;;
(defun children-match-potential (base-exp target-exp) "boolean"
  (every #'(lambda (base-child target-child)
	     (cond ((entity? base-child) (entity? target-child))
		   ((entity? target-child) nil)
		   ((eq (expression-functor base-child) (expression-functor target-child)))))
	 (expression-arguments base-exp) (expression-arguments target-exp)))


;;; Crude method to see if base-child and target-child are both
;;;   (1) arguments of the corresponding base/target expressions and
;;;   (2) they occupy the same argument position.
;;;   NOTE:  This is only good for non-commutative predicates.
;;;
(defun children-of? (base-child target-child base-exp target-exp)
  (let ((pb (position base-child (expression-arguments base-exp))))
    (and pb
	 (eql pb (position target-child (expression-arguments target-exp))))))



;;;; Declaration of externally established pairings

;;; For some applications, a number of MHs will be discovered prior to invoking
;;;    SME on the current base-target pair.
;;;  These pairings are declared (within the rules file or from application)
;;;    using either defGiven-Mappings or declare-given-mappings
;;;  The pairings then actually get used to constrain mappings when the user's rules invoke:
;;;     sanctioned-pairing? (base-item target-item)
;;;            - is the given pair forced to go together?
;;;     paired-item? ([:base-item :target-item] item)
;;;            - is the given item one of those already spoken for?
;;;

(defvar *given-entity-pairings* nil "A-list <dotted-pairs> of (base-item . target-item)")
(defvar *given-predicate-pairings* nil "A-list <dotted-pairs> of (base-item . target-item)")


;;;        (defGiven-Mappings
;;;             Entities ((beaker horse-shoe) (vial water))
;;;             Predicates ((pressure temperature) (amount-of heat)))
;;;
(defmacro defGiven-Mappings (&rest declarations)
  `(sme::declare-given-mappings ',(getf declarations 'Entities)
				',(getf declarations 'Predicates)))

	  
(defun declare-given-mappings (entities predicates)
  (setq *given-entity-pairings* entities)
  (setq *given-predicate-pairings* predicates))

(defun clear-given-mappings ()
  (setq *given-entity-pairings* nil
	*given-predicate-pairings* nil))


;;; Is the pair one of the a-priori pairings?
;;;
(defun sanctioned-pairing? (base-item target-item)
  (some #'(lambda (pair) (and (eq (car pair) base-item) (eq (second pair) target-item)))
	(if (entity-name? base-item)
	    *given-entity-pairings*
	    *given-predicate-pairings*)))

;;; Is the given item one of those already spoken for?
;;;
(defun paired-item? (&key base-item target-item)
  (if (and base-item target-item) (error "Paired-item? takes only a single argument."))
  (if base-item
      (if (entity-name? base-item)
	  (assoc base-item *given-entity-pairings* :test #'eq)
	  (assoc base-item *given-predicate-pairings* :test #'eq))
      (if (entity-name? target-item)
	  (member target-item *given-entity-pairings* :test #'eq :key #'second)
	  (member target-item *given-predicate-pairings* :test #'eq :key #'second))))



;;;; Minimal-Ascension Principle support code

;;;  ** crude, initial pass version **


(defun predicate-type-intersection? (base-predicate target-predicate
						    &aux current bp-trail tp-trail)
  (setq current base-predicate)
  (loop (push current bp-trail)
	(setq current (fetch-predicate-definition current))
	(when (or (null current)
		  (null (setq current (sme-predicate-expression-type current))))
	  (return)))
  (setq current target-predicate)
  (loop (push current tp-trail)
	(setq current (fetch-predicate-definition current))
	(when (or (null current)
		  (null (setq current (sme-predicate-expression-type current))))
	  (return)))
  (when (eq (car bp-trail) (car tp-trail))
    (values bp-trail tp-trail)))


(defun map-path-length (base-predicate target-predicate)
  (multiple-value-bind (bp-trail tp-trail)
      (predicate-type-intersection? base-predicate target-predicate)
    (cond ((null bp-trail) 0)
	  ((eq base-predicate target-predicate) 1)
	  (t (loop (cond ((eq (second bp-trail) (second tp-trail))
			  (pop bp-trail)
			  (pop tp-trail))
			 ((return (1- (+ (length bp-trail) (length tp-trail)))))))))))


;;;; Contextual Structure Mapping support

;;; Rule-based MH NoGoods: Time rearrangement
  
(defun temporally-scoped? (item)
  (if (expression? item)
      (temporal-periods item)))			;see if it has one


;;; What's the temporal period of this expression? If not temporally-scoped, return NIL
;;;
(defun temporal-periods (exp)
  (if (temporal-relation? exp)			    ;either it's temporal
      (list exp)
      (do ((ancestors (expression-parents exp)	    ; ...or one of its ancestors
		      (append new-ancestors (cdr ancestors)))
	   (new-ancestors nil nil)
	   (temporal-periods nil))
	  ((null ancestors) temporal-periods)	;return the time period identifier
	(if (temporal-relation? (car ancestors))
	    (pushnew (car ancestors) temporal-periods)
	    (setq new-ancestors (expression-parents (car ancestors)))))))


;;; The expression is itself a temporal relation [e.g., (Situation S1 ...)]
;;;
(defun temporal-relation? (exp)
  (and (expression? exp)
       (eq (sme-predicate-expression-type
	           (fetch-predicate-definition (expression-functor exp)))
	   :temporal)))


;;; Are the two expressions within the same temporal scope?
;;;
(defun same-time? (exp1 exp2)
  (let ((periods1 (temporal-periods exp1)))
    (and periods1
	 (some #'(lambda (period2) (member period2 periods1))
	       (temporal-periods exp2)))))

;;; Are the two expressions from different temporal periods?
;;;   Note how this isn't necessarily the same as (not (same-time? exp1 exp2)),
;;;   since here we're testing to see if they exist in different times, not that
;;;   they don't coexist at some other time.
;;;
(defun different-time? (exp1 exp2)
  (let ((periods1 (temporal-periods exp1))
	(periods2 (temporal-periods exp2)))
    (and periods1
	 (or (some #'(lambda (period2) (not (member period2 periods1)))
		   periods2)
	     (some #'(lambda (period1) (not (member period1 periods2)))
		   periods1)))))


;;; Rule-based MH NoGoods: Contained-liquid rearrangement

(defun contained-liquid? (e dgroup-indicator)
  (when (entity? e)
     (let ((dgroup (case dgroup-indicator
		     (:base-item *base*)
		     (:target-item *target*)
		     (T (error "Invalid dgroup indicator: ~S" dgroup-indicator))))
	   (form (list 'user::contained-liquid (entity-name e))))
       (find-if #'(lambda (exp) (equal form (expression-original-lisp-form exp)))
		(dgroup-expressions dgroup)))))


(defun container-of? (cs can dgroup-indicator)
  (when (and (entity? cs) (entity? can))
     (let ((dgroup (case dgroup-indicator
		     (:base-item *base*)
		     (:target-item *target*)
		     (T (error "Invalid dgroup indicator: ~S" dgroup-indicator))))
	   (form (list 'user::container-of (entity-name cs) (entity-name can))))
       (find-if #'(lambda (exp) (equal form (expression-original-lisp-form exp)))
		(dgroup-expressions dgroup)))))


;;; Functionally analogous

(defun implicational? (exp)
  (and (expression? exp)
       (eq (sme-predicate-expression-type
	           (fetch-predicate-definition (expression-functor exp)))
	   :implicational)))

(defun implicational-consequent (exp)
  (second (expression-arguments exp)))

(defun implicational-antecedent (exp)
  (first (expression-arguments exp)))


(defun behavioral-relation? (exp)
  (member (expression-functor exp) '(user::decreasing user::increasing)))

;;;

(defun current-observation (token)
  (labels ((current-obs? (bseg)
	      (or (eq (user::bseg-name bseg) token)
		  (some #'current-obs? (user::bseg-components bseg)))))
    (current-obs? (user::obs-given user::*current-observation*))))
