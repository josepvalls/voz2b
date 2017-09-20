;;; -*- Mode: LISP; Syntax: Common-lisp; Package: SME; Base: 10; Fonts: CPTFONT,TR12I -*-
#-Symbolics
(in-package "SME" :use sme::*the-lisp-package*)

;;;; Structure-Mapping Engine  (module generalize.lisp: Gmap generalization)

;;; Copyright (C) 1986,  Brian C. Falkenhainer, University of Illinois
;;; All rights reserved


(export '(generalize))

;;;
;;; Generalize algorithm based on Hayes-Roth, J.McDermott (ACM 1978),
;;;   plus two extended types
;;;
;;;  Will form three alternative (possibly the same) generalizations
;;;   from the given analogy results.
;;;      (1) Minimal generalization:  Only those aspects found to be the same.
;;;      (2) Maximal common generalization:  Only those aspects found to be
;;;             the same, including functions of different names.
;;;      (3) Maximal generalization:  All aspects found to be the same,
;;;             including all infered common aspects (e.g. CI's)
;;;

(defvar *skolemize-index* 0 "counter for skolemization purposes (like gensym)")
(defvar *skolem-storage* nil "stores a list of predicate pairings to maintain uniqueness")

;;; Print out the three generalizations
(defun generalize (gmap &optional display?)
  (when (null gmap) (format t "~%~%  THERE IS NO ANALOGY TO GENERALIZE~%~%")
	            (return-from generalize))
  (let ((mark (list nil))
	gmap-roots gen1 gen2 gen3 a-gen modes bad-tree?)
    (if display?
	(sme-format "~%Generalizations for Match from ~A to ~A:"
		    (dgroup-name *base*) (dgroup-name *target*)))
    (setq *skolem-storage* nil)
    (setq gmap-roots (remove-if #'mh-justifications (gm-elements gmap)))
    ;; form generalization number one
    (dolist (root gmap-roots)
      (multiple-value-bind (a-gen bad-subtree?) (gather-generalization root gmap 1 mark)
	(cond (bad-subtree? (setq gen1 (nconc a-gen gen1))
			    (setq bad-tree? t))
	      ((push a-gen gen1)))))
    (when bad-tree?
      (setq gen1 (remove-duplicates gen1 :test #'equal))
      (setq gen1 (remove-if #'symbolp gen1)))
    (setq modes (list 1))
    ;; form generalization number two
    (dolist (root gmap-roots)
      (setq a-gen (gather-generalization root gmap 2 mark))
      (push a-gen gen2))
    (cond ((equal gen1 gen2)	;print out the 1st one if unique, or just note 2nd one
	   (nconc modes (list 2)))
	  (t (if display? (display-gen gen1 modes))
	     (setq modes (list 2))))
    ;; form generalization number three
    (dolist (ci (gm-inferences gmap))		;get those structures headed by CI's
      (multiple-value-setq (a-gen gmap-roots)
	                   (gather-gen-ci ci gmap gmap-roots *target* mark))
      (push a-gen gen3))
    (dolist (root gmap-roots)		;get generalizations from any remaining structures
      (setq a-gen (gather-generalization root gmap 3 mark))
      (push a-gen gen3))
    (when display?
      (cond ((equal gen2 gen3)			;print this stuff
	     (display-gen gen2 (nconc modes (list 3))))
	    (t (display-gen gen2 modes)
	       (display-gen gen3 '(3))))
      (sme-terpri)(sme-terpri))
    (values gen1 gen2 gen3)))

;;; Start gathering a generalization from the candidate inferences.
;;;   When a target expression name is reached, resume with
;;;   Gather-Generalization, deleting used-up root MH's as they are found
;;; Returns:  (values  a-generalization  new-gmap-roots)
;;;
(defun gather-gen-ci (root-ci gmap gmap-roots target-dgroup mark &aux a-gen the-mh)
  (cond ((entity-name? root-ci)
	 (cond ((setq the-mh (find (fetch-entity-definition root-ci)
				   (gm-elements gmap)
				   :test #'(lambda (entity form) (eq entity (third form)))
				   :key #'mh-form))
		(values (intern (format nil "ENTITY~A" (bit-number (mh-id the-mh))))
			gmap-roots))
	       (T (if (entity-constant? (fetch-entity-definition root-ci))
		      (values root-ci gmap-roots)
		      (error "SME.GENERALIZE: If have entity ~S that is neither matched ~
                              nor a constant!" root-ci)))))
	((and (symbolp root-ci) (expression-name? root-ci *target*))
	 (let ((exp (fetch-expression root-ci target-dgroup)))
	   (values (gather-generalization (find exp (gm-elements gmap) :key #'mh-target-item)
					  gmap 3 mark)
		   (delete exp gmap-roots :key #'mh-target-item))))
	((skolem? root-ci)
	 (values (intern (format nil "ENTITY-S~A" (gen-skolemize-id (skolem-token root-ci)
								    mark)))
		 gmap-roots))
	(t (dolist (sub-tree (cdr root-ci))
	     (multiple-value-bind (sub-gen new-roots)
		 (gather-gen-ci sub-tree gmap gmap-roots target-dgroup mark)
	       (push sub-gen a-gen)
	       (setq gmap-roots new-roots)))
	   (values (cons (car root-ci) (nreverse a-gen))
		   gmap-roots))))
  

;;; Go down a MH justification structure, collecting predicates & entities.
;;;   When a function predicate is reached in which the MH has different
;;;   names matched, skolemize the function if not mode 1.
;;;
(defun gather-generalization (root-mh gmap mode mark)
  (labels ((fetch-functor (mh)
	     (if (and (function? (expression-functor (mh-base-item mh)))
		      (not (eq (expression-functor (mh-base-item mh))
			       (expression-functor (mh-target-item mh)))))
		 (intern (format nil "FUNCTION~A"
			     (gen-skolemize-id (list (expression-functor (mh-base-item mh))
						     (expression-functor (mh-target-item mh)))
					       mark)))
		 (expression-functor (mh-base-item mh)))))
    (cond ((eq (mh-type root-mh) :entity)	      ;it's an EMap
	   (values (intern (format nil "ENTITY~A" (bit-number (mh-id root-mh))) nil)))
	  ((and (= mode 1)		;it's a mis-match on functions in mode one => bad
		(function? (expression-functor (mh-base-item root-mh)))
		(not (eq (expression-functor (mh-base-item root-mh))
			 (expression-functor (mh-target-item root-mh)))))
	   (values nil t))
	  (t (let* ((base-arguments (expression-arguments (mh-base-item root-mh)))
		    (children (make-sequence 'list (length base-arguments))) ;to preserve order
		    (bad-tree? nil))
	       (dolist (child (mh-justifies root-mh))         ;it's a tree, break it down
		 (multiple-value-bind (subtree bad-subtree?)
		     (gather-generalization child gmap mode mark)
		   (cond (bad-subtree?		      ;we don't like this subtree
			  (setq bad-tree? t)
			  (setq children (nconc subtree children))) ;nconc-it's list of good sub-subtrees
			 ((setf (nth (position (mh-base-item child) base-arguments) children)
				subtree)))))	;put this subtree in its proper place
	       (if bad-tree?
		   (values (delete nil children) t)
		   (values (cons (fetch-functor root-mh) children) nil)))))))


(defun gen-skolemize-id (item mark)
  (cond ((consp item)
	 (or (position item *skolem-storage* :test #'equal)
	     (prog1 (length *skolem-storage*)
		    (setq *skolem-storage* (nconc *skolem-storage* (list item))))))
	((let ((entry (get item :gen-mark))
	       id)
	   (cond ((and entry (eq (car entry) mark))  (cdr entry))
		 (t (setq id (incf *skolemize-index*))
		    (setf (get item :gen-mark) (cons mark id))
		    id))))))


(defun display-gen (generalization modes)
  (labels ((special-output (string)
	     (apply #'concatenate
		    (cons 'string
			  (map 'list #'(lambda (ch)
					 (if (eql #\return ch)
					     "~%~10T" (string ch))) string)))))
    (sme-print (with-output-to-string (stream)
	   (dolist (mode modes)
	     (case mode
	       (1 (format stream "~%~5TGeneralization #1 (Literally Common Aspects Only):"))
	       (2 (format stream "~%~5TGeneralization #2 (All Common Aspects Only):"))
	       (3 (format stream "~%~5TGeneralization #3 (Maximal Generalization):"))))
	   (dolist (gen-form generalization)
	     (format stream (special-output (with-output-to-string (stream2)
					      (pprint gen-form stream2)))))))))
