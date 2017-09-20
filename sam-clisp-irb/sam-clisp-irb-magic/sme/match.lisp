;;; -*- Mode: LISP; Syntax: Common-lisp; Package: SME; Base: 10; Fonts: CPTFONT,TR12I -*-
#-Symbolics
(in-package "SME" :use (list sme::*the-lisp-package*))

;;; Structure-Mapping Engine  (module match.lisp: the SME algorithm)

;;;; Structure-Mapping Engine (SME):

;;; Author:  Brian Falkenhainer
;;;          1304 W. Springfield, Urbana, Illiniois, 61801
;;;          Arpanet: falken@a.cs.uiuc.edu
;;;
;;; Qualitative Reasoning Group:
;;;          Kenneth D. Forbus
;;;          1304 W. Springfield, Urbana, Illiniois, 61801
;;;          Arpanet: forbus@a.cs.uiuc.edu

;;; The contents of this directory structure is subject to the following
;;; copyright notice:
;;;
;;;	Copyright (c) 1986 Brian C. Falkenhainer and the University of Illinois
;;;
;;;	This material was developed at the University of Illinois,
;;;	Department of Computer Science. Permission to use this software is
;;;	granted, subject to the following restrictions and understandings:
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. This software is for educational and research purposes only, and
;;;	should not be re-distributed in any form without first notifying
;;;	Brian Falkenhainer.
;;;
;;;	3. Users of this software agree to make their best efforts (a)
;;;	to return to Brian Falkenhainer or the Qualitative Reasoning Group
;;;	any improvements or extensions that they make, so that these may be
;;;	included in future releases; and (b) to inform Brian Falkenhainer of
;;;	noteworthy uses of this software.
;;;
;;;	4. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	5. Brian Falkenhainer and the University of Illinois have made no
;;;	warrantee or representation that the operation of this software will
;;;	be error-free, and are under no obligation to provide any services,
;;;	by way of maintenance, update, or otherwise.
;;;
;;;	6. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of Brian Falkenhainer,
;;;	the University of Illinois nor of any adaptation thereof in any
;;;	advertising, promotional, or sales literature without prior written
;;;	consent from Brian Falkenhainer, or the University, in each case as
;;;	appropriate.



;;; SME History
;;;   Version   Date  Tale
;;;       1       4/86  Original SME, reported in AAAI-86 and used for AAAI-87
;;;               7/86  Generalizer added
;;;      2A       9/86  New SME.  o Now complete - guaranteed to generate ALL possible Gmaps
;;;                               o BMS added
;;;      2B     ~10/86  Commutative predicates added to SME (not to rules).
;;;      2C       6/87  Commutative MHC rule added to all rule files to
;;;                        make AIJ paper accurate.
;;;      2D       9/87  o Commutative predicates fixed in propagate-death. 
;;;                           Had failed to take into account justification holes.
;;;                     o Version numbers added to system
;;;              10/87  o Postpone setting mh-nogoods & mh-descendants in
;;;                         justifications-for(-non)-commutatives until propagate-descendants.
;;;                         Allows commutative entity-jusitifiers.
;;;                     o Commutative systematicity rule added to all rule files.
;;;               6/88  o Fixed justifications-for-non-commutatives.
;;;                         Wasn't justification-holing MH's between rels w/ different #args.
;;;      2E      10/88  o Ability to break grounding (support) criteria for relational groups.
;;;                     o One-to-one requirement removed (may be switched with *one-to-one?*)
;;;                         Rules may now enforce such things through Conflicting.

(defvar *version* "2E")

(export '(*base* *target* *Gmaps* *Match-Hypotheses*
	  MHC-rule install-MH *MHC-intern-rules* *MHC-filter-rules* sme-rules-file
	  match best-gmaps fetch-mh fetch-mh-target
	  gm-inferences gm-bms-node gm-base gm-target gmap? gm-emaps gm-id gm-plist gm-elements
	  mh-base-item mh-target-item mh-type expression-name
	  emap-alist skolem? skolem-token))

;;; Analogical matching engine
;;;
;;;    Given two description groups which are possibly analogous, the matcher will:
;;;          1) Create a match hypothesis network using the current
;;;               set of Match Hypothesis Creation rules
;;;          2) Form all sets of consistent match hypotheses (Gmaps to some)
;;;          3) Weigh the quality of each Gmap according to the available evidence
;;;               

;;; Syntax of rules:
;;;
;;; Match Hypothesis Creation Rules:
;;;
;;;     (MHCrule  (<condition>  base-exp-variable  target-exp-variable {:test test-form})
;;;           . <body>)
;;;
;;;       <condition> ::   :intern  |  :filter
;;;              filter rules called for every combination of
;;;                  base expression - target expression
;;;              intern rules called for every base-target expressions
;;;                  which have a MH between them.
;;;       <body>  :: forms to eval representing the body of the rule.
;;;                     a MH is created by the call: (install-MH base-item target-item)
;;;
;;;    (MHCrule (:filter ?base-exp ?target-exp :test (eq (expression-functor ?base-exp)
;;;							(expression-functor ?target-exp)))
;;;	 (install-MH ?base-exp ?target-exp))
;;;
;;;
;;; Evidence Rules:
;;;
;;;     (rule (list of <trigger>)  .  <body>)
;;;
;;;         <trigger> ::   (:intern <pattern>  {:test test-form}
;;;                                            {:var var-to-set-to-pattern})
;;;
;;;     (rule ((:intern (MH ?b ?t) :test (and (expression? ?b) (expression? ?t)
;;;					     (eq (expression-functor ?b)
;;;                                               (expression-functor ?t)))))
;;;	   (rsme-assert! (implies same-functor (MH ?b ?t) (0.5 . 0.0))))
;;;


;;;; Definitions

(defstruct (Match-Hypothesis (:predicate mh?)
			     (:conc-name mh-)
			     (:print-function (lambda (mh str ignore)
						(format str "MH#~A" (mh-form mh)))))
  id			;unique bit-vector identifier
  form			;(MH base-item target-item) - for hashing purposes
  base-item		;an expression or entity
  target-item		;an expression or entity
  nogood		;no-good bit-vector set of all MH's which cause
			; inconsistency with this one
  descendants		;bit-vector of all MH's (including itself) which
			; represent its descendants
  justifications	;those MH's which caused this one to be created (parents)
  justifies		;those MH's which this one justifies
			; (eg. the entity MH's this 1st order caused)
  justifies-incomplete?	;boolean - if not all args matched, we could sit
			; forever waiting for them
  (order 0)    		;max of the orders for base-item and target-item
  type			; :entity or :expression  -  speeds the long form for testing this
  mark			;one more slot!
  bms-node		;the bms node for this MH
  plist			;???
  )


(defstruct (global-mapping (:predicate gmap?)
			   (:conc-name gm-)
			   (:print-function (lambda (gm str ignore)
					      (format str "#GM~A" (gm-id gm)))))
  id			;unique integer identifier - starting from 0
  elements		;list of the MH's composing this gmap
  emaps			;list of entity mappings this gmap represents <list of (e1 e2)>
  elements-vector	;the bit-vector union of all of its elements (MH's)
  nogood		;the union of all of its elements no-good sets
  inferences		;candidate inferences we could make from this gmap
  root-elements		;list of the MH's which represent roots of MH trees
  base			;base dgroup this is a mapping to
  target		;target dgroup this is a mapping to
  bms-node		;the bms node for this GMap
  plist			;
  )

(defsme-Class MH t)		;declare sme-class of forms (MH ... ...) for BMS to hash

(defvar *Match-Hypotheses* nil "a list of all current match hypotheses")
(defvar *mh-identifier* (cons 1 nil) "next available bit vector identifier for a MH")
(defvar *mh-hash-table*    nil "Store match hypotheses")
(defvar *mh-table-size*   1021 "Size of analogical matching table")

(defvar *gmaps*      nil "list of maximum sets of MH's which are consistent")
(defvar *gmap-count* 0  "counter to give each gmap a unique identity")

(defvar *match-rules-file* "match file not defined" "name of current match-rules file")
(defvar *MHC-intern-rules* nil)
(defvar *MHC-filter-rules* nil)
(defvar *mhc-rule-count* 0)

(defvar *base* nil "holds the dgroup which is the current base (for outside module support)")
(defvar *target* nil "holds the dgroup which is the current target")
(defvar *bms-run-time* 0 "time in seconds it took bms to run on last match")
(defvar *total-run-time* 0 "time in seconds it took all of SME to run on last match")

(defvar *Gmap-merge-step3?* t "Should the third MH combination step be performed?")
(defvar *CI-flag* t "Generate candidate inferences?")
(defvar *rel-groups?* nil "Allow structural consistency exceptions for relational groups?")
(defvar *one-to-one?* T "Adhere to full one-to-one criterion. SMT default.")

;;; Matcher parameter options
;;;
(defSME-Parameter *Gmap-merge-step3?* "Run Gmap Merge Step 3?" :boolean)
(defSME-Parameter *CI-flag* "Generate Candidate Inferences?" :boolean)
(defSME-Parameter *rel-groups?*
		  "Allow structural consistency exceptions for relational groups?"
                  :boolean)
(defSME-Parameter *one-to-one?* "Require one-to-one correspondences?" :boolean)


;;;; Match top-level

;;; Top level matcher.  Given base and target dgroups (name or structure),
;;;   computes the match and displays it to *sme-output-stream* if display? is set.
;;;
(defun match (base target &optional (display? nil) &aux start-time)
  (let ((base-dgroup (return-dgroup base))
	(target-dgroup (return-dgroup target)))
    (unless *MHC-filter-rules*
      (format t "~%=====> I can't match until I know the match rules")
      (format t "~%=====> Try (load 'match-rules)~%")
      (return-from match nil))
    (if *mh-hash-table*
	(clrhash *mh-hash-table*)
	(setq *mh-hash-table* (make-hash-table :test #'equal :size *mh-table-size*)))
    (tre-init)
    (setq start-time (get-internal-run-time))
    (setq *Match-Hypotheses* nil)
    (setq *MH-identifier* (cons 1 nil))		;bit vectors
    (setq *gmaps* nil)
    (setq *gmap-count* 0)
    (setq *base* base-dgroup)			;for applications programs mostly
    (setq *target* target-dgroup)
    
    (create-match-hypotheses base-dgroup target-dgroup)	;generate local match hypotheses
    (if *one-to-one?*
	(calculate-nogoods base-dgroup target-dgroup))	;init MH bit-vector's nogood elements
    ;; NoGoods must precede (run-rules) due to destructive
    ;;  vs/ non-destructive handling of bit vectors (NoGoods from run-rules are destructive)
    (let ((start (get-internal-run-time)))
      (run-rules)				        ;run local evidence rules
      (setq *bms-run-time* (- (get-internal-run-time) start)))
    ;; If rules contain calls to Conflicting, (run-rules) must precede (generate-gmaps)
    (generate-gmaps)				        ;generate global matches
    (dolist (gmap *gmaps*)			        ;give each Gmap unique id
      (setf (gm-id gmap) (incf *gmap-count*))
      (setf (gm-base gmap) base-dgroup)
      (setf (gm-target gmap) target-dgroup))
    (if *CI-flag* (gather-inferences base-dgroup target-dgroup)) ;gather candidate inf's
    (intern-gmaps)				        ;intern Gmaps so BMS can see them
    (let ((start (get-internal-run-time)))
      (run-rules)				        ;run Gmap evidence rules
      (incf *bms-run-time* (- (get-internal-run-time) start)))
    (setq *total-run-time* (/ (float (- (get-internal-run-time) start-time))
			      internal-time-units-per-second))
    (setq *bms-run-time* (/ (float *bms-run-time*) internal-time-units-per-second))
    (if display? (display-match base-dgroup target-dgroup *total-run-time* *bms-run-time*))
    (values *total-run-time* *bms-run-time*)))



;;;; Match Hypothesis Creation

;;; Given base and target dgroups to match, create the Match Hypothesis
;;;   network by stepping through base and target expression permutations
;;;   and feeding them to the MHC rules
;;;
(defun create-match-hypotheses (base target)
  (dolist (base-exp (dgroup-expressions base))
    (dolist (target-exp (dgroup-expressions target))
      (dolist (rule *MHC-filter-rules*)
	(funcall rule base-exp target-exp)))))


;;; Create a new Match-Hypothesis if it does not already exist.
;;;    These may be made between expressions and/or entities
;;;    On interning of a new MH, must then check firing of evidence rules
;;;
(defun install-MH (base-item target-item)
  (let* ((form (list 'MH base-item target-item))
	 (bms-node (install-bms-node form))
	 mh)
    (when (gethash form *mh-hash-table*)	;check for allowed MHCrule redundancy
      (return-from install-mh))
    (setq mh (make-match-hypothesis :id (next-identifier *mh-identifier*)
				    :base-item base-item
				    :target-item target-item
				    :form form
 				    :bms-node bms-node))
    (setf (mh-descendants mh) (fcopy (mh-id mh)))        ;its descendants include itself
    (cond ((and (expression? base-item) (expression? target-item))
	   (do-predicate-referencing base-item target-item mh)
	   (setf (mh-type mh) ':expression)
	   (setf (mh-order mh) (max (expression-order base-item)
				    (expression-order target-item))))
	  ((setf (mh-type mh) ':entity)))		        ;for gmap stuff
    (setf (gethash form *mh-hash-table*) mh)	;sme-assert the lisp form for both base and target
    (push mh (gethash (cons base-item 'base) *mh-hash-table*))  ;allow referencing on base only
    (push mh (gethash (cons target-item 'target) *mh-hash-table*)) ;referencing on target only
    (push mh *Match-Hypotheses*)
    (dolist (rule *MHC-intern-rules*) (funcall rule base-item target-item))  ;run MHC-interns
    mh))


;;; We want to know all other ways each predicate may match.
;;;  We don't want Temp->Pressure and Temp->Amount-of going into the same Gmap.
;;;
(defun do-predicate-referencing (base-exp target-exp mh)
  (let ((base-matches (gethash (cons (expression-functor base-exp) 'base) *mh-hash-table*))
	(target-matches (gethash (cons (expression-functor target-exp) 'target)
				 *mh-hash-table*))
	a-list)
    (cond ((null base-matches)
	   (setf (gethash (cons (expression-functor base-exp) 'base) *mh-hash-table*)
		 (list (cons (expression-functor target-exp) (list (fcopy (mh-id mh)) mh)))))
	  ((setq a-list (assoc (expression-functor target-exp) base-matches))
	   (push mh (cddr a-list))
	   (vunionf (cadr a-list) (mh-id mh)))
	  ((push (cons (expression-functor target-exp) (list (fcopy (mh-id mh)) mh))
		 (cdr base-matches))))
    (cond ((null target-matches)
	   (setf (gethash (cons (expression-functor target-exp) 'target) *mh-hash-table*)
		 (list (cons (expression-functor base-exp) (list (fcopy (mh-id mh)) mh)))))
	  ((setq a-list (assoc (expression-functor base-exp) target-matches))
	   (push mh (cddr a-list))
	   (vunionf (cadr a-list) (mh-id mh)))
	  ((push (cons (expression-functor base-exp) (list (fcopy (mh-id mh)) mh))
		 (cdr target-matches))))))


;;;; MH NoGoods

;;;*******don't have to loop through all items. mark them so don't redo
;;;    the union over and over for each member!!!

;;;     No Goods....
;;;  (1a) goes through all MH's for a single base EXPRESSION and makes
;;;        them all nogood's of each other
;;;  (1b) goes through all MH's for a single base ENTITY and makes them
;;;        all nogood's of each other
;;;  (2a,b) goes through all MH's for a single target EXPRESSION/ENTITY
;;;          and makes them all nogood's of each other
;;;  (3) goes through all MH's for each base predicate
;;;         and makes MH's from it to different target predicate types nogood
;;;  (4) goes through all MH's for each target predicate
;;;         and makes MH's from it to different base predicate types nogood
;;;
;;;    This method causes an expensive set-up time, but the pay-off far outweighs
;;;      the cost for non-trivial problems
;;;
;;; We must be very CAREFUL about using the destructive union function (vunionf).
;;;   After the first two dolist forms, we are guaranteed that all MH's have
;;;   been looked at since we've gone through all base expressions and entities.
;;;   For each of these, we have generated fresh cons-cells or nil for the
;;;   mh-nogood sets. Thus, the latter forms may use vunionf. Otherwise, some of
;;;   the no-good sets would have been sharing cons-cells.
;;;
(defun calculate-nogoods (base target &aux mh-set bit-set predicates tmp-bit-set)
  (labels ((safe-nogood (mh-set)
	     (when (cdr mh-set)
	       (setq bit-set (fcopy (mh-id (car mh-set))))  ;get a clean bit-set collector
	       (dolist (mh (cdr mh-set)) (setq bit-set (vunionf bit-set (mh-id mh))))
	       (dolist (mh mh-set)
		 (setf (mh-nogood mh) (fcopy (vector-difference bit-set (mh-id mh)))))))
	   (destructive-nogood (mh-set)
	     (when (cdr mh-set)
	       (setq bit-set (fcopy (mh-id (car mh-set))))
	       (dolist (mh (cdr mh-set)) (setq bit-set (vunionf bit-set (mh-id mh))))
	       (dolist (mh mh-set)
		 (setf (mh-nogood mh) (vunionf (mh-nogood mh)
					       (vector-difference bit-set (mh-id mh))))))))
    
    ;;One-to-one expression mapping consistency
    (dolist (bexp (dgroup-expressions base))  (safe-nogood (fetch-MH bexp)))
    (dolist (bentity (dgroup-entities base))  (safe-nogood (fetch-MH bentity)))
    
    (dolist (texp (dgroup-expressions target)) (destructive-nogood (fetch-MH-target texp)))
    (dolist (tentity (dgroup-entities target)) (destructive-nogood (fetch-MH-target tentity)))
    
    ;;Predicate consistency (one-to-one on predicate names)
    (setq predicates (delete-duplicates (mapcar #'expression-functor
						(dgroup-expressions base))))
    (dolist (pred predicates)
      (setq mh-set (gethash (cons pred 'base) *mh-hash-table*))
      (when (cdr mh-set)			;length greater-than one
	(setq bit-set (fcopy (cadr (first mh-set))))
	(dolist (entry (cdr mh-set)) (setq bit-set (vunionf bit-set (cadr entry))))
	(dolist (entry mh-set)
	  (setq tmp-bit-set (vector-difference bit-set (cadr entry)))
	  (dolist (mh (cddr entry))
	    (setf (mh-nogood mh) (vunionf (mh-nogood mh) tmp-bit-set))))))
    (setq predicates (delete-duplicates (mapcar #'expression-functor
						(dgroup-expressions target))))
    (dolist (pred predicates)
      (setq mh-set (gethash (cons pred 'target) *mh-hash-table*))
      (when (cdr mh-set)			;length greater-than one
	(setq bit-set (fcopy (cadr (first mh-set))))
	(dolist (entry (cdr mh-set)) (setq bit-set (vunionf bit-set (cadr entry))))
	(dolist (entry mh-set)
	  (setq tmp-bit-set (vector-difference bit-set (cadr entry)))
	  (dolist (mh (cddr entry))
	    (setf (mh-nogood mh) (vunionf (mh-nogood mh) tmp-bit-set))))))))
  
  
;;; Allow explicit declaration of a pair of MH's being NoGood.
;;;  Thus, we can get a rule-based determination of NoGood sets as well.
;;;
(defun conflicting (mh1 mh2)
  (setf (mh-nogood mh1) (vunionf (mh-nogood mh1) (mh-id mh2)))	  ;put each in the other's
  (setf (mh-nogood mh2) (vunionf (mh-nogood mh2) (mh-id mh1))))	  ; nogood set


;;;; Gmap Construction and Handling

;;;   o  Go through MH's  (1) generating justification structure.
;;;                       (2) marking first-order MH's emaps and marking those MH's
;;;                              whose "justifies" are incomplete
;;;   o  Delete all MH's whose "justifies" are incomplete or whose "justifies"
;;;        are themselves marked for deletion
;;;   o  Merge step #1: Collect all consistent MH structures
;;;   o  Merge step #2: Merge these structures together when they intersect a common base structure
;;;   o  Merge step #3: Merge these structures together (in all remaining possible ways)
;;;        to get maximally general gmaps
;;;
(defun generate-gmaps ()
  (let ((structure-groups (generate-structure-groups)))
    (when structure-groups		 ;each group is a consistent structure of MH's
      (merge-gmaps structure-groups))))	 ;merge structures to form general gmaps


;;; return a list of gmap structures representing groups of MH's
;;;  grouped strictly on justification structure
;;;
;;;  NOTE FOR COMMUTATIVES:
;;;       generate-justifications locally informs a commutative MH how it
;;;          will copy in its :copies plist slot and does killing of MH's
;;;          (and their parents) having justification holes.
;;;       propagate-descendants (aka. bubble Emaps upward) then makes duplicate MH's
;;;          (resulting in "duplicate" structures) on its way up the propagation procedure.
;;;
;;; Collect-structure returns list of (root structure emaps)
;;;
(defun generate-structure-groups (&aux roots structures)
  (multiple-value-bind (new-mhs ejusts) (generate-justifications)
    (setq new-mhs (propagate-descendants ejusts new-mhs))
    (setq roots (remove-if #'mh-justifications new-mhs))

    (setq structures (mapcan #'collect-structure roots))
    (setq structures				;remove effects of bug described below
	  (delete-duplicates structures
			     :test #'(lambda (s1 s2)
				       (vector-equal (mh-descendants (car s1))
						     (mh-descendants (car s2))))))
    (mapcar #'(lambda (structure)
		 (make-global-mapping :elements (second structure)
				      :emaps (mapcar #'mh-form (third structure))
				      :elements-vector (mh-descendants (first structure))
				      :nogood (mh-nogood (first structure))
				      :root-elements (list (first structure))))
	    structures)))


;;; The normal code - save until bug is figured out properly
;;(defun generate-structure-groups (&aux roots)
;;  (multiple-value-bind (new-mhs ejusts) (generate-justifications)
;;    (setq new-mhs (propagate-descendants ejusts new-mhs))
;;    (setq roots (remove-if #'mh-justifications new-mhs))
;;    (mapcan #'(lambda (mh)
;;		(mapcar #'(lambda (structure) ;1 root may generate >1 structure due to Emap conflicts
;;			    (let ((gm (make-global-mapping :elements (second structure)
;;					   :emaps (mapcar #'mh-form (third structure))
;;					   :elements-vector (mh-descendants (first structure))
;;					   :nogood (mh-nogood (first structure))
;;					   :root-elements (list (first structure)))))
;;			      gm))
;;			(collect-structure mh)))
;;	     ;; We're getting the error at collect-structure. We have 15 clones of (IF (AND ...) (THEN ..))
;;	     ;;  but only one of the 15 "IF"s are internally consistent. Thus, we end up with 14
;;	     ;;  copies of the children to AND...
;;	     ;; Perhaps a more fundamental problem here. What if two roots share substructure?
;;	     ;;  Probably not, since no cloning going on.... What's happening here!
;;	     ;; The duplication on gm-root-elements comes from reduce-from-set-form.
;;	     ;;   It isn't checking that what was a root isn't anymore or vice-versa. Comes from
;;	     ;;   potential for a substructure of a GM to get merged into the GM - how'd it get FREE?
;;	     ;;   only by cloning?????
;;	     ;;  SWIFT:>SKOR>SEQL> 16 (base) and 17 (target)
;;
;;	    roots)))


;;; From the given entity justifiers, propagate their contents (MH's and Emaps)
;;;   upward through the justification links
;;;            ==> uses mh-mark slot <==
;;;
(defun propagate-descendants (ejustifiers new-mhs)
  (let ((mark (list nil))
	copy-mhs)
    (do ((queue ejustifiers  (nconc (cdr queue) new))
	 (new nil nil))
	((null queue) new-mhs)
      (cond ((eq (mh-mark (car queue)) mark))	             ;already processed this MH
	    ((every #'(lambda (child-mh) (or (eq (mh-type child-mh) :entity)
					     (eq (mh-mark child-mh) mark)))
		    (mh-justifies (car queue)))	             ;processed all children?
	     (setf (mh-mark (car queue)) mark)
	     (setq copy-mhs (check-for-copies (car queue)))  ;make copies at this pt if needed
	     (dolist (mh  (cons (car queue) copy-mhs))
	       (dolist (child (mh-justifies mh))        ;now that we have children, can do bitsets
		 (setf (mh-descendants mh)
		       (vunionf (mh-descendants mh) (mh-descendants child)))
		 (setf (mh-nogood mh)
		       (vunionf (mh-nogood mh) (mh-nogood child))))
	       (setq new (nconc (fcopy (mh-justifications mh)) new)))	;*go after parent (why)
	     (setq new-mhs (nconc copy-mhs new-mhs)))
	    ((push (car queue) new))))))


;;; During upward propagation of Emaps, etc., when we get to a MH which has
;;;    been flagged to be copied (e.g., AND), we make a clone of the original
;;;    MH for each complete, consistent permutation of child MHs
;;;    (found in :copies slot of plist). Its justifies slot is set to just
;;;    that choice of children, their justifications slot is fixed to be
;;;    the clone, and the clone's parents are notified (in their :copies slot)
;;;    that they will have to clone for him when it's their turn.
;;;
(defun check-for-copies (mh)
  (let ((versions (grow-mh-copies mh))
	new-mh copy-mhs)
    (when versions
       (dolist (version (cdr versions))
	  ;;will make a copy of parent, removing all siblings from it's
	  ;;justifications structure siblings will be all those whose
	  ;;base/target pairing is the same. On propagating through parents,
	  ;;we replace the old mh in the parent's justifies with the new mh
	  ;* The above comment is shit - fix it when I remember this code better (9/28/88)
	  (setq new-mh (make-copy-mh mh))
	  (push new-mh copy-mhs)
	  (setf (mh-justifies new-mh) (cdr version)) ;justifies slot fixed to be just relevant subset
	  (dolist (child-mh (mh-justifies new-mh))     ;fix children's justifications slot
	     (setf (mh-justifications child-mh)	      ; - clone is now its parent
		   (subst new-mh mh (mh-justifications child-mh))))
	  (dolist (parent-mh (mh-justifications new-mh))   ;notify its parents how they must clone
	     (push (cons mh new-mh) (getf (mh-plist parent-mh) :child-cloning))))
       (setf (mh-justifies mh) (cdr (first versions)))	;fix the original
       copy-mhs)))


(defun grow-mh-copies (mh)
  (let ((copies (getf (mh-plist mh) :copies))
	(ch-clones (getf (mh-plist mh) :child-cloning))
	new-versions)
    (cond ((and *rel-groups?*
		(relgroup? (expression-functor (mh-base-item mh)))
		(relgroup? (expression-functor (mh-target-item mh))))
	   (grow-mh-copies-for-relgroups mh copies ch-clones))
	  (T (dolist (binding ch-clones)	;(old-child-mh . clone-child)
	       (cond (copies
		      (setq new-versions nil)
		      (dolist (version copies)
			(if (member (car binding) (cdr version))
			    (push (nsubst (cdr binding) (car binding)
					  (fcopy version)) new-versions)))
		      (setq copies (nconc new-versions copies)))
		     (t (setq copies
			      (list (cons 'bit-vector-position (mh-justifies mh))
				    (cons 'bit-vector-position
					  (nsubst (cdr binding) (car binding)
						  (fcopy (mh-justifies mh)))))))))
	     copies))))



(defun grow-mh-copies-for-relgroups (mh copies ch-clones)
  (let ((big-bucket
	  (mapcar #'(lambda (cmh)
		      (make-global-mapping :elements (list cmh)
					   :elements-vector (fcopy (mh-descendants cmh))
					   :nogood (fcopy (mh-nogood cmh))
					   :root-elements (list cmh)))
		  (nconc (mapcar #'cdr ch-clones) (mh-justifies mh)))))
    (when big-bucket
      (mapcar #'(lambda (version)
		  (cons 'bit-vector-position (gm-elements version)))
	      (full-gmap-merge big-bucket)))))	;back to the full gmap merge junk


;;;* Eleventh hour thesis bug fix - don't want to throw old away just yet...
;;(defun check-for-copies (mh)
;;  (let ((versions (getf (mh-plist mh) :copies))
;;	new-mh copy-mhs parent-versions new-versions)
;;    (when versions
;;       (dolist (version (cdr versions))
;;	  ;;will make a copy of parent, removing all siblings from it's
;;	  ;;justifications structure siblings will be all those whose
;;          ;;base/target pairing is the same. On propagating through parents,
;;          ;;we replace the old mh in the parent's justifies with the new mh
;;	  ;* The above comment is shit - fix it when I remember this code better (9/28/88)
;;	  (setq new-mh (make-copy-mh mh))
;;	  (push new-mh copy-mhs)
;;	  (setf (mh-justifies new-mh) (cdr version)) ;justifies slot fixed to be just relevant subset
;;	  (dolist (child-mh (mh-justifies new-mh))     ;fix children's justifications slot
;;	    (setf (mh-justifications child-mh)	      ; - clone is now its parent
;;		  (subst new-mh mh (mh-justifications child-mh))))
;;	  (dolist (parent-mh (mh-justifications new-mh))   ;notify its parents how they must clone
;;	    (setq parent-versions (getf (mh-plist parent-mh) :copies))	 ;**circular!!!!!!!!***
;;	    (cond (parent-versions
;;		   (setq new-versions nil)
;;		   (dolist (parent-version parent-versions)
;;		     (if (member mh (cdr parent-version) :test #'eq)
;;		       (push (nsubstitute new-mh mh (fcopy parent-version)) new-versions)))
;;		   (setf (getf (mh-plist parent-mh) :copies)
;;			 (nconc new-versions parent-versions)))
;;		  (t (setf (getf (mh-plist parent-mh) :copies)
;;			   (list (cons 'bit-vector-position (mh-justifies parent-mh))
;;				 (cons 'bit-vector-position
;;				       (nsubstitute new-mh mh
;;						    (fcopy (mh-justifies parent-mh))))))))))
;;       (setf (mh-justifies mh) (cdr (first versions)))	;fix the original
;;       copy-mhs)))


;;; isn't telling children of new parent
;;(defun propagate-copy-through-parents (new-mh old-mh
;;						 &aux new-parent new-parents side-effects)
;;  (dolist (parent (mh-justifications old-mh))
;;    (setq new-parent (make-copy-mh parent))
;;    (setf (mh-justifies new-parent) (subst new-mh old-mh (mh-justifies new-parent)))
;;    (setf (getf (mh-plist new-parent) :copies)
;;	  (subst new-mh old-mh (getf (mh-plist new-parent) :copies)))
;;    (push parent new-parents)
;;    (setq side-effects
;;	  (nconc (propagate-copy-through-parents new-parent parent) side-effects)))
;;  (setf (mh-justifications new-mh) new-parents)
;;  (nconc (fcopy new-parents) side-effects))	;report back the new MH's



(defun make-copy-mh (mh)
  (make-match-hypothesis :id (next-identifier *mh-identifier*)
			 :form (mh-form mh)
			 :base-item (mh-base-item mh)
			 :target-item (mh-target-item mh)
			 :nogood (fcopy (mh-nogood mh))
			 :descendants (fcopy (mh-descendants mh))
			 :justifications (fcopy (mh-justifications mh))
			 :justifies (fcopy (mh-justifies mh))
			 :order (mh-order mh)
			 :type (mh-type mh)
			 :mark (mh-mark mh)
			 :bms-node (mh-bms-node mh)
			 :plist (fcopy (mh-plist mh))))



;;; return list of structurally sound MH's with justification pointers installed
;;;
(defun generate-justifications (&aux death-row ejusts)
  (dolist (mh *Match-Hypotheses*)	;generate justification structure / initial Gmaps
    (when (eq (mh-type mh) ':expression)
      (cond ((commutative? (expression-functor (mh-base-item mh)))
	     (if (commutative? (expression-functor (mh-target-item mh)))
		 (multiple-value-bind (ejustifier? dead? copies)
		     (justifications-for-commutatives mh)
		   (if dead? (push mh death-row)
		             (if ejustifier? (push mh ejusts)))
		   (setf (getf (mh-plist mh) :copies) copies))
		 (error "WARNING!! A commutative matched to a non-commutative <~A  ~A>"
			(mh-base-item mh) (mh-target-item mh))))
	    (t (multiple-value-bind (ejustifier? dead?)
		   (justifications-for-non-commutatives mh)
		 (if dead?
		     (push mh death-row)
		     (if ejustifier? (push mh ejusts))))))))
  (propagate-death death-row ejusts))  ;here's where we kill MH's with justification holes.


;;; Given a commutative MH, find all possible ways to pair up the
;;;    base-item and target-item arguments. Tell these children
;;;    MH's about the parent MH and vice versa.
;;;
(defun justifications-for-commutatives (mh)  ;have to get dirt under fingernails someplace...
  (let ((base-exp (mh-base-item mh))
	(target-exp (mh-target-item mh))
	arg-mh-sets ejustifier? copies)
    (setq arg-mh-sets
	  (mapcar #'(lambda (arg)
		      (delete-if-not #'(lambda (arg-mh)
					 (member target-exp
						 (if (expression? (mh-target-item arg-mh))
						     (expression-parents
						              (mh-target-item arg-mh))
						     (entity-parents (mh-target-item arg-mh)
								     *target*))
						 :test #'eq))
				     (fcopy (fetch-mh arg))))
		  (expression-arguments base-exp)))
    (dolist (arg-mh-set arg-mh-sets)
      (dolist (arg-mh arg-mh-set)
	(push arg-mh (mh-justifies mh))
	(push mh (mh-justifications arg-mh))
	(if (eq (mh-type arg-mh) :entity)  (setq ejustifier? t))))
    (setq copies (make-permutations arg-mh-sets base-exp target-exp))
    (values ejustifier? (null copies) copies)))


;;; For use with justifications-for-commutatives
;;;  mh-lists = ((all MHs pairing base-item arg #1 to a target-item arg)
;;;                (...base-item arg #2 to a target-item arg) (..#3..) ...)
;;;  Find all consistent sets of argument orderings for base-item and target-item
;;;
(defun make-permutations (mh-lists base-exp target-exp)
  (if (and *rel-groups?*
	   (relgroup? (expression-functor base-exp))
	   (relgroup? (expression-functor target-exp)))
      (permutations-for-rel-groups mh-lists)
      (permutations-for-normals mh-lists)))


(defun permutations-for-rel-groups (mh-lists)
  (let ((mh-bunch
	  (mapcan #'(lambda (mh-list)
		      (mapcar #'(lambda (mh)
				  (make-global-mapping :elements (list mh)
						:elements-vector (fcopy (mh-descendants mh))
						:nogood (fcopy (mh-nogood mh))
						:root-elements (list mh)))
			      mh-list))
		  mh-lists)))
    (when mh-bunch
      (mapcar #'(lambda (version &aux id-bitset)
		  (setq id-bitset (fcopy (mh-id (car (gm-elements version)))))
		  (dolist (other (cdr (gm-elements version)))
		    (vunionf id-bitset (mh-id other)))
		  (cons (cons id-bitset (fcopy (gm-nogood version)))
			(gm-elements version)))
	      (full-gmap-merge mh-bunch)))))	;back to the full gmap merge junk
  
	       
(defun permutations-for-normals (mh-lists &aux versions new-version)
  (cond ((cdr mh-lists)
	 (setq versions (permutations-for-normals (cdr mh-lists)))
	 (mapcan #'(lambda (version)
		     (mapcan #'(lambda (mh)
				 (unless (or (vector-intersection? (mh-id mh) (cdar version))
					     (vector-intersection? (mh-nogood mh)
								   (caar version)))
				   (setq new-version (copy-tree version))
				   (vunionf (caar new-version) (mh-id mh))
				   (vunionf (cdar new-version) (mh-nogood mh))
				   (push mh (cdr new-version))
				   (list new-version)))
			     (car mh-lists)))
		 versions))
	((mapcar #'(lambda (mh) (list (cons (fcopy (mh-id mh))
					    (fcopy (mh-nogood mh)))
				      mh))
		 (car mh-lists)))))


;;; Very simple - given a MH, find MHs for corresponding arguments of the
;;;    base and target items and tell these children MH's about the parent
;;;    MH and vice versa. If one of the children MHs is an Emap, add Emap's
;;;    descendants and nogood bit vectors to parent (initial upward Emap
;;;    propagation) and return ejustifier? true.  If one of the expected
;;;    children MHs is missing, mark the parent as dead (having a justification hole).
;;;
(defun justifications-for-non-commutatives (mh &aux ejustifier? dead?)
  (do ((base-args  (expression-arguments (mh-base-item mh))  (cdr base-args))
       (target-args  (expression-arguments (mh-target-item mh))  (cdr target-args))
       (args-mh))
      ((or (null base-args) (null target-args))
       (when (or base-args target-args)		;don't let different number of args slip by
	 (setf (mh-justifies-incomplete? mh) t)	;*we have a dependency hole
	 (setq dead? t)))
    (cond ((setq args-mh (fetch-MH (first base-args) (first target-args)))
	   (push args-mh (mh-justifies mh))	;add justification links
	   (push mh (mh-justifications args-mh))
	   (if (eq (mh-type args-mh) :entity)  (setq ejustifier? t)))
	  ((setf (mh-justifies-incomplete? mh) t)	;*we have a dependency hole
	   (setq dead? t))))
  (values ejustifier? dead?))


;;; Given a list of MH's which have an incomplete "justifies" sub-structure,
;;;    propagate upward marking their justifiers as incomplete also.
;;;    Return list of structurally sound MH's
;;;
(defun propagate-death (death-row ejusts &aux copies)
  (do ((queue death-row (append new (cdr queue)))
       (new nil nil))
      ((null queue)
       (values (remove-if #'mh-justifies-incomplete? *match-hypotheses*)
	       (remove-if #'mh-justifies-incomplete? ejusts)))
    (setf (mh-justifies-incomplete? (first queue))  t)    ;mark it as bad
    (dolist (mh (mh-justifies (first queue)))		  ;can't justify somebody anymore
      (setf (mh-justifications mh)
	    (delete (first queue) (mh-justifications mh) :test #'eq)))
    (dolist (parent (mh-justifications (first queue)))	   ;collect parents to kill
      (cond ((and *rel-groups?*		;in a relgroup, doesn't necessarily kill the copy
		  (relgroup? (expression-functor (mh-base-item parent)))
		  (relgroup? (expression-functor (mh-target-item parent)))
		  (setq copies (getf (mh-plist parent) :copies)))
	     (setq copies (remove-from-relgroup (car queue) copies))
	     (setf (mh-justifies parent)	;remove bad seed from parent's memory
		   (delete (first queue) (mh-justifies parent) :test #'eq))
	     (setf (getf (mh-plist parent) :copies) copies)  ;don't make parent clone for him
	     (if (null copies)
		 (push parent new)))  ;parent killed - all copies represent justification hole
	    ((setq copies (getf (mh-plist parent) :copies))      ;for commutatives
	     (setq copies (delete-if #'(lambda (version)
					 (vector-intersection? (mh-id (car queue))
							       (caar version)))	;children ids
				     copies))
	     (setf (mh-justifies parent)	;remove bad seed from parent's memory
		   (delete (first queue) (mh-justifies parent) :test #'eq))
	     (setf (getf (mh-plist parent) :copies) copies)  ;don't make parent clone for him
	     (if (null copies)
		 (push parent new)))  ;parent killed - all copies represent justification hole
	    ((push parent new))))))


;;;* I might be able to do this just by deleting the mh from each copy and then deleting
;;;  things that are now duplicates, but since I don't have time to think it through, we'll
;;;  just use the more expensive permutations-for-rel-groups
;;;
;;;  mh-lists = ((all MHs pairing base-item arg #1 to a target-item arg)
;;;                (...base-item arg #2 to a target-item arg) (..#3..) ...)
;;;
(defun remove-from-relgroup (mh copies &aux mh-lists entry)
  (dolist (version copies)
    (dolist (sibling (cdr version))
       (unless (eq sibling mh)
	 (cond ((setq entry
		      (find-if #'(lambda (set)
				   (eq (mh-base-item sibling) (mh-base-item (car set))))
			       mh-lists))
		(unless (member sibling entry) (push sibling (cdr entry))))
	       ((push (list sibling) mh-lists))))))
  (permutations-for-rel-groups mh-lists))


;;; Merge step #1.
;;;  Given the root of a MH justification structure
;;;   return list of  (1: root MH of the structure  2: the MH's comprising the structure
;;;                    3: the Emaps caused by that structure)
;;;          ==> uses mh-mark slot <==
;;;
(defun collect-structure (root)
  (when (eq (mh-type root) :expression)
    (setf (mh-justifications root) nil) ;***Make current MH a root - kills ancestors that are Inconsistent(MH)
    (cond ((vector-intersection? (mh-descendants root) (mh-nogood root))
	   (mapcan #'collect-structure (mh-justifies root)))  ;root inconsistent-recurse on offspring
	  ((do ((queue (list root) (append (mh-justifies (first queue)) (cdr queue)))
		(mark (list nil))		;so can use "push" instead of "pushnew"
		(structure)
		(emaps))
	       ((null queue)  (list (list root structure emaps)))
	     (if (and (eq (mh-type (first queue)) :entity)
		      (not (eq (mh-mark (first queue)) mark)))
		 (push (first queue) emaps))	       ;save Emap
	     (unless (eq (mh-mark (first queue)) mark)
	       (push (first queue) structure))	      ;save all MHs
	     (setf (mh-mark (first queue)) mark))))))


;;; Merge steps #2 and #3.
;;;  Given a list of gmap defstructs, merge them together
;;;   where consistent to form the true *gmaps*
;;;
(defun merge-gmaps (structure-groups)
  (setq structure-groups (merge-base structure-groups))
  (cond (*Gmap-merge-step3?*	  ;Do MH conbination step number 3: all permutations
	 (setq *gmaps* (full-gmap-merge structure-groups)))
	((setq *gmaps* structure-groups))))


;;; This roughly merges all gmaps which intersect the same base structure.
;;;    It isn't complete (it doesn't check all roots,
;;;       see (member (car (expression-roots ...)))
;;;    since this would force subsumption checks.
;;;
;;;* Why do I do it like this? (6/8/88)
;;;    Why not (dolist (broot base-roots) (find #'intersects? gmaps)...)
;;;       => current is cheaper but incomplete (G1 keeps shrinking during "do")
;;;
(defun merge-base (groups &aux final-groups)
  (labels ((find-root-mh (gm) (car (gm-root-elements gm)))
	   (same-root (group1 group2)
	     (member (car (expression-roots (mh-base-item (find-root-mh group1)))) ;rough check
		     (expression-roots (mh-base-item (find-root-mh group2))))))
    ;; In the do, GROUPS are those gmaps we haven't considered yet - 
    ;; aren't part of a base structure we've looked at yet.
    ;; CURRENT-GROUPS are a set of gmaps which intersect the same base structure
    (do ((groups (remove-if #'(lambda (agroup) (same-root agroup (car groups))) (cdr groups))
		 (remove-if #'(lambda (agroup) (same-root agroup (car groups))) (cdr groups)))
	 (current-groups
	   (remove-if-not #'(lambda (agroup) (same-root agroup (car groups))) groups)
	   (remove-if-not #'(lambda (agroup) (same-root agroup (car groups))) groups)))
	((null current-groups)  final-groups)
      (setq final-groups (nconc final-groups (full-gmap-merge current-groups))))))


;;; Full, complete merging algorithm

;;; Define a temporary structure to hold a set of gmaps.  We delay
;;;   doing the actual union to form a true gmap until after we are
;;;   finished, since subsumption may mean throwing many of these away.
;;;
(defstruct (set-of-gmaps (:conc-name sg-))
  els nogoods gmaps)
  

(defun inconsistent? (gm1 gm2)
  (or (vector-intersection? (gm-elements-vector gm1) (gm-nogood gm2))
      (vector-intersection? (gm-elements-vector gm2) (gm-nogood gm1))))


;;; This is a HIGHLY tuned function - seemingly innocent (or apparently better)
;;;   changes could result in speed variations.
;;;
(defun full-gmap-merge (structure-groups &aux final-sets)
  (do ((gmap-sets (list structure-groups) (nconc (cdr gmap-sets) new))
       (new nil nil)
       (contra-set nil nil))
      ((null gmap-sets) (reduce-from-set-form final-sets))
    (do ((a-gmap-set (cdar gmap-sets) (cdr a-gmap-set))	;start with cdr of first set
	 (a-gmap (cadar gmap-sets) (cadr a-gmap-set))
	 (accum-els (fcopy (gm-elements-vector (caar gmap-sets))) ;init els to car of 1st set
		    (vunionf accum-els (gm-elements-vector a-gmap)))
	 (accum-ngs (fcopy (gm-nogood (caar gmap-sets)))     ;init nogoods from car of 1st set
		    (vunionf accum-ngs (gm-nogood a-gmap))))
	((null a-gmap)
	 (setq final-sets (delete-if #'(lambda (gm)
					 (vector-subset? (sg-els gm) accum-els))
				     final-sets))
	 (unless (member accum-els final-sets :test #'vector-subset? :key #'sg-els)
	   (push (make-set-of-gmaps :els accum-els   ;this member test assumes accum-els is 1st arg!*
				    :nogoods accum-ngs
				    :gmaps (car gmap-sets))
		 final-sets)))
      (when (or (vector-intersection? accum-els (gm-nogood a-gmap));when conflict (a-gmap is bad)
		(vector-intersection? (gm-elements-vector a-gmap) accum-ngs))
	(dolist (item (car gmap-sets))
	  (unless (or (eq item a-gmap)
		      (inconsistent? item a-gmap))
	    (push item contra-set)))
	(push (cons a-gmap contra-set) new)
	(push (delete a-gmap (car gmap-sets)) new)
	(return)))))


;;; hash version (doesn't seem to work yet..)
;;(defvar *gmap-set-store* (make-hash-table :test #'eq :size 1621))
;
;;(defun full-gmap-merge (structure-groups &aux final-sets tmp-bit-vector)
;;  (clrhash *gmap-set-store*)
;;  (do ((gmap-sets (list structure-groups) (nconc (cdr gmap-sets) new))
;;       (new nil nil)
;;       (contra-set nil nil))
;;      ((null gmap-sets) (reduce-from-set-form final-sets))
;;    (do ((a-gmap-set (cdar gmap-sets) (cdr a-gmap-set))             ;start with cdr of first set
;;	 (a-gmap (cadar gmap-sets) (cadr a-gmap-set))
;;	 (accum-els (fcopy (gm-elements-vector (caar gmap-sets)))   ;init els to car of first set
;;		    (vunionf accum-els (gm-elements-vector a-gmap)))
;;	 (accum-ngs (fcopy (gm-nogood (caar gmap-sets)))       ;init nogoods from car of 1st set
;;		    (vunionf accum-ngs (gm-nogood a-gmap))))
;;	((null a-gmap)
;;	 (setq final-sets (delete-if #'(lambda (gm)
;;					 (vector-subset? (sg-els gm) accum-els))
;;				     final-sets))
;;	 (unless (member accum-els final-sets :test #'vector-subset? :key #'sg-els)
;;	   (push (make-set-of-gmaps :els accum-els ;this member test assumes accum-els is 1st arg!*
;;				    :nogoods accum-ngs
;;				    :gmaps (car gmap-sets))
;;		 final-sets)))
;;      (when (or (vector-intersection? accum-els (gm-nogood a-gmap))  ;when conflict (a-gmap is bad)
;;		(vector-intersection? (gm-elements-vector a-gmap) accum-ngs))
;;	(dolist (item (car gmap-sets))
;;	  (unless (or (eq item a-gmap)
;;		      (inconsistent? item a-gmap))
;;	    (push item contra-set)))
;;	(push a-gmap contra-set)
;;	(setq tmp-bit-vector (union-gmap-vectors contra-set))
;;	(unless (member tmp-bit-vector (gethash (car tmp-bit-vector) *gmap-set-store*)
;;			:test #'equal)
;;	  (push tmp-bit-vector (gethash (car tmp-bit-vector) *gmap-set-store*))
;;	  (push contra-set new))
;;	(setq contra-set (delete a-gmap (car gmap-sets)))
;;	(setq tmp-bit-vector (union-gmap-vectors contra-set))
;;	(unless (member tmp-bit-vector (gethash (car tmp-bit-vector) *gmap-set-store*)
;;			:test #'equal)
;;	  (push tmp-bit-vector (gethash (car tmp-bit-vector) *gmap-set-store*))
;;	  (push contra-set new))
;;	(return)))))

(defun union-gmap-vectors (gmaps)     ;goes with hash version
  (labels ((reduce-gm (current rest)
	     (if rest
		 (reduce-gm (vunionf current (gm-elements-vector (car rest))) (cdr rest))
		 current)))
    (reduce-gm (fcopy (gm-elements-vector (car gmaps))) (cdr gmaps))))


;;; take a list of set-of-gmaps structs and reduce each set-of-gmaps to
;;;     a single gmap representing a union of its elements
;;;
(defun reduce-from-set-form (sets-of-gmaps &aux gmap final-gmaps)
  (dolist (gmap-set sets-of-gmaps)
    (setq gmap (make-global-mapping :elements (fcopy (gm-elements (car (sg-gmaps gmap-set))))
				:emaps (gm-emaps (car (sg-gmaps gmap-set)))
				:root-elements (gm-root-elements (car (sg-gmaps gmap-set)))))
    (dolist (next-mem (cdr (sg-gmaps gmap-set)))
       (setf (gm-elements gmap) (union (gm-elements gmap) (gm-elements next-mem) :test #'eq))
       (setf (gm-emaps gmap) (union (gm-emaps gmap) (gm-emaps next-mem) :test #'eq))
       (setf (gm-root-elements gmap)
	     (append (gm-root-elements gmap) (gm-root-elements next-mem))))
    (setf (gm-elements-vector gmap) (sg-els gmap-set))
    (setf (gm-nogood gmap) (sg-nogoods gmap-set))
    (push gmap final-gmaps))
  final-gmaps)


;;; Total up the weights supporting each gmap
;;;
(defun intern-gmaps (&aux group-nodes)
  (dolist (gmap *gmaps*)
    (setf (gm-bms-node gmap) (install-bms-node (list 'gmap gmap)))
    (push (gm-bms-node gmap) group-nodes))
  (additive-nodes group-nodes))	  ;declare evidence to be additive rather than dempster-shafer


;;;; Candidate Inference Gathering

;;; For each GMap, find its candidate inferences.  A candidate inference
;;;   must be an unclaimed part of a base structure where the base structure
;;;   must intersect the GMap somewhere. The early intersect test here saves
;;;   lots of time & consing on big problems (sys-lit).
;;;
;;; Notice that its taking (gm-root-elements ...), so it's more constrained than
;;;          just structure intersection -- possible bug!!!!!
;;;
;;;     See note below for SME (V. 1) CI generator.
;;;
(defun gather-inferences (base target)
  (dolist (gmap *gmaps*)
    (setf (getf (gm-plist gmap) :ci-orders) nil)    ;simple exp. orders list for Janice's S.E.
    (dolist (root (dgroup-roots base))
      (when (and (not (member root (gm-elements gmap) :key #'mh-base-item))  ;full structure already
		 (some #'(lambda (mh) (member root (expression-roots (mh-base-item mh))))
		       (gm-root-elements gmap)))  ;root's exp tree must intersect the gmap somewhere
	(multiple-value-bind (ci order)
	    (gather-inf root gmap target)
	  (when ci
	    (push ci (gm-inferences gmap))
	    (push order (getf (gm-plist gmap) :ci-orders))))))))

;;; For a given base root, return the corresponding candidate inference expression.
;;;    This is returned in a format which could then be directly installed into
;;;    the Target if desired by calling (expression ci).  Thus known target
;;;    expressions are replaced by their expression names and entities are
;;;    replaced by their entity names.
;;;
;;;    (values ci abort?)
;;;
(defun gather-inf (root gmap target &aux ci mh ent current-order)
  (cond ((entity? root)
	 (values (or (if (setq ent (third (find root (gm-emaps gmap) :key #'cadr)))
			 (entity-name ent))  ;ARG!  Why don't they let us struct access nil?
		     (if (paired-item? :base-item (entity-name root))
			 (second (assoc (entity-name root) *given-entity-pairings*)))
		     (if (constant-entity? (entity-name root))
			 (entity-name root))
		     (list ':SKOLEM (entity-name root)))
		 0
		 nil))
	((setq mh (find root (gm-elements gmap) :key #'mh-base-item))
	 (values (expression-name (mh-target-item mh))
		 (expression-order (mh-target-item mh)) nil))
	(t (setq ci (list (expression-functor root)))
	   (setq current-order 0)
	   (dolist (arg (expression-arguments root))
	      (multiple-value-bind (arg-ci child-order abort?)
		  (gather-inf arg gmap target)
		(cond (abort? (return-from gather-inf (values nil nil t)))
		      (t (push arg-ci ci)
			 (if (> child-order current-order)
			     (setq current-order child-order))))))
	   (setq ci (nreverse ci))
	   (if (contradictory? ci target)
	       (values nil nil t)
	       (values ci (1+ current-order) nil)))))


;;;--------------- SME (V. 1) CI Generator -------------------------
;;; The original CI generator, as describd in all papers, will take any
;;;   base structure "intersecting the gmap structure". The newer (V. 2)
;;;   edition only takes base structure "intersecting a Gmap root".
;;;   Thus, the newer edition is more cautious and far more efficient
;;;   than the older edition.
;;;
'''''(defun gather-inferences (base target)
  (dolist (gmap *gmaps*)
    (dolist (root (dgroup-roots base))
      (unless (member root (gm-elements gmap) :key #'mh-base-item)
	(multiple-value-bind (ci flag)
	    (gather-inf root gmap target)
	  (if flag (push ci (gm-inferences gmap))))))))


;;; (values ci flag abort?)
;;;
'''''(defun gather-inf (root gmap target &aux flag ci mh ent)
  (cond ((entity? root)
	 (values (or (if (setq ent (third (find root (gm-emaps gmap) :key #'cadr)))
			 (entity-name ent)) ;ARG!  Why don't they let us do struct access on nil?
		     (if (constant-entity? (entity-name root))
			 (entity-name root))
		     (list ':SKOLEM (entity-name root)))
		 nil nil))
	((setq mh (find root (gm-elements gmap) :key #'mh-base-item))
	 (values (expression-name (mh-target-item mh)) t nil))
	(t (setq ci (list (expression-functor root)))
	   (dolist (arg (expression-arguments root))
	     (multiple-value-bind (arg-ci arg-flag abort?)
		 (gather-inf arg gmap target)
	       (cond (abort? (return-from gather-inf (values nil nil t)))
		     (t (if arg-flag (setq flag t))
			(push arg-ci ci)))))
	   (setq ci (nreverse ci))
	   (if (contradictory? ci target)
	       (values nil nil t)
	       (values ci flag nil)))))
;;;------------------------------------------------------------------


;;; For now, a contradictory CI uses a non-commutative predicate and
;;;   permutes the arguments of a known target expression.
;;;
;;;*  This needs to be cleaned up!!!!  Problem having to do with an entity
;;;    being an instance or a type.
;;;
(defun contradictory? (exp-form dgroup &aux sys-lisp-form)
  (unless (commutative? (car exp-form))
    (let ((predicate-instances (remove-if-not #'(lambda (dg-exp)
						  (eq (expression-functor dg-exp)
						      (car exp-form)))
					      (dgroup-expressions dgroup))))
      (dolist (instance predicate-instances)
	(setq sys-lisp-form (expression-sys-lisp-form instance))
	(when (and (not (equal exp-form sys-lisp-form))
		   (null (set-difference (cdr exp-form)
					 (cdr sys-lisp-form))))
	  (return-from contradictory? t))))))


;;; Skolemized variables' utitlies
;;;
(defmacro skolem? (form)
  `(and (consp ,form) (eq (car ,form) ':SKOLEM)))

(defmacro skolem-token (form)
  `(second ,form))



;;;; Match Hypothesis Creation Rules

;;; Match Hypothesis Construction rule macro
;;;
;;;     format:  (MHC-rule  (condition ?base-var ?target-var { :test form } )
;;;                      body)
;;;        where condition element-of { :intern :filter }
;;;             :intern rules are run on each new match hypothesis
;;;             :filter rules are run for each pair of base/target predicate combinations
;;;        :test form is an optional form which, if nil, will block execution of the rule
;;;

;;;
(defmacro MHC-rule (trigger &rest body)
  (multiple-value-bind (rule-function insert-form)
       (build-MHC-rule trigger body)
    `(progn ,rule-function ,insert-form)))


(defun build-MHC-rule (trigger body &aux fname body-function)
  (let ((condition (first trigger))
	(vars (list (second trigger) (third trigger)))
	(test (second (member ':TEST (cdddr trigger)))))
    (if (not (member condition '(:FILTER :INTERN)))
	(error "~%~A is an invalid condition in an MHC rule." condition))
    (setq fname (intern (format nil "MHC-~A-RULE-~A-~A" condition
				(incf *mhc-rule-count*)
				*dynamic-rule-file-identifier*)))
    (setq body-function `(defun ,fname ,vars
			   ,@ (if test
				  (list `(when ,test ,@ body))
				  body)))
    (values body-function `(push ',fname ,(if (eq condition :INTERN)
					      '*mhc-intern-rules*
					      '*mhc-filter-rules*)))))

;;; Why is this a macro? => we need to get the file-name string at compile
;;;   time so that rule function names will have this as part of their name.
;;;   This avoids collisions on function names.
;;;
(defmacro sme-rules-file (file-name-string)
  (setq *dynamic-rule-file-identifier* file-name-string)   ;compile-time file-name access
  `(progn (tre-rules-file ,file-name-string) ;TRE needs some things initialized to save original rules
	  (setq *mhc-intern-rules* nil)
	  (setq *mhc-filter-rules* nil)
	  (setq *match-rules-file* ,file-name-string)))


;;;; Miscellaneous Functions


;;; Given a base expression or entity structure and optionally the same for a target item
;;;    return:   a list of all MH's for that base expression if no target expression is given
;;;             or the specific MH corresponding to the given base and target expressions
;;;
(defun fetch-MH (base-item &optional target-item)
  (if target-item
      (gethash (list 'MH base-item target-item) *mh-hash-table*)  ;get the unique MH
      (gethash (cons base-item 'base) *mh-hash-table*)))     ;get all MH's for this base-item


;;; Return all MH's for the given target item
;;;
(defun fetch-MH-target (target-item)
    (gethash (cons target-item 'target) *mh-hash-table*))


;;; Select the best Gmap(s) within a percentage range of equality
;;;
(defun best-gmaps (&optional (gmaps *gmaps*) (percentage-range 0.02))
  "VALUES: best-gmaps highest-score"
  (let ((best-value 0.0)
	lower-bound)
    (dolist (gm gmaps)				;opt for a simple two-pass approach
      (if (>  (node-belief+ (gm-bms-node gm))  best-value)
	  (setq best-value (node-belief+ (gm-bms-node gm)))))
    (setq lower-bound (- best-value (* best-value percentage-range)))
    (values (remove-if #'(lambda (gm) (<  (node-belief+ (gm-bms-node gm))  lower-bound))
		       gmaps)			;return those >= lower-bound
	    best-value)))


;;; Convert a Gmap's (MH base-entity target-entity) form to a-list form:
;;;    (base-entity . target-entity)
;;;
(defun emap-alist (gmap)
  (mapcar #'(lambda (emap)
	      (cons (entity-name (second emap))
		    (entity-name (third emap))))
	  (gm-emaps gmap)))
