;;; -*- Mode: LISP; Syntax: Common-lisp; Package: SME; Base: 10; Fonts: CPTFONT,TR12I -*-
#-Symbolics
(in-package "SME" :use (list sme::*the-lisp-package*))

;;;; Belief Maintenence System (subset specialized for SME)
;;;
;;; Copyright (C) 1986, Brian C. Falkenhainer
;;;
;;;     derived from Ken Forbus' justification-based truth maintenence system
;;;

(defvar *propagation-delta* 1.0d-3  "belief changing less than this amount = no change")
(defvar *belief-threshold* 0.90 "believe if the belief for/against is greater than threshold")

(defvar *round-off-error* 1.0d-9 "allow for slight double-precision round-off error")

(defvar *bms-node-identifier* (cons 1 nil) "next available bit vector identifier for a node")

(defvar *link-counter* 0)
(defvar *nodes* nil)
(defvar *links* nil)
(defvar *debug-bms* nil)

(defstruct (bms-node (:conc-name node-)
		     (:predicate bms-node?)
		     (:print-function (lambda (n st ignore)
					(format st "#<BMS ~A>" (node-datum n)))))
  datum				; Pointer to external problem solver
  (belief+ 0.0d0)		; support-for
  (belief- 0.0d0)		; support-against
  supporters			; list of ALL links providing support for and/or against this node
  justifications		; supports causing absolute belief for or against (depending on
				;  belief+/-). This is a list of only those links providing
				;  absolute evidence and must therefore be nil if belief+ or belief-
				;  are not equal to one.
  consequences			; supports that use this node
  identifier			; bit-vector identifier for this node
  ante-ids			; identifiers for all antecedents - all nodes eventially giving
				;  support for this node
  mark				; marker for sweep algorithms
  plist)			; property list for misc things


(defstruct (support-link (:conc-name link-)
			 (:predicate support-link?)
			 (:print-function
			   (lambda (j st ignore)
			     (format st "#<LINK ~A>" (link-index j)))))
  (index 0)			
  type				; The justification type (e.g. implication, conjunction, etc.)
  function			; The link's lambda function used to return the new consequent support
  invert?			; Is this an evidence link (requiring retraction) or is this a hard link
  premise?			; Is this support a premise?
  antecedents			; List of antecedent bms nodes
  consequent			; The consequent bms node
  (evidence+ 0.0d0)		; The support this link provides for its consequent
  (evidence- 0.0d0)		; The support this link provides against its consequent
  mark)				; marker for sweep algorithms


;;; This gets called by tre-init
;;;
(defun bms-init ()
  (setq *nodes* nil)
  (setq *links* nil)
  (setq *link-counter* 0)
  (setq *bms-node-identifier* (cons 1 nil)))


;;;; User query hooks

(defmacro ~zero (num)
  `(< (abs ,num) *round-off-error*))

(defmacro ~= (num1 num2 &optional (error '*round-off-error*))
  `(< (abs (- ,num1 ,num2)) ,error))


(defmacro true-node? (node)
  `(> (node-belief+ ,node) *belief-threshold*))

(defmacro false-node? (node)
  `(> (node-belief- ,node) *belief-threshold*))

(defmacro unknown-node? (node)
  `(and (< (node-belief+ ,node) *belief-threshold*)
	(< (node-belief- ,node) *belief-threshold*)))


(defmacro support-for-node (node)
  `(node-belief+ ,node))

(defmacro support-against-node (node)
  `(node-belief- ,node))

(defmacro possible-true-node (node)
  `(- 1.0 (node-belief- ,node)))

(defmacro possible-false-node (node)
  `(- 1.0 (node-belief+ ,node)))

(defmacro node-belief-uncertainty (node)
  `(- 1.0 (node-belief- ,node) (node-belief+ ,node)))


(defmacro absolutely-unknown-node? (node)
  `(and (~zero (node-belief+ ,node))
	(~zero (node-belief- ,node))))

(defmacro absolutely-true-node? (node)
  `(~= (node-belief+ ,node) 1.0))

(defmacro absolutely-false-node? (node)
  `(~= (node-belief- ,node) 1.0))


;;;; User data base hooks

;;; return the bms node for the given form if it exists. Preferably don't use this
;;;   function. Use the supplied database hook - e.g., referent-bms-node.
;;;
(defun fetch-node (form)
  (find form *nodes* :test #'equal :key #'node-datum))

;;; return all bms nodes that unify with the given pattern
;;;
(defun fetch-all-nodes (pattern &aux bindings nodes)
  (dolist (candidate *nodes* nodes)
    (setq bindings (unify pattern (node-datum candidate)))
    (unless (eq bindings 'FAIL)
      (push candidate nodes))))


;;; if it doesn't already exist, add a bms node for the given datum
;;;
(defun install-bms-node (datum)
  (let ((node (referent-bms-node datum)))
    (unless node
      (setq node (make-bms-node :belief+ 0.0d0
				:belief- 0.0d0
				:datum datum
				:identifier (next-identifier *bms-node-identifier*)))
      (push node *nodes*)
      (insert datum node))      ;Rule-engine support - notify it that a new bms node is being installed
    node))


;;; Add or modify a premise support link for the given node.  Premise support links have no antecedent.
;;;   There can only be one per node
;;;
(defun install-premise (node &key (reason 'user) (support+ 1.0d0) (support- 0.0d0))
  (let ((link (find-if #'link-premise? (node-supporters node))))
    (cond (link (when (not (and (~= support+ (link-evidence+ link))
				(~= support- (link-evidence- link))))
		  (recognize-support link support+ support- t)
		  (format t "~%  [Changing belief in premise ~A to (~,3F, ~,3F)]"
			  node
			  (node-belief+ node) (node-belief- node))))
	  (t (setq link (make-support-link :index (incf *link-counter*)
					   :type reason
					   :invert? t
					   :premise? t
					   :consequent node))
	     (if *debug-bms* (format t "~%   DBG: Making ~A a premise (~A)." node reason))
	     (push link (node-supporters node))
	     (recognize-support link support+ support-)))
    node))


;;; Creates a bms support link and causes the new support to be merged into the
;;;   consequent node's current belief
;;;
(defun install-support (type supporters conseq function invert?)
  (if (some #'(lambda (ante) (vector-intersection? (node-identifier conseq)
						   (node-ante-ids ante)))
	    supporters)
      (error "Circular support structure discovered entering ~A" conseq)
      (propagate-circular-check supporters conseq))
  (let ((link (make-support-link :index (incf *link-counter*)
				 :type type
				 :function function
				 :invert? invert?
				 :antecedents supporters
				 :consequent conseq)))
    (push link (node-supporters conseq))
    (dolist (node supporters) (push link (node-consequences node)))
    (push link *links*)
    (if *debug-bms* (format t "~%   DBG: Justifying ~A by ~A using ~A."
			    conseq  type  supporters))
    (multiple-value-bind (evidence-for evidence-against)
	(funcall (link-function link) link)
      (recognize-support link evidence-for evidence-against))))


(defun propagate-circular-check (antecedents consequent &aux new-stuff)
  (labels ((propagate (bits node)
	     (setf (node-ante-ids node) (vunionf (node-ante-ids node) bits))
	     (dolist (clink (node-consequences node))
	       (propagate bits (link-consequent clink)))))
    (dolist (ante antecedents)
      (setq new-stuff (vunionf new-stuff (node-identifier ante)))
      (setq new-stuff (vunionf new-stuff (node-ante-ids ante))))
    (propagate new-stuff consequent)))



;;;; Belief maintenance routines

;;; Recognize a node's new support link.  If invert?, the link is being modified,
;;;   so its old support must be retracted.
;;; 
(defun recognize-support (link e+ e- &optional (invert? nil))
  (let ((node (link-consequent link)))
    (cond ((discernment-link? link)
	   (if invert?
	       (error "Evidence for a frame of discernment cannot be modified: ~A" node))
	   (setf  (link-evidence+ link) e+   (link-evidence- link) e-)
	   (run-discernment-link link nil))
	  (t (if invert? (retract-support link))
	     (setf  (link-evidence+ link) e+   (link-evidence- link) e-)
	     (add-node-belief link)
	     (propagate-belief node)))))


;;; Add the evidence "link" provides for and against its consequent
;;;   If the evidence is absolutely for or against the consequent,
;;;       add the link to the consequent's justifications list
;;;   If the evidence is merely partial support (invert?), add in the evidence using Dempster's rule
;;;   If the evidence is to be the only source of support (as in the case of "and"),
;;;      simply set the consequent's new value
;;;   If the link causes less than *propagation-delta* change in its consequent's belief, return :moot
;;;
(defun add-node-belief (link &optional (old+ -1) (old- -1))
  (let ((node (link-consequent link)))
    (cond ((= (link-evidence+ link) 1.0)	           ;absolute support for
	   (cond ((absolutely-false-node? node)
		  (signal-contradiction node link))
		 (t (pushnew link (node-justifications node) :test #'eq)
		    (setf (node-belief+ node) 1.0d0
			  (node-belief- node) 0.0d0))))
	  ((= (link-evidence- link) 1.0)	           ;absolute support against
	   (cond ((absolutely-true-node? node)
		  (signal-contradiction node link))
		 (t (pushnew link (node-justifications node) :test #'eq)
		    (setf (node-belief+ node) 0.0d0
			  (node-belief- node) 1.0d0))))
	  ((link-invert? link)			           ;add evidence with dempster's rule
	   (multiple-value-bind (support-for support-against)
	       (dempster (link-evidence+ link) (link-evidence- link) node)
	     (setf (node-belief+ node) support-for
		   (node-belief- node) support-against)))
	  (t (setf (node-justifications node)              ;set the belief to the evidence
		   (delete link (node-justifications node) :test #'eq))
	     (setf (node-belief+ node) (link-evidence+ link)
		   (node-belief- node) (link-evidence- link))))
    (when (and (~= (node-belief+ node) old+ *propagation-delta*)
	       (~= (node-belief- node) old- *propagation-delta*))
      (if *debug-bms* (format t "~%   DBG: Add-node-belief moot ~A to ~A"
			      (link-type link) (node-datum node)))
      :moot)))					;indicate if the link changed the node's belief




;;; Propagate the effects of the current belief in the given node
;;;
(defun propagate-belief (node)
  (if *debug-bms* (format t "~%   DBG: Propagating belief in ~A..." node))
  (do ((queue (copy-list (node-consequences node))
	      (nconc (cdr queue) new))
       (new nil nil))
      ((null queue))
    (unless (eq (run-link (car queue)) :moot)	;only propagate if the link represents a change
      (setq new (copy-list (node-consequences (link-consequent (car queue))))))))
	


;;; Run a support link by re-calculating the evidence supplied by that link
;;;   If the support link provides partial evidence (invert?), retract the old evidence it supplied before
;;;    calculating the new evidence
;;; 
(defun run-link (link)
  (if *debug-bms* (format t "~%   DBG: Running link ~A to ~A"
			  (link-type link) (node-datum (link-consequent link))))
  (if (discernment-link? link)
      (run-discernment-link link)          ;special treatment for f-o-d's
      (let ((old+ (node-belief+ (link-consequent link)))	;save the consequent's old beliefs
	    (old- (node-belief- (link-consequent link))))
	(multiple-value-bind (evidence-for evidence-against)	;get evidence this link now supplies
	    (funcall (link-function link) link)
	  (cond ((and (~= evidence-for (link-evidence+ link))	;the change in the link is moot
		      (~= evidence-against (link-evidence- link)))
		 (if *debug-bms* (format t "  ...moot"))
		 :moot)
		(t (if  (link-invert? link)  (retract-support link))	;retract the old support
		   (setf (link-evidence+ link) evidence-for
			 (link-evidence- link) evidence-against)
		   (add-node-belief link old+ old-)))))))  ;supply evidence to the link's consequent



;;; Retract the support a link provides its consequent
;;;    Assumes link is not a discernment link for efficiency.  This must be checked by higher-level functions
;;;
(defun retract-support (link)
  (let ((node (link-consequent link)))
    (if *debug-bms* (format t "~%   DBG: Retracting ~A support for ~A" (link-type link) node))
    (cond ((and (~zero (link-evidence+ link))	        ;do nothing - no evidence to retract
		(~zero (link-evidence- link)))
	   (if *debug-bms* (format t "  ...moot")))
	  ((or (= (link-evidence+ link) 1.0)            ;retracting an absolute
	       (= (link-evidence- link) 1.0))
	   (setf (node-justifications node)
		 (delete link (node-justifications node) :test #'eq))
	   (if (null (node-justifications node))
	       (recalculate-belief node (list link))))
	  ((multiple-value-bind (support-for support-against)   ;retracting a partial support
	       (inverted-dempster (link-evidence+ link) (link-evidence- link) node)
	     (setf (node-belief+ node) support-for)
	     (setf (node-belief- node) support-against))))))


;;; Recalculate a node's belief from scratch.
;;;   Ignore support from the exclusion-set to allow for retraction support.
;;;
(defun recalculate-belief (node exclusion-set)
  (reset-node-belief node)
  (dolist (link (node-supporters node))
    (unless (member link exclusion-set :test #'eq)
      (add-node-belief link))))


;;; Reset the belief in the given node to its no-evidence, a-priori state.
;;;
(defun reset-node-belief (node)
  (setf (node-belief+ node) 0.0d0
	(node-belief- node) 0.0d0))


;;;; Premise manipulations

;;; Is this node a premise - i.e. does it have a premise support-link?
;;;
(defun premise? (node)
  (some #'link-premise? (node-supporters node)))


;;; Is this node justified (i.e. given 100% support for or against) by a premise?
;;;
(defun premise-justified? (node)
  (some #'link-premise? (node-justifications node)))


;;; Return a list of all premise nodes supporting the given node.
;;;
(defun get-premises (node &aux premises nodes)
  (if (premise? node) (push node premises))
  (dolist (support (node-supporters node))
    (if (not (link-premise? support))
	(setq nodes (nconc (copy-list (link-antecedents support)) nodes))))
  (do ((nodes nodes (nconc (cdr nodes) new))
       (new nil nil)
       (marker (list nil)))
      ((null nodes) premises)
    (unless (eq (node-mark (car nodes)) marker)
      (setf (node-mark (car nodes)) marker)
      (if (premise? (car nodes))
	  (push (car nodes) premises)                           ;this one's a premise
	  (dolist (support (node-supporters (car nodes)))	;check its supporters
	    (setq new (nconc (copy-list (link-antecedents support)) new)))))))	;efficiency


;;; Print out the premises supporting the given node.
;;;
(defun premises-node (node)
  (if (absolutely-unknown-node? node)
      (format t "~%~A is unknown." node)
      (dolist (node (get-premises node))
	(format t "~%  ~A, from ~A" node
		(link-type (find-if #'link-premise? (node-supporters node))))))
  node)


;;;; Forgetting


;;; Forgets the premise support-link for the given node.  If evidence for the node only comes from a premise,
;;;  then all belief in the node is effectively forgotten.  It will not work for non-node type of structures,
;;;  such as implies.
;;;
(defun forget-premise (node)
  (when (premise? node)
    (if *debug-bms* (format t "~%   DBG: Retracting premise ~A." node))
    (let ((plink (find-if #'link-premise? (node-supporters node))))
      (retract-support plink)
      (setf (node-supporters node) (delete plink (node-supporters node)))
      (propagate-belief node))))


;;;; Interogatives

(defun why-nodes (&optional (stream *standard-output*))
  (dolist (node *nodes*) (why-node node stream)))


(defun why-node (node &optional (stream *standard-output*))
  (cond ((absolutely-unknown-node? node)
	 (format stream "~%~A is unknown" (node-datum node)))
	((and (absolutely-true-node? node) (premise-justified? node))
	 (format stream "~%~A is a premise." (node-datum node)))
	((absolutely-true-node? node)
	 (format stream "~%~A is true" (node-datum node))
	 (dolist (link (node-justifications node))
	   (format stream "~%   via ~A on  {"  (link-type link))
	   (dolist (anode (link-antecedents link))
	     (format stream "  ~A" (node-datum anode)))
	   (format stream "  }")))
	((and (absolutely-false-node? node) (premise-justified? node))
	 (format stream "~%NOT(~A) is a premise." (node-datum node)))
	((absolutely-false-node? node)
	 (format stream "~%NOT(~A) is true" (node-datum node))
	 (dolist (link (node-justifications node))
	   (format stream "~%   via ~A on  {" (link-type link))
	   (dolist (anode (link-antecedents link))
	     (format stream "  ~A" (node-datum anode)))
	   (format stream "  }")))
	(t (format stream "~%~A has evidence (~,4F, ~,4F) due to" (node-datum node)
		   (node-belief+ node) (node-belief- node))
	   (dolist (link (node-supporters node))
	     (format stream "~%   ~A~A  (~,4F, ~,4F)" (link-type link)
		     (or (mapcar #'node-datum (link-antecedents link)) "")
		     (link-evidence+ link)
		     (link-evidence- link))))))


;;;; Contradiction processing

(defvar *contra-premises* nil)


(defun signal-contradiction (node link &aux the-answer)
  (setq *contra-premises* (get-premises node))
  (format t "~%~A~A causes a contradiction with Node ~A"
	  link  (or (mapcar #'node-datum (link-antecedents link)) (link-type link))
	  (node-datum node))
  (print-contra-list *contra-premises*)
  (format t "~%Please call BMS-ANSWER to retract")
  (setq the-answer
	(catch 'bms-contradiction-handler
	  (break "~%BMS contradiction break")))
  (if (and (integerp the-answer)
	   (> the-answer 0)
	   (not (> the-answer (length *contra-premises*))))
      (forget-premise (nth (1- the-answer)
			   *contra-premises*))))


(defun print-contra-list (nodes)
  (do ((counter 1 (1+ counter))
       (nn nodes (cdr nn)))
      ((null nn))
    (format t "~%~A ~A from ~A" counter (car nn)
	    (link-type (find-if #'link-premise? (node-supporters (car nn)))))))

(defun bms-answer (num)
  (if (integerp num)
      (if (> num 0)
	  (if (not (> num (length *contra-premises*)))
	      (throw 'bms-contradiction-handler num)
	      (format t "~%Ignoring answer, too big"))
	  (format t "~%Ignoring answer, too small"))
      (format t "~%Ignoring answer, must be an integer.")))


;;;; Dempster's Orthogonal Sum

;;; Apply the Dempster rule of combination to two Shafer represented probability intervals.
;;;       The Shafer intervals are given by:  ( s(A)  .  s(~A) )
;;;          where s(A) is a probability (0-1) value representing the support present for proposition A
;;;             and s(~A) is a probability (0-1) value representing the support present for proposition ~A
;;;
(defun dempster (efor eagainst node) "return new values ( s(A) , s(not A) )"
  (let ((a efor)
	(b eagainst)
	(c (node-belief+ node))
	(d (node-belief- node))
	for against normalize)
    (setq normalize (- 1.0d0 (+ (* a d) (* b c))))
    (setq for (- 1.0d0 (/ (* (- 1.0d0 a) (- 1.0d0 c))  normalize)))
    (setq against (- 1.0d0 (/ (* (- 1.0d0 b) (- 1.0d0 d))  normalize)))
    (when (> (+ for against) 1.0d0)
      (setq for (- for (- (+ for against) 1.0d0)))
      (setq against (- against (- (+ for against) 1.0d0))))
   (values for against)))


;;; Use an inverted form of Dempster's rule to retract previously given evidence
;;;    node-belief = old-node-belief + (efor, egainst)       -normal Dempster   (a b) + (c d)
;;;    old-node-belief = node-belief - (efor, egainst)       -inverted Dempster  (a b) - (c d)
;;;
(defun inverted-dempster (efor eagainst node)
  (let ((a (node-belief+ node))
	(b (node-belief- node))
	(c efor)
	(~c (- 1.0d0 efor))
	(d eagainst)
	for against normalize)
    (setq normalize (- (* ~c (- 1.d0 d)) (* (- 1.d0 b) c ~c) (* (- 1.d0 a) d (- 1.d0 d))))
    (setq for (/ (* ~c (- (* a (- 1.d0 d)) (* (- 1.d0 b) c))) normalize))
    (setq against (/ (* (- 1.d0 d) (- (* b ~c) (* (- 1.d0 a) d))) normalize))
    (when (> (+ for against) 1.d0)
      (setq for (- for (- (+ for against) 1.d0)))
      (setq against (- against (- (+ for against) 1.d0))))
    (if *debug-bms*
	(format t "~%   DBG: (~,4F, ~,4F)-(~,4F, ~,4F)=(~,4F, ~,4F)" a b c d for against))
    (values for against)))



;;;; Special nodes (e.g., frames of discernment, or additive evidence nodes
;;;   In the original BMS, this section supported frame-of-discernment functions only.
;;;   For SME, new "additive" nodes were added whose evidence is added rather than
;;;   using the simplified or full dempster-shafer formulas. I still keep much of the
;;;   "discernment" terminology, however.

;;; does this link provide support for a node which is part of a frame-of-discernment.
;;;
(defun discernment-link? (link)
  (getf (node-plist (link-consequent link)) :discernment-node))


;;; Run a support link for a "special" node (where discernment-link is the blanket term for both kinds),
;;;   by re-calculating the evidence supplied by that link.
;;;  Support links for special nodes can only be run once
;;;     (since the full dempster formula is not (currently) invertable).
;;;
(defun run-discernment-link (link &optional (evaluate? t))
  (let ((link-evidence-supply-function (getf (node-plist (link-consequent link))
					     :discernment-link-function)))
    (cond ((not evaluate?) (funcall link-evidence-supply-function link))
	  ((and (= 0.0 (link-evidence+ link)) (= 0.0 (link-evidence- link)))
	   (multiple-value-bind (evidence-for evidence-against)
	       (funcall (link-function link) link)
	     (cond ((and (= evidence-for 0.0) (= evidence-against 0.0))	;change in link is moot
		    :moot)
		   (t (setf (link-evidence+ link) evidence-for
			    (link-evidence- link) evidence-against)
		      (funcall link-evidence-supply-function link)))))
	  ((error "evidence links into a discernment-node may only be run once: ~A" link)))))


;;; Additive nodes

;;; Rather than run Dempster's orthogonal sum, run sum (for SME....!)
;;;
(defun run-additive-link (link)
  (incf (node-belief+ (link-consequent link)) (link-evidence+ link)))

;;; Create a set of special "additive" nodes out the given list of bms nodes
;;;
(defun additive-nodes (nodes)
  (dolist (node nodes)
    (setf (getf (node-plist node) :discernment-node) t)
    (setf (getf (node-plist node) :discernment-link-function) 'run-additive-link)
    (if (not (absolutely-unknown-node? node))
	(error "node with prior belief status given to make-frame: ~A" (node-datum node)))))


;;; Frame of Discernment nodes  (these don't work in the SME version - extra overhead. Call if you need it).


(defstruct (discernment-node (:conc-name theta-)
			     (:predicate theta?)
			     (:print-function (lambda (th str ignore)
						(format str "THETA-node"))))
  values			;vector holding current evidence values for the members of this frame
  members			;a list of the bms nodes which are members of this frame-of-discernment
  bms-node)			;the bms node this is the datum for


;;; return the discernment structure the node is a member of.
;;;
(defun discernment-theta (node)
  (getf (node-plist node) :discernment-node))


;;; Create a frame-of-discernment out the given list of bms nodes
;;;
(defun frame-of-discernment (nodes)
  (let* ((dim (1+ (length nodes)))
	 (i 0)
	 (theta (make-discernment-node :members nodes
				       :values (make-array (list dim)
							   :initial-element 0.0d0)))
	 (th-node (install-bms-node (list 'theta theta) 1.0d0)))
    (setf (theta-bms-node theta) th-node)
     ;assign each member a unique position
    (dolist (node nodes)
      (setf (getf (node-plist node) :theta-index) (incf i))
      (setf (getf (node-plist node) :theta-bitset) (expt 2 i))
      (setf (getf (node-plist node) :discernment-node) theta)
      (if (not (absolutely-unknown-node? node))
	  (error "node with prior belief status given to make-frame: ~A" (node-datum node))))
     ;assign theta its position
    (setf (getf (node-plist th-node) :discernment-node) theta)
    (setf (getf (node-plist th-node) :discernment-link-function) 'run-full-dempster)
    (setf (getf (node-plist th-node) :theta-index) 0)
    (setf (getf (node-plist th-node) :theta-bitset) -1)
    (setf (svref (theta-values theta) 0) 1.0d0)
    theta))


;;; Add in the link's evidence for its consequent using dempster's orthogonal sum,
;;;  taking care of any resulting changes in the belief of other members of the frame of discernment.
;;;
(defun run-full-dempster (link)
  (let* ((node (link-consequent link))
	 (theta (discernment-theta node))
	 (theta-node (theta-bms-node theta)))
    (full-dempster  (cons theta-node (theta-members theta))  link  (theta-values theta))
    (dolist (a-node (theta-members theta))       ;look for changes in the other members of the frame
      (when (and (not (eq a-node node))
		 (/= (svref (theta-values theta) (getf (node-plist a-node) :theta-index))
		     (node-belief+ a-node)))
	(setf (node-belief+ a-node)
	      (svref (theta-values theta) (getf (node-plist a-node) :theta-index)))
	(propagate-belief a-node)))
    (setf (node-belief+ node)                    ;make the change in the link's consequent
	  (svref (theta-values theta) (getf (node-plist node) :theta-index)))
    (setf (node-belief+ theta-node) (svref (theta-values theta) 0))))


;;; Add the evidence provided by "link" to the frame-of-discernment whose elements are listed in m1.
;;;   each element is a bms node having a unique bit number assigned to it and a bitset of (expt 2 bit-num)
;;;   m is an array of length |m1| whose elements correspond to the new values for m1.
;;;
(defun full-dempster (m1 link m)
  (let ((normalize 0.0d0)
	(m2 (list (cons (getf (node-plist (link-consequent link)) :theta-bitset)
			(link-evidence+ link))                ;m2 member
		  (cons -1 (- 1.0 (link-evidence+ link)))))   ;m2's theta
	(sum 0.0d0))
    (fill m 0.0d0)				;clear new values
    (dolist (m1-item m1)
      (dolist (m2-item m2)
	(if (zerop (logand (getf (node-plist m1-item) :theta-bitset)
			   (car m2-item)))
	    (incf normalize (* (node-belief+ m1-item) (cdr m2-item))))
	(dolist (m-item m1)
	  (if (= (getf (node-plist m-item) :theta-bitset)
		 (logand (getf (node-plist m1-item) :theta-bitset)
			 (car m2-item)))
	      (incf (svref m (getf (node-plist m-item) :theta-index))
		    (* (node-belief+ m1-item) (cdr m2-item)))))))
    (setq normalize (- 1.0d0 normalize))
    (dotimes (i (array-dimension m 0))		;normalize the new values
      (when (> i 0)
	(setf (svref m i) (/ (svref m i) normalize))
	(incf sum (svref m i))))
    (setf (svref m 0) (- 1.0d0 sum))))          ;prevent precision problems



;;;; Translating sme-assertions to Internal Form (using D-S uncertainty calculus)


;;; sme-assert the given form according to its logic parse.   Return the bms node if created, nil otherwise.
;;;  (An implies will not create a bms node for the implies itself)
;;;
(defun bms-sme-assert (form &optional (belief+ 1.0) (belief- 0.0) &aux node)
  (cond ((symbolp form)
	 (setq node (install-bms-node form))
	 (install-premise node :support+ belief+ :support- belief-)
	 node)
	((consp form)
	 (case (car form)
	   (implies (bms-implies form))
	   (and (make-and form t))
	   (or (make-or form t))
	   (not (bms-sme-assert (second form) belief- belief+))
	   (t (setq node (install-bms-node form))
	      (install-premise node :support+ belief+ :support- belief-)
	      node)))))

;;; Parse the logic of form, returning its resulting bms node and the functions to access
;;;   the original form's + and - belief.
;;;
(defun parse-sme-assertion (form &aux node access+ access-)
  (cond ((symbolp form)
	 (setq node (install-bms-node form)
	       access+ `(node-belief+ ',node)
	       access- `(node-belief- ',node)))
	((consp form)
	 (case (car form)
	   (and (setq node (make-and form)
		      access+ `(node-belief+ ',node)
		      access- `(node-belief- ',node)))
	   (or (setq node (make-or form)
		     access+ `(node-belief+ ',node)
		     access- `(node-belief- ',node)))
	   (not (multiple-value-setq (node access- access+)     ;switch +/-
		  (parse-sme-assertion (second form))))
	   (t (setq node    (install-bms-node form)
		    access+ `(node-belief+ ',node)
		    access- `(node-belief- ',node))))))
  (values node access+ access-))


;;; Make an "or" bms node by making the "or" node, a node for each disjunct,
;;;   and a support link from the disjuncts to the or node
;;;       OR = ( max(belief+),   max(0, (sum(belief-) - (|disjuncts| - 1))) )
;;;
(defun make-or (form &optional (sme-assert? nil))
  (let ((node (referent-bms-node form))
	antecedents access+ access- fn)
    (unless node
      (setq node (install-bms-node form))
      (dolist (disjunct (cdr form))
	(multiple-value-bind (disj-node a+ a-)
	    (parse-sme-assertion disjunct)
	  (push disj-node antecedents)
	  (push a+ access+)
	  (push a- access-)))
      (setq fn (eval `#'(lambda (link)
			  (values (max ,@access+)
				  (max 0 (+ ,(- 1 (length (cdr form))) ,@access-))))))
      (install-support 'disjunction antecedents node fn nil))
    node))


;;; Make an "and" bms node by making the "and" node, a node for each conjunct,
;;;   and a support link from the conjuncts to the and node
;;;       AND = ( max(0, (sum(belief+) - (|disjuncts| - 1))),   max(belief-) )
;;;
(defun make-and (form &optional (sme-assert? nil))
  (let ((node (referent-bms-node form))
	antecedents access+ access- fn)
    (unless node
      (setq node (install-bms-node form))
      (dolist (conj (cdr form))
	(multiple-value-bind (conj-node a+ a-)
	    (parse-sme-assertion conj)
	  (push conj-node antecedents)
	  (push a+ access+)
	  (push a- access-)))
      (setq fn (eval `#'(lambda (link)
			  (values  (max 0 (+ ,(- 1 (length (cdr form))) ,@access+))
				   (max ,@access-)))))
      (install-support 'conjunction antecedents node fn nil)
      (if sme-assert? (dolist (node antecedents) (install-premise node))))
    node))


;;; Make the necessary support-links for the given implies form:  (implies p q (+ . -))
;;;   Here implies will always have a single antecedent node: (e.g. q, (or q s), (and q s) ...)
;;;
(defun bms-implies (form)
  (let ((weight (or (fourth form) (cons 1.0d0 0.0d0)))
	consequents fn)
    (if (consp (third form))                      ;get the consequent nodes
	(case (car (third form))
	  (not (psetf (car weight) (cdr weight)   ;switch the rule's weights
		      (cdr weight) (car weight))
	       (setq consequents (list (parse-sme-assertion (third form)))))
	  (and (setq consequents (mapcar #'parse-sme-assertion (cdr (third form)))))
	  (t   (setq consequents (list (parse-sme-assertion (third form))))))
	(setq consequents (list (parse-sme-assertion (third form)))))
    (multiple-value-bind (antecedent access+)
	(parse-sme-assertion (second form))
      (setq fn
	    (eval `#'(lambda (link)
		       (values (* ,access+ ,(car weight))
			       (* ,access+ ,(cdr weight))))))
      (dolist (conseq consequents)
	(install-support 'implication (list antecedent) conseq fn t))
      nil)))

;;;
(defun retract (form)
  (cond ((symbolp form)
	 (forget-premise (referent-bms-node form)))
	((case (car form)
	   (not (retract (second form)))
	   (and (dolist (conj (cdr form))  (forget-premise (parse-sme-assertion conj))))
	   (or nil)    ;;;  (forget-premise
	   (implies (let ((antecedent (parse-sme-assertion (second form)))
			  (consequents (if (and (consp (third form))
						(eq (car (third form)) 'and))
					   (mapcar #'parse-sme-assertion (cdr (third form)))
					   (list (parse-sme-assertion (third form)))))
			  link)
		      (dolist (conseq consequents)
			(setq link (find antecedent (node-supporters conseq)
					:key #'(lambda (link) (car (link-antecedents link)))))
			(retract-support link)
			(setf (node-supporters conseq) (delete link (node-supporters conseq)))
			(setf (node-consequences antecedent)
			      (delete link (node-consequences antecedent)))
			(propagate-belief conseq))))
	   (t (forget-premise (referent-bms-node form)))))))

