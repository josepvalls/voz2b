;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: mf; -*- 

(in-package 'SME)

;;; Simple structural evaluator
;;;  K. Forbus, University of Illinois

; Based on the evidence system used in the original SME, but
; w/o all the Dempster-Shafer stuff.  Basically, D/S makes it
; impossible to analyze.

;; This is the same code as used with ESENSE.  I've just
;; copied it under MAC/FAC to keep everything in one place.

(defvar *se-trace* nil)
(defvar *local-evidence-rules* nil)
(defvar *trickle-down-rules* nil) 
(defvar *se-name* nil) ;; Name for set of rules
(defvar *se-cache* nil) ;; Stores sets of rules

(defun load-se-rules (rule-file)
  (setq *local-evidence-rules* nil)
  (setq *trickle-down-rules* nil)
  (load rule-file)
  (push (list *se-name* *local-evidence-rules* *trickle-down-rules*)
	*se-cache*))

(defun uncache-se (se)
  (let ((entry (assoc se *se-cache*)))
    (unless entry (error "~a doesn't name a structural evaluator." se))
    (setq *se-name* (car entry)
	  *local-evidence-rules* (cadr entry)
	  *trickle-down-rules* (caddr entry))
    se))

(defmacro defSE (name)
  `(setq *se-name* ',name))

;;;; Defining SE rules

;; Format is:
; (defSErule <name> <key> <test> . <body>)
;; Where <name> = symbol (for debugging and records)
;;       <key> = :LOCAL or :TRICKLE-DOWN
;;       <test> = if non-nil, execute <body>
;;       <body> = a non-empty sequence of forms

;; The variables ?MH, ?B, and ?T can be used freely in <test>
;; and <body>.  All :LOCAL rules are executed before any
;; :TRICKLE-DOWN rules are.  :TRICKLE-DOWN rules are executed
;; from mh roots downward, such that all rules are executed on
;; facts of order N before any are executed on facts of order N-1.

(defmacro defSErule (name key test &rest body)
  `(push (list ',name
	       #'(lambda (?mh ?b ?t) ,test)
	       #'(lambda (?mh ?b ?t) ,@ body))
	 ,(case key
		(:LOCAL '*local-evidence-rules*)
		(:TRICKLE-DOWN '*trickle-down-rules*)
		(t (error "~A not valid SE rule type for ~A" key name)))))

(defun try-se-rule-on (rule mh bitem titem)
  (if *se-trace* (format t "~%Trying ~A on ~A" (car rule) mh))
  (when (funcall (cadr rule) mh bitem titem)
	(funcall (caddr rule) mh bitem titem)))


;;;; Running the SE rules

;; We will use the BMS node structure, but we will NOT use
;; Dempster/Shafer!  Start by nuking beliefs in match hypotheses,
;; also those for gmaps.  Then sort the match hypotheses by
;; order, and iterate.

(defun run-SE (&optional (structure-groups *gmaps*) &aux node mh-queue)
  ;; First nuke the old values 
  (dolist (mh *match-hypotheses*)
	  (setq node (sme::mh-bms-node mh))
	  (setf (sme::node-belief+ node) 0.0)
	  (setf (sme::node-belief- node) 0.0))
  (dolist (gm structure-groups)
	  (setq node (sme::gm-bms-node gm))
	  (setf (sme::node-belief+ node) 0.0)
	  (setf (sme::node-belief- node) 0.0))
  (setq mh-queue (sort (remove-if #'(lambda (mh) (sme::mh-justifies-incomplete? mh))
				  (copy-list *match-hypotheses*))
		       #'(lambda (mh1 mh2)
			   (> (mh-order mh1) (mh-order mh2)))))
  (dolist (mh mh-queue)
	  (dolist (rule *local-evidence-rules*)
		  (try-se-rule-on rule mh (mh-base-item mh)
					(mh-target-item mh))))
  (dolist (mh mh-queue)
	  (dolist (rule *trickle-down-rules*)
		  (try-se-rule-on rule mh (mh-base-item mh)
					(mh-target-item mh))))
  (dolist (gm structure-groups)
	  (setq node (gm-bms-node gm))
	  (dolist (mh (gm-elements gm))
		  (incf (node-belief+ node)
			(node-belief+ (mh-bms-node mh))))))

;;;; Helpers for evidence rules

;; Allowing values to grow w/o bound is a little scary, not to
;; mention not physically realizable.  Even the range of flonums
;; is awfully big.  So if *maximum-node-evidence* is non-nil, it
;; will be assumed to be a floating-point number which no node will
;; be allowed to rise above.  

(defvar *maximum-node-evidence* 1.0) 

(defun add-evidence (mh amount)
  (incf (node-belief+ (mh-bms-node mh)) amount)
  (when *maximum-node-evidence*
    (if (> (node-belief+ (mh-bms-node mh)) 
	   *maximum-node-evidence*)
	(setf (node-belief+ (mh-bms-node mh)) 
	      *maximum-node-evidence*))))


;;;; Inspecting the results

(defun show-mh-ses (&key (by-order nil)) ;; Otherwise, by score.
  (let ((mh-list (sort (copy-list *match-hypotheses*)
		       (if by-order
			   #'(lambda (x y) (< (mh-order x) (mh-order y)))
			   #'(lambda (x y) (> (node-belief+ (mh-bms-node x))
					      (node-belief+ (mh-bms-node y)))))))
	(min 100000.0)
	(max -100000.0))
    (dolist (mh mh-list)
      (if (> (node-belief+ (mh-bms-node mh)) max) (setq max (node-belief+ (mh-bms-node mh))))
      (if (< (node-belief+ (mh-bms-node mh)) min) (setq min (node-belief+ (mh-bms-node mh)))))
    (format t "~% ~D match hypotheses,~% Min SES = ~D~% Max SES = ~D"
	    (length mh-list) min max)
    (dolist (mh mh-list)
      (format t "~%~A = ~D" mh (node-belief+ (mh-bms-node mh))))))
