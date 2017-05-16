;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SME; -*-

;; Character-oriented interface for SME
;;  uses cmenu system

;;; Copyright (c) 1989, Kenneth D. Forbus, University of Illinois.
;;; All rights reserved.

;;; BUG -- loading rules pops into selecting them!

(in-package 'SME)

(proclaim '(special *current-rule-set*))

(defvar *current-rule-set* nil)

(setq *dgroup-extension-name* #+:UNIX "dgr"
                                #+:Symbolics "dgr"
                                #+:Apple "dgroup"
                                )
(setq *rule-extension-name* #+:UNIX "rules"
                              #+:Symbolics "rules"
                              #+:Apple "rules"
                              )

(defvar *sme-extra-language-files* nil) ;; For adding language modules

;;;; The toplevel

(proclaim '(special user::*cmenu-prompt* *sme-command-menu*))

(defun toplevel ()
  (let ((user::*cmenu-prompt* "SME >>"))
    (user::run-command-menu *sme-command-menu* "SME: Toplevel Menu")))

(defvar *sme-command-menu*
  '(("Initialize" (progn (sme-init)
			 (dolist (lfile *sme-extra-language-files*)
				 (load lfile)))
     :documentation "Clears SME & reloads language")
    ("Change file parameters" (menu-set-file-parameters) :documentation "Change file parameters")
    ("Load rules & dgroups" (menu-load-rules-and-dgroups) :documentation "Load rule and dgroup files")
    ("Match parameters" (menu-set-match-parameters) :documentation "Set base, target, rules, and flags.")
    (#-MACFAC "Run SME" #+MACFAC "Run plain SME"
     (run-matcher-from-menu) :documentation 
     #-MACFAC "Run matcher"  #+MACFAC "Run standard SME match rules")
    #+MACFAC ("run MAC/FAC matcher" (run-macfac-matcher-from-menu)
	      :documentation "Run MAC/FAC matcher on a particular example.")
    ("Examine" (menu-examine-match-results) :documentation "Examine SME results")))

;;;; Loading rules and dgroups

;; Uses *sme-dgroup-pathname* and *dgroup-extension-name* to provide selections

(setq *sme-file-parameter-menu*
  '(("language definitions file" *SME-LANGUAGE-FILE* :string)
    ("Language extension files" *sme-extra-language-files* :list-of-strings)
    ("Rules pathname" *sme-rules-pathname* :string)
    ("Dgroup pathname" *sme-dgroup-pathname* :string)
    ("Rule file extension" *rule-extension-name* :string)
    ("Dgroup file extension" *dgroup-extension-name* :string)))

(defun menu-set-file-parameters ()
  (user::cmenu-choose-values *sme-file-parameter-menu* "Current file parameters"))

(defun menu-load-rules-and-dgroups ()
  (user::run-command-menu '(("Load dgroups" (menu-load-dgroups))
			    ("Load rules" (menu-load-rules)))
			  "SME: Loading dgroups and rules"))

;; Helpers
(defvar *dgroup-load-menu* nil)

(defun make-dgroup-load-menu ()
    (setq *dgroup-load-menu*
	  (mapcar #'(lambda (file)
		      (list (pathname-name file) file))
		  (directory (concatenate 'string *sme-dgroup-pathname*
					  "*." *dgroup-extension-name*)))))

(defvar *rules-load-menu* nil)

(defun make-rules-load-menu ()
    (setq *rules-load-menu*
	  (mapcar #'(lambda (file)
		      (list (pathname-name file) file))
		  (directory (concatenate 'string *sme-rules-pathname*
					  "*." *rule-extension-name*)))))

(defun menu-load-rules ()
  (make-rules-load-menu)
  (do ((file nil)) (nil)
    (setq file (user::cmenu-choose *rules-load-menu*))
    (cond ((or (null file) (eq file :PUNT)) (return-from menu-load-rules T))
	  (t (load-rule-set (pathname-name file) file)))))

(defvar *rule-set-index* nil)

(proclaim '(special *tre-rules-saver* *initial-sme-assertions*
		    *mhc-intern-rules* *mhc-filter-rules*))

(defun load-rule-set (rset-name rset-file) 
  (let ((*tre-rules-saver* nil)
	(*initial-sme-assertions* nil)
	(*mhc-intern-rules* nil)
	(*mhc-filter-rules* nil)
	(entry (assoc rset-name *rule-set-index*
		      :test #'string=)))
    (load rset-file)
    (when entry (setq *rule-set-index* (delete entry *rule-set-index*)))
    (push (list rset-name (list rset-name *mhc-intern-rules* *mhc-filter-rules*
				*tre-rules-saver* *initial-sme-assertions*
				))
	  *rule-set-index*)))

(defun menu-load-dgroups ()
  (make-dgroup-load-menu)
  (do ((file nil)) (nil)
    (setq file (user::cmenu-choose *dgroup-load-menu*))
    (cond ((or (null file) (eq file :PUNT)) (return-from menu-load-dgroups T))
	  (t (load file)))))

;;;; Setting match parameters

(defun menu-set-match-parameters ()
  (user::run-command-menu '(("Select base" (menu-select-dgroup :base)
			     :documentation "Select base dgroup")
			    ("Select target" (menu-select-dgroup :target)
			     :documentation "Select target dgroup")
			    ("Select rule set" (menu-select-rule-set)
			     :documentation "Determines what match rules are used.")
			    ("Set flags" (menu-set-sme-flags)
			     :documentation "Various internal and print control flags")
			    ("Show current settings" (describe-sme-match-vars)
			     :documentation "Shows current base, target, and rule set."))
			  "SME: Match Parameters Menu"))

(defun describe-sme-match-vars ()
  (format t "~% Base = ~A; Target = ~A; Rule set = ~A."
	  *base* *target* *current-rule-set*))

(defun menu-select-rule-set ()
  (let ((entry (user::cmenu-choose *rule-set-index*)))
    (cond ((or (null entry) (eq entry :PUNT)) (setq *current-rule-set* nil))
	  (t (set-rule-set (car entry))))))

(defun set-rule-set (name)
  (let ((rset (cadr (find name *rule-set-index* :test #'(lambda (name entry)
						    (string= name (car entry)))))))
    (unless rset
      (error "~% SME rule set ~A not found -- SME::SET-RULE-SET." name))
    (when rset
      (setq *tre-rules-saver* (fourth rset))
      (setq *initial-sme-assertions* (fifth rset))
      (setq *current-rule-set* (first rset))
      (clear-sme)
      ;; Since SME doesn't provide a cache for match rules, I do.
      (setq *match-rules-file* (first rset))
      (setq *mhc-intern-rules* (second rset))
      (setq *mhc-filter-rules* (third rset)))
    rset))

(defun clear-sme ()
  ;; This splits out stuff hidden in MATCH and SME-RULES-FILE
  ;; to simplify switching between rule sets
  ;; Nuke SME internal pointers (to falken: why doesn't TRE-INIT do this??)
  (setq *mhc-intern-rules* nil)
  (setq *mhc-filter-rules* nil)
  (setq *match-hypotheses* nil)
  (setq *mh-identifier* (cons 1 nil))
  (setq *gmaps* nil)
  (setq *gmap-count* 0)
  (if *mh-hash-table*
      (clrhash *mh-hash-table*)
      (setq *mh-hash-table* (make-hash-table :test #'equal :size *mh-table-size*)))
  (tre-init))

(defun menu-set-sme-flags ()
  (user::cmenu-choose-values '(("Which gmaps to display" *gmap-display* :one-of 
					       (("All Gmaps" :ALL) ("Best Gmaps" :BEST)
						("No Gmaps" :NONE)))
			       ("Which gmap statistics to display"
				*gmap-statistics* :one-of
				(("All Gmaps" :ALL) ("Best Gmaps" :BEST)
                                           ("No Gmaps" :NONE)))
			       ("Show dgroup statistics" *DGROUP-STATISTICS*
				:one-of (("Yes" t) ("No" nil)))
			       ("Display all match hypotheses" *DISPLAY-ALL-MH* :one-of (("Yes" t) ("No" nil)))
			       ("Generate Candidate Inferences" *CI-FLAG*  :one-of (("Yes" t) ("No" nil)))
			       ("Run Gmap Merge Step 3" *GMAP-MERGE-STEP3?*  :one-of (("Yes" t) ("No" nil)))
			       ("SME Debug Flag" *DEBUG-SME* :one-of (("On" t) ("Off" nil)))
			       ("Indicate all changes to the SME knowledge base" *SME-KNOWLEDGE-FLAG* 
				:one-of (("On" t) ("Off" nil))))
			     "SME flags"))

(defun menu-select-dgroup (bt)
  (let ((selection (user::cmenu-choose (mapcar #'(lambda (dgname) (list dgname dgname)) *description-groups*)
				       (if (eq bt :base) "Select dgroup for base"
					   "Select dgroup for target"))))
    (unless (or (null selection) (eq selection :punt))
      (cond ((eq bt :base) (setq *base* (fetch-dgroup selection)))
	    ((eq bt :target) (setq *target* (fetch-dgroup selection)))))))

;;;; Running the matcher

(defun run-matcher-from-menu (&aux abort?)
  ;; must see if everything is set
  (when (or (null *base*) (not (dgroup? *base*)))
    (format t "~% Base description not yet specified.")
    (setq abort? t))
  (when (or (null *target*) (not (dgroup? *target*)))
    (format t "~% Target description not yet specified.")
    (setq abort? t))
  (when (null *current-rule-set*)
    (format t "~% Rule set not yet selected.")
    (setq abort? t))
  (when abort? (return-from run-matcher-from-menu t))
  (print (multiple-value-list (match *base* *target*))))

#+MACFAC (defvar *selected-macfac-matcher* nil)

#+MACFAC (defun run-macfac-matcher-from-menu ()
	   (user::run-command-menu '(("Select MAC/FAC matcher"
				      (let ((menu (mapcar #'(lambda (x) (list (mf::matcher-name x) x))
							  mf::*matchers*))
					    (choice nil))
					(setq choice
					      (user::cmenu-choose menu
						"Choose a MAC/FAC matcher"))
					(unless (or (null choice) (eq choice :PUNT))
					  (setq *selected-macfac-matcher* choice)))
				      :documentation "Choose a MAC/FAC matcher to try.")
				     ("Run MAC/FAC matcher"
				      (cond ((null (mf::matcher? *selected-macfac-matcher*))
					     (format t "~%You must select a MAC/FAC matcher first."))
					    (t (print 
						(funcall (mf::matcher-procedure *selected-macfac-matcher*)
							*base* *target*))))
				      :documentation "Run MAC/FAC matcher on the base and target."))
				   "SME: Run MAC/FAC matcher menu"))

(defun menu-examine-match-results ()
  (user::run-command-menu '(("Display match" (sme::display-match *base* *target*)
			     :documentation "Shows full listing of information about the match.")
			    ("Display base" (sme::describe-dgroup *base*)
			     :documentation "Shows the base description.")
			    ("Display target" (sme::describe-dgroup *target*)
			     :documentation "Shows the target description.")
			    ("Display statistics" (sme::display-match-statistics)
			     :documentation "Shows a summary of match properties")
                            ;; Graphics system isn't general -- right now only working
                            ;; version is for the IBM RT, under Lucid 1.01.  Kind of sad...
			#+RT ("Plot match" (sme::plot-match-menu)
			     :documentation "Plots match graphically (must be in the editor to use)."))
			  "SME: Display Menu"))

(defun plot-match-menu ()
  (user::run-command-menu '(("Create plot" (user::show-sme-match)
			     :documentation "Creates and displays match plot")
			    ("Hide plot" (user::hide-sme-match))
			    ("Show plot" (user::refresh-sme-plot)
			     :documentation "Re-displays the entire plot.")
			    ("Show dgroups only" (user::show-plot-dgroups-only)
			     :documentation "Shows just the dgroups.")
			    ("Show match hypotheses" (user::show-plot-mhs-only)
			     :documentation "Shows match hypotheses w/o gmaps.")
			    ("Redisplay plot" (user::refresh-sme-plot)
			     :documentation "Re-displays plot, if hidden.")
			    ("Dump plot" (user::dump-sme-plot)
			     :documentation "Creates a bitmap file from the plot (.lbm)")
			    ("Dump plots for stages" (user::dump-plot-stages)
			     :documentation "Dumps a bitmap for each stage (dgroups, mhs, gmaps) of the match."))
			  "SME: Plot Menu")) 

;;;; Some useful SME-oriented print routines

(defun lispify-sme-thing (thing)
  (cond ((entity? thing) (sme::entity-name thing))
	((expression? thing)
	 (cons (sme::expression-functor thing)
	       (mapcar #'lispify-sme-thing 
		       (sme::expression-arguments thing))))))

(defun lispify-candidate-inferences (cis dgroup)
  (cond ((null cis) nil)
	((symbolp cis)
	 (let ((exp (get cis (dgroup-name dgroup))))
	   (if exp (lispify-sme-thing exp) cis)))
	((listp cis)
	 (cons (lispify-candidate-inferences (car cis) dgroup)
	       (lispify-candidate-inferences (cdr cis) dgroup)))
	(t cis)))

(defun dgroup-concordance (dgr &optional (stream *standard-output*))
  (labels ((print-concordance-divider (stream which)
	      (format stream "~%~A"
		      (if (eq which :boundary)
			  "========================================================="
			  "---------------------------------------------------------")))
	   (print-concordance-item (item stream)
				   (format stream "~%~A" (expression-name item))
				   (pprint (lispify-sme-thing item) stream)))
  (print-concordance-divider stream :boundary)
  (format stream "~% Concordance for Dgroup ~A." (dgroup-name dgr))
  (dolist (item (sort (copy-list (dgroup-expressions dgr))
		      #'(lambda (x y) (string< (symbol-name (expression-name x))
					       (symbol-name (expression-name y))))))
    (print-concordance-divider stream :Internal)
    (print-concordance-item item stream))
  (print-concordance-divider stream :Internal)
  (format stream "~% End of concordance for Dgroup ~A." (dgroup-name dgr))
  (print-concordance-divider stream :boundary)))

(defun dgroup-set-concordance (dgroup-list report-pathname)
  ;;; Produces a file containing the concordances for the listed dgroups.
  ;;; (Can be used with a single dgroup if needed, of course)
  (with-open-file (rout report-pathname :direction :output)
		  (do ((dgrs dgroup-list (cdr dgrs)))
		      ((null dgrs))
		    (dgroup-concordance (car dgrs) rout)
		    ;; Install a page break if needed between dgroups
		    (if (cdr dgrs) (format rout "~|")))))

;;;; LaTeX'ing dgroup concordances
;; Use the "description" envirionment and alltt to produce a compact 
;; guide to match plots and decoding other arcane SME output.
;; All this needs to be better integrated.  

(defun LaTeX-dgroup-concordance (dgr &optional (stream *standard-input*) (borders? t))
  ;; Generates the LaTeX source for a single dgroup.
  (labels ((print-concordance-divider (stream which)
            (format stream "~%~A"
		    (case which
		      (:boundary "\\rule{\\textwidth}{0.04in}")
		      (:no-border "\\linebreak")
		      (t "\\hline" )))) ;; "\\rule{\\textwidth}{0.01in}"
	   (print-concordance-item (item stream)
	     (format stream "~%{\\bf ~A:} & ~%\\begin{minipage}{6in}~%\\begin{alltt}~A~%\\end{alltt}~%\\end{minipage} \\\\"
		     (expression-name item)
		     (with-output-to-string (foo) (pprint (lispify-sme-thing item) foo)))))
    (print-concordance-divider stream (if borders? :boundary :no-border))
    (format stream "~%{\\center {\\bf Concordance for Dgroup ~A}}~%" (dgroup-name dgr))
    (user::LaTeX-start stream "tabular" "{ll}")
    (dolist (item (sort (copy-list (dgroup-expressions dgr))
			#'(lambda (x y) (string< (symbol-name (expression-name x))
						 (symbol-name (expression-name y))))))
      (print-concordance-divider stream (if borders? :Internal :no-border))
      (print-concordance-item item stream))
    (print-concordance-divider stream (if borders? :Internal :no-border))
    (user::LaTeX-end stream "tabular")
    (format stream "~%{\\center {\\bf End of concordance for Dgroup ~A.}}~%"
	    (dgroup-name dgr))
    (if borders? (print-concordance-divider stream :boundary))))

(defun LaTeX-dgroup-set-concordance (dgroup-list report-pathname &optional (border? t))
  ;; Produces a file containing LaTeX source code for a fancy
  ;; dgroup concordance
  (with-open-file (rout report-pathname :direction :output)
	(user::start-LaTeX-file rout)
	(do ((dgrs dgroup-list (cdr dgrs)))
	    ((null dgrs))
	  (LaTeX-dgroup-concordance (car dgrs) rout border?)
	  (if (cdr dgrs) (user::New-LaTeX-Page rout)))
	(user::end-LaTeX-file rout)))

;;;; Pretty-printing whole dgroups

(defun pp-dgroup (dgr &optional (stream *standard-output*))
  (dolist (form (lispify-dgroup dgr))
    (pprint form stream)))

(defun lispify-dgroup (dgr)
  (mapcar #'lispify-sme-thing (dgroup-roots dgr)))

(defun lispify-inferences (gm)
  (fully-expand-expression-form (gm-inferences gm) (gm-target gm)))

(defun dgroup-set-pp-listing (dgroup-list report-pathname)
  ;;; Produces a file containing the pretty-printed descriptions for the listed dgroups.
  ;;; (Can be used with a single dgroup if needed, of course)
  (let ((divider "========================================================="))
    (with-open-file (rout report-pathname :direction :output)
		    (do ((dgrs dgroup-list (cdr dgrs)))
			((null dgrs))
		      (format rout "~%~A" divider)
		      (format rout "~% Dgroup ~A:" (dgroup-name (car dgrs)))
		      (pp-dgroup (car dgrs) rout)
		      (format rout "~%~A" divider)
		      ;; Install a page break if needed between dgroups
		      (if (cdr dgrs) (format rout "~|"))))))

;;;; Printing dgroup statistics

;; What is there to know?  Number of entities, attributes, relations, functions, and order vector.
;; All this could fit on one line, thus making a pretty concise report.

(defun dgroup-set-statistics (dgroup-list report-pathname)
  (with-open-file (rout report-pathname :direction :output)
		  (format rout "~% Dgroup statistics -- ~D in all.~% File = ~A"
			  (length dgroup-list) report-pathname)
  (format rout "~%~25@:<Name~>~6:<# Ent~>~6:<# Atr~>~6:<# Fun~>~6:<# Log~>~6:<# Rel~>  Order vector")
		  (do ((dgrs (sort (copy-list dgroup-list)
				   #'(lambda (dg1 dg2)
				       (string< (dgroup-name dg1) (dgroup-name dg2))))
			     (cdr dgrs)))
		      ((null dgrs))
		    (dgroup-stats (car dgrs) rout))
		  (format rout "~% End of dgroup statistics listing ~A" report-pathname)))

(defun dgroup-stats (dgr &optional (stream *standard-output*))
 ;; Number of entities, attributes, relations, functions, and order vector.
  (multiple-value-bind (nentities nattributes nfunctions nlogical nrelations order-vector)
		       (compute-dgroup-stats dgr)
  (format stream "~%~25:<~A~>~6:<~A~>~6:<~A~>~6:<~A~>~6:<~A~>~6:<~A~>  ~A"
	  (dgroup-name dgr) nentities nattributes nfunctions nlogical nrelations order-vector)))

(defun compute-dgroup-stats (dgroup)
  (let ((nattributes 0) (nfunctions 0) (nlogical 0)(nrelations 0)
	(order-vector (list (length (dgroup-entities dgroup)))))
    (dolist (exp (dgroup-expressions dgroup))
      (case (sme::predicate-type (sme::expression-functor exp))
	(ENTITY (error "Can't have an entity here -- ~A in ~A." exp dgroup))
	(FUNCTION (incf nfunctions))
	(ATTRIBUTE (incf nattributes))
	(RELATION (incf nrelations))
	(LOGICAL (incf nlogical))
	(t (error "Unknown expression type -- ~A is ~A in ~A."
		  exp (sme::predicate-type (sme::expression-functor exp))
		  dgroup)))
      (setq order-vector (extend-order-vector order-vector
					      (sme::expression-order exp))))
    (values (car order-vector) nattributes nfunctions nlogical
	    nrelations order-vector)))

(defun extend-order-vector (vector order &aux v)
  ;; First make sure it extends to that order
  (setq v vector)
  (dotimes (i order)
    (cond ((null (cdr v)) ;; Extend it
	   (setf (cdr v) (list 0))))
    (setq v (cdr v)))
  (incf (car v))
  vector)

;;;; Inspecting matches in more detail

(defun fetch-form (form dgroup &aux result)
  (setq result (find form (dgroup-expressions dgroup)
		     :test #'(lambda (form exp)
			       (equal form (lispify-sme-thing exp)))))
  (if result (return-from FETCH-FORM result))
  (find form (dgroup-entities dgroup)
		   :test #'(lambda (form exp)
			     (equal form (lispify-sme-thing exp)))))

(defun find-mh (base-item target-item &optional (mhs sme::*match-hypotheses*))
  ;; Make not user-unfriendly, at least.
  (unless (or (entity? base-item) (expression? base-item))
    (setq base-item (fetch-form base-item sme::*base*)))
  (unless (or (entity? target-item) (expression? target-item))
    (setq target-item (fetch-form target-item sme::*target*)))
  (dolist (mh mhs)
    (when (and (eq base-item (mh-base-item mh))
	       (eq target-item (mh-target-item mh)))
      (return-from find-mh mh))))

(defun pp-mh (mh &optional (stream *standard-output*))
  (format stream "~%MH(~A,~A)~A, Order = ~D, SES = ~D" (sme::mh-base-item mh)
	  (sme::mh-target-item mh) (if (sme::mh-justifies-incomplete? mh) "*" "")
	  (sme::mh-order mh) (sme::node-belief+ (sme::mh-bms-node mh)))
  (format stream "~% B: ~A" (lispify-sme-thing (sme::mh-base-item mh)))
  (format stream "~% T: ~A" (lispify-sme-thing (sme::mh-target-item mh)))
  (format stream "~% #P: ~D #C: ~D." (length (sme::mh-justifications mh))
	  (length (sme::mh-justifies mh))))

(defun pp-gmap (gm &optional (details? nil) (stream *standard-output*))
  (format stream "~%GM~D: ~D correspondences, SES = ~D"
	  (gm-id gm) (length (gm-elements gm)) (node-belief+ (gm-bms-node gm)))
  (format stream "~%  Object mappings:")
  (dolist (emap (gm-emaps gm))
    (format stream "~%   ~A <-> ~A" (lispify-sme-thing (cadr emap))
	    (lispify-sme-thing (caddr emap))))
  (cond ((gm-inferences gm)
	 (format stream "~%  Candidate Inferences:")
	 (dolist (inf (lispify-candidate-inferences (gm-inferences gm) (gm-target gm)))
	   (pprint inf stream)))
	(t (format stream "~%  No candidate inferences.")))
  (when details?
    (dolist (mh (gm-elements gm)) (pp-mh mh stream)))
  gm)
