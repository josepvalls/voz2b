;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: mf; -*-

;;;; MAC/FAC character-oriented interface

;;; Copyright (c) 1989, Kenneth D. Forbus, University of Illinois.
;;; All rights reserved.

;; (may not be pretty, but it sure is portable!)
;; Assumes cmenu system is loaded.

(in-package 'mf)

(proclaim '(special user::*cmenu-prompt*))

(defvar *macfac-command-menu*
  '(("Initialize" (clear-macfac) :documentation "Clears previous results.")
    ("SME" (sme::toplevel) nil :documentation "Provides access to SME commands.")
    ("Loading parts" (menu-load-macfac-parts)
     :documentation "Commands for loading matchers and selectors")
    ("Loading dgroups" (menu-load-dgroups)
     :documentation "Commands for loading dgroups.") 
    ("Wire it up" (menu-configure-macfac)
     :documentation "Select matchers and selectors for MAC and FAC stages.")
    ("Arrange memory" (menu-arrange-memory)
     :documentation "Set and change contents of MAC/FAC's memory")
    ("Choose probe" (menu-choose-probe)
     :documentation "Select which dgroup will be used as a probe.")
    ("Run MAC/FAC" (menu-run-macfac)
     :documentation "Runs MAC/FAC on the given memory and probe list.")
    ("Inspect" (menu-macfac-inspect)
     :documentation "Displays information about the current MAC/FAC run.")
    ("Report" (menu-macfac-reports)
     :documentation "Dumps information about current MAC/FAC run.")))

(defun macfac ()
  (let ((user::*cmenu-prompt* "MAC/FAC >>"))
    (user::run-command-menu *macfac-command-menu* "MAC/FAC: Toplevel Menu")))

(defvar *macfac-parts-parameter-menu*
  '(("Pathname for matchers" *matchers-pathname* :string)
    ("Extension for matcher files" *matchers-extension* :string)
    ("Pathname for selectors" *selectors-pathname* :string)
    ("Extension for selector files" *selectors-extension* :string)))

(defun menu-load-macfac-parts ()
  ;;for loading matchers and selectors.  Three basic options, change parameters
  ;; i.e., pathname and extension, load all, or load one.
  (user::run-command-menu
   '(("File parameters" (user::cmenu-choose-values *macfac-parts-parameter-menu*
							 "Current parts file parameters")
     :documentation "Change pathname and extension for selector and matcher files.")
    ("Load a matcher" (choose-some-files (generate-directory-menu *matchers-pathname* *matchers-extension*))
     :documentation "Load some matchers from the current directory.")
    ("Load a selector" (choose-some-files (generate-directory-menu *selectors-pathname* *selectors-extension*))
     :documentation "Load some selectors from the current directory.")
    ("Load all matchers" (load-files-matching *matchers-pathname* *matchers-extension*)
     :documentation "Load all matchers in the current directory")
    ("Load all selectors" (load-files-matching *selectors-pathname* *selectors-extension*)
     :documentation "Load all selectors in the current directory"))
   "MAC/FAC Parts Menu"))

;;;; Some helpers for menus involving files 

;; (worth moving to cmenu?)

(defun generate-directory-menu (pathname extension)
  (mapcar #'(lambda (file)
	      (list (pathname-name file) file))
	  (directory (concatenate 'string pathname "*." extension))))

(defun choose-some-files (file-menu)
  (do ((file nil)) (nil)
    (setq file (user::cmenu-choose file-menu))
    (cond ((or (null file) (eq file :PUNT)) (return-from choose-some-files t))
	  (t (load file)))))

(defun load-files-matching (pathname extension)
  (dolist (file (directory (concatenate 'string pathname "*." extension)))
    (load file)))

;; Now, back to the show...

(defvar *dgroup-menu-pathname* "")

(defun menu-load-dgroups ()
  (user::run-command-menu
   '(("Show defaults" (progn (format t "~%The default dgroup directories are:")
					(dolist (path *dgroup-directories*)
					  (format t "~%     ~A" path)))
      :documentation "Directories where MAC/FAC loads dgroups from.")
     ("Change defaults" (do ((dir nil)(dirs nil)) (nil)
				     (format t "-")
				     (setq dir (read-line))
				     (cond ((string= dir "") ;; that's all
					    (setq *dgroup-directories* (nreverse dirs))
					    (return t))
					   (t (push dir dirs))))
      :documentation "Changes where MAC/FAC loads dgroups from.")
     ("Load all dgroups" (dolist (path *dgroup-directories*)
			   (load-dgroup-directory path))
      :documentation "Load all dgroups on default directories.")
     ("Load some dgroups" (menu-load-some-dgroups)
      :documentation "Selectively load dgroups."))
   "MAC/FAC: Dgroups Menu"))

(defun menu-load-some-dgroups ()
  (let ((path nil)(extension nil))
    (format t "~% Pathname =")
    (setq path (read-line))
    (format t "~% Extension =")
    (setq extension (read-line))
    (choose-some-files (generate-directory-menu path extension))))

;;;; Choosing selectors and matchers

(defun menu-configure-macfac ()
  (user::run-command-menu 
   '(("MAC matcher" (careful-choose-part :MAC :Matcher)
      :documentation "Selects from loaded matchers which is used in MAC stage.")
     ("FAC matcher"  (careful-choose-part :FAC :Matcher)
      :documentation "Selects from loaded matchers which is used in FAC stage.")
     ("MAC selector" (careful-choose-part :MAC :Selector)
      :documentation "Selects from loaded selectors which is used in MAC stage.")
     ("FAC selector" (careful-choose-part :FAC :Selector)
      :documentation "Selects from loaded selectors which is used in FAC stage.")
     ("Show current wiring" (report-on-MACFAC-wiring *standard-output*)
      :documentation "Show current matchers and selectors"))
   "MAC/FAC: Configuration Menu"))

(defun careful-choose-part (stage part-type)
  (let ((menu (case part-type
		(:Matcher (mapcar #'(lambda (x) (list (matcher-name x) x))
				  *matchers*))
		(:Selector (mapcar #'(lambda (x) (list (selector-name x) x))
				   *selectors*))))
	(choice nil))
    (setq choice (user::cmenu-choose menu
				     (format nil "Choose ~A ~A"
					     (case stage
					       (:MAC "MAC") (:FAC "FAC"))
					     (case part-type
					       (:Matcher "matcher") (:Selector "selector")))))
    (when (or (null choice) (eq choice :PUNT)) (return-from careful-choose-part nil))
    (case part-type
      (:Matcher (case stage
		  (:MAC (setq *mac-matcher* choice))
		  (:FAC (setq *fac-matcher* choice))))
      (:Selector (case stage
		   (:MAC (setq *mac-selector* choice))
		   (:FAC (setq *fac-selector* choice)))))))

(defun menu-arrange-memory ()
  ;; Add a dgroup
  ;; Remove a dgroup
  ;; Load all dgroups
  (let ((user::*cmenu-prompt* "MAC/FAC MM>"))
  (user::run-command-menu 
   '(("Show memory" (show-macfac-memory)
      :documentation "Shows current state of MAC/FAC memory.")
     ("Use all dgroups" (progn (clear-memory) (fill-memory))
      :documentation "Use every loaded dgroup as MAC/FAC's memory set.")
     ("Add dgroup(s)" (menu-memorize-dgroups)
      :documentation "Add dgroup(s) to MAC/FAC's memory.")
     ("Remove dgroup(s)" (menu-forget-dgroups)
      :documentation "Remove dgroup(s) from MAC/FAC's memory."))
   "MAC/FAC: Memory Menu")))

(defun menu-memorize-dgroups ()
  ;; Adds dgroups if they aren't there already.
  ;; Dumb version, assumes memory is small
  (do ((menu nil nil) (choice nil))
      (nil)
    (dolist (dgname sme::*description-groups*)
      (let ((dgroup (sme::fetch-dgroup dgname)))
	(unless (member dgroup *memory*)
	  (push (list dgname dgroup) menu))))
    (setq choice (user::cmenu-choose menu "Dgroup to remember"))
    (when (or (null choice) (eq choice :punt))
      (return-from menu-memorize-dgroups t))
    (push choice *memory*)))

(defun menu-forget-dgroups ()
  (do ((menu (mapcar #'(lambda (dgroup) (list (sme::dgroup-name dgroup) dgroup))
		       *memory*))
       (choice nil))
      (nil)
    (setq choice (user::cmenu-choose menu "Dgroup to forget"))
    (when (or (null choice) (eq choice :punt)) (return-from menu-forget-dgroups t))
    (setq *memory* (delete choice *memory*))
    (setq menu (delete-if #'(lambda (pair) (eq (cadr pair) choice)) menu))))

;;;; Setting up the probe

(defun choose-a-dgroup (&optional (saying "Choose a dgroup"))
  (user::cmenu-choose (mapcar #'(lambda (dgname)
				  (list dgname (sme::fetch-dgroup dgname)))
			      sme::*description-groups*)
		      saying))

(defun menu-choose-probe ()
   (let ((choice (choose-a-dgroup "Choose probe")))
     (cond ((user::choice-aborted? choice))
	   (t (let ((old-probe *probe*))
		(setq *probe* choice)
		(when (and old-probe
			   (not (eq *probe* old-probe)))
		  (format t "~%  ;;; Returning ~A to MAC/FAC memory..." 
			  (sme::dgroup-name old-probe))
		  (memorize-dgroup old-probe))
		(format t "~% Remove probe ~A from memory?"
			(sme::dgroup-name *probe*))
		(if (yes-or-no-p) (forget-dgroup *probe*)))))))

;;;; Running MAC/FAC once

;; This is done as a variable so that matchers and selectors could when chosen
;; have parameters installed in here.  Is this ever a good idea?? 

(setq *macfac-parameter-menu* 
  '(("Debug flag" *debug-macfac* :boolean :documentation "Prints internal debugging information.")
    ("Details flag" *macfac-details* :boolean :documentation "Prints trace of MAC/FAC activities.")
    ("Dump details" *dump-macfac-details* :boolean
     :documentation "Dump details of matches to a file.")
    ("Detail dump file" *macfac-details-file* :string
     :documentation "File to dump match details to (must be set if dumping details!)")
    ("Cache new matches?" *cache-new-matches?* :boolean
     :documentation "Cache newly derived matches?")
    ("File for caching new matches" *new-match-cache-file* :string
     :documentation "File for caching newly derived matches.")))

(defun menu-run-macfac ()
  (user::run-command-menu 
   '(("Set flags and switches"
      (user::cmenu-choose-values *macfac-parameter-menu*
				 "Current trace and debug flag settings")
      :documentation "Set trace and debug flags.")
     ("Clear match cache" (setq *match-cache* nil)
      :documentation "Clear cached matches.")
     ("Load cache of matches"
      (progn (format t "~%File of cached matches:")
	     (load-cached-matches (read-line))))
     ("Do it" (when (MACFAC-ready?) (run-MACFAC))
      :documentation "Run MAC/FAC"))
   "MAC/FAC: Run Menu"))

;;;; Generating reports

(defvar *report-print-mode* :ASCII) ;; Alternative is LaTeX.
(defvar *latex-report-borders* t)

(defun menu-macfac-reports () 
  (user::run-command-menu
   '(("Background reports" (menu-macfac-background-reports)
      :documentation "Prints information about dgroups and other stuff.")
     ("Match reports" (menu-macfac-match-reports)
      :documentation "Prints information about the current run."))
   "MAC/FAC: Report Menu"))
     
(defun menu-macfac-background-reports ()
  (user::run-command-menu
   '(("Concordance for all dgroups" 
      (let ((output-path nil))
	(format t "~% Pathname for report =")
	(setq output-path (read-line))
	(case *report-print-mode*
	  (:ASCII (sme::dgroup-set-concordance
		   (mapcar #'sme::fetch-dgroup sme::*description-groups*) output-path))
	  (:LaTeX (sme::LaTeX-dgroup-set-concordance
		   (mapcar #'sme::fetch-dgroup sme::*description-groups*) output-path))))
      :documentation "Makes concordance file for all dgroups loaded.")
     ("Concordance for dgroups in memory" 
      (let ((output-path nil))
	(format t "~% Pathname for report =")
	(setq output-path (read-line))
		(case *report-print-mode*
	  (:ASCII (sme::dgroup-set-concordance *memory* output-path))
	  (:LaTeX (sme::dgroup-set-concordance *memory* output-path))))
      :documentation "Makes concordance file for dgroups in MAC/FAC memory.")
     ("Concordance for some dgroups" 
      (let ((output-path nil))
	(format t "~% Pathname for report =")
	(setq output-path (read-line))
	(case *report-print-mode*
	  (:ASCII (sme::dgroup-set-concordance (choose-some-dgroups) output-path))
	  (:LaTeX (sme::LaTeX-dgroup-set-concordance (choose-some-dgroups) output-path))))
      :documentation "Makes concordance file for selected dgroups.")
     ("PP all dgroups" 
      (let ((output-path nil))
	(format t "~% Pathname for report =")
	(setq output-path (read-line))
	(sme::dgroup-set-pp-listing (mapcar #'sme::fetch-dgroup sme::*description-groups*) output-path))
      :documentation "Makes PP listing file for all dgroups loaded.")
     ("PP memory" 
      (let ((output-path nil))
	(format t "~% Pathname for report =")
	(setq output-path (read-line))
	(sme::dgroup-set-pp-listing *memory* output-path))
      :documentation "Makes ground listing file for dgroups in MAC/FAC memory.")
     ("PP some dgroups" 
      (let ((output-path nil))
	(format t "~% Pathname for report =")
	(setq output-path (read-line))
	(sme::dgroup-set-pp-listing (choose-some-dgroups) output-path))
      :documentation "Makes PP listing file for selected dgroups.")
     ("Printing parameters"
      (user::cmenu-choose-values 
       '(("Report Print Mode" *report-print-mode* :one-of (("ASCII" :ASCII) ("LaTeX" :latex)))
	 ("Borders in LaTeX concordances" *Latex-report-borders* :one-of (("Yes" t) ("No" nil)))))
      :documentation "Selecting report modes and switches."))
   "MAC/FAC: Background Report Menu"))

(defun choose-some-dgroups ()
  (do ((menu nil nil) (choice nil)
       (choices nil))
      (nil)
    (dolist (dgname sme::*description-groups*)
      (let ((dgroup (sme::fetch-dgroup dgname)))
	(unless (member dgroup choices)
	  (push (list dgname dgroup) menu))))
    (setq choice (user::cmenu-choose menu))
    (when (or (null choice) (eq choice :punt))
      (return-from choose-some-dgroups choices))
    (push choice choices)))

;;;; Match reports

;; Dumping the bitmaps is the only one so far.
;; Here is where the database generation will occur, though.

(defun menu-macfac-match-reports ()
  (user::run-command-menu
   '(("Dump all match plots" (menu-macfac-mplot-dump)
      :documentation "Dumps bitmaps of each match plot"))
   "MAC/FAC: Match Report Menu"))

(defun menu-macfac-mplot-dump (&aux output-path prefix show?)
  ;; Reads pathname, prefix, and whether or not to show plots as we go.
  (format t "~% Has Lucid graphics system been started?")
  (unless (yes-or-no-p) (format t "~% You must be in the editor to run this.")
    (return-from MENU-MACFAC-MPLOT-DUMP nil))
  (format t "~% Pathname for bitmaps = ")
  (setq output-path (read-line))
  (format t "~% Short distinguishing prefix for file name = ")
  (setq prefix (read-line))
  (format t "~% Display match plots as generated?")
  (setq show? (yes-or-no-p))
  (dump-macfac-match-plots output-path prefix show?))

;;; On-screen information

(defun menu-macfac-inspect ()
  (user::run-command-menu
   '(("Inspect dgroups" (menu-macfac-dgroup-inspect)
      :documentation "Shows information about dgroups")
     ("Inspect MAC" (menu-macfac-mac-inspect)
      :documentation "Shows information about MAC stage of processing")
     ("Inspect FAC" (menu-macfac-fac-inspect)
      :documentation "Shows information about FAC stage of processing"))
   "MAC/FAC: Inspect Menu"))

(defvar *selected-dgroup* nil)

(defun menu-macfac-dgroup-inspect ()
  (user::run-command-menu
   '(("Select dgroup to inspect"
      (let ((choice (choose-a-dgroup "Select dgroup to inspect")))
	(cond ((user::choice-aborted? choice))
	      (t (setq *selected-dgroup* choice))))
      :documentation "Selects a dgroup for further operations.")
     ("Print dgroup" 
      (cond ((sme::dgroup? *selected-dgroup*)
	     (sme::pp-dgroup *selected-dgroup*))
	    (t (format t "~% Please select a dgroup first.")))
      :documentation "Pretty-printed description of selected dgroup.")
     ("Dgroup concordance" 
      (cond ((sme::dgroup? *selected-dgroup*)
	     (sme::dgroup-concordance *selected-dgroup*))
	    (t (format t "~% Please select a dgroup first."))) 
      :documentation "Prints concordance for selected dgroup."))
   "MAC/FAC: Inspect Dgroup Menu"))

(defvar *selected-MAC-match* nil)
(defvar *selected-FAC-match* nil)

(defun choose-a-match (possible-matches &optional (saying "Choose a match"))
  (user::cmenu-choose (mapcar #'(lambda (match)
				  (list (format nil "~A (~D)" 
						(sme::dgroup-name (car match))
						(fourth match))
					match))
			      possible-matches)
		      saying))

(defun menu-macfac-mac-inspect ()
  ;; What are viable options here?
  (user::run-command-menu
   '(("Select MAC match"
      (let ((choice (choose-a-match *mac-match-results* "Select MAC match")))
	(cond ((user::choice-aborted? choice))
	      (t (setq *selected-MAC-match* choice))))
      :documentation "Select a dgroup from memory")   
     ("Describe MAC match"
      (describe-MAC-match *selected-MAC-match*)
      :documentation "Summarize the MAC match.")
     ("Plot MAC match"
      (show-mac-match *selected-MAC-match*)
      :documentation "Plot the MAC match.")
     ("Hide current plot" ;; Need better type predicate for this
      (when *current-plot* (user::send *current-plot* :deexpose)))
     ("Show current plot" ;; Need better type predicate for this
      (when *current-plot* (user::send *current-plot* :expose)))
     ("Print concordances"
      (progn (sme::dgroup-concordance (car *selected-MAC-match*))
	     (sme::dgroup-concordance *probe*))
      :documentation "Print concordances for the dgroups in the match")
     ("Pretty-print base and target "
      (progn (format t "~%=========================== Base ===========================")
	     (sme::pp-dgroup (car *selected-MAC-match*))
	     (format t "~%=========================== Target ===========================")
	     (sme::pp-dgroup *probe*)
	     (format t "~%==========================="))
      :documentation "Print concordances for the dgroups in the match"))
   "MAC/FAC: Inspect MAC Match Menu"))

(defun describe-MAC-match (mac-match)
  (format t "~% Item = ~A, Matcher = ~A~%   Score = ~D~% # Match hypotheses = ~D"
	  (sme::dgroup-name (car mac-match))
	  (matcher-name (cadr mac-match))
	  (fourth mac-match)
	  (length (third mac-match))))

(defun menu-macfac-fac-inspect ()
  ;; What are viable options here?
  (user::run-command-menu
   '(("Select FAC match"
      (let ((choice (choose-a-match *fac-match-results* "Select FAC match")))
	(cond ((user::choice-aborted? choice))
	      (t (setq *selected-FAC-match* choice))))
      :documentation "Select a dgroup from MAC output")   
     ("Describe FAC match"
      (describe-FAC-match *selected-FAC-match*)
      :documentation "Summarize the FAC match.")
     ("Plot FAC match"       (plot-fac-match-menu)
      ;; (show-fac-match *selected-FAC-match*)
      :documentation "Plot the FAC match (sub-menu).")
;     ("Hide current plot" ;; Need better type predicate for this
;      (when *current-plot* (user::send *current-plot* :deexpose)))
;     ("Show current plot" ;; Need better type predicate for this
;      (when *current-plot* (user::send *current-plot* :expose)))
     ("Print concordances"
      (progn (sme::dgroup-concordance (car *selected-FAC-match*))
	     (sme::dgroup-concordance *probe*))
      :documentation "Print concordances for the dgroups in the match")
     ("Pretty-print base and target "
      (progn (format t "~%=========================== Base ===========================")
	     (sme::pp-dgroup (car *selected-FAC-match*))
	     (format t "~%=========================== Target ===========================")
	     (sme::pp-dgroup *probe*)
	     (format t "~%==========================="))
      :documentation "Print concordances for the dgroups in the match"))
   "MAC/FAC: Inspect FAC Match Menu"))

(defun describe-FAC-match (mac-match &aux best)
  (format t
 "~% Item = ~A, Matcher = ~A~%   Score = ~D~% # Match hypotheses = ~D~% # Gmaps = ~D; Best Gmap = ~A."
	  (sme::dgroup-name (car mac-match)) ;; Memory item
	  (matcher-name (cadr mac-match)) ;; Name of matcher
	  (fourth mac-match) ;; Best score
	  (length (third (third mac-match))) ;; # of match hypotheses
	  (length (second (third mac-match))) ;; # of gmaps
	  (setq best (car (third mac-match)))) ;; Best gmap
  (cond ((sme::gm-inferences best) 
	 (format t "~% The candidate inferences are:")
	 (mapcar #'pprint (sme::lispify-candidate-inferences (sme::gm-inferences best) *probe*)))
	(t (format t "~% No candidate inferences."))))

;;;; Interface to display system

(defvar *current-plot* nil)

(defun fetch-plot (legend)
  (dolist (plot *plots*)
    (when (equal (user::send plot :legend) legend)
      (return-from FETCH-PLOT plot)))) 

;;;; ******* WARNING: These assume that MAC is a form of numerosity,
;;;; *******           and that FAC is a form of standard SME match!

(defun show-mac-match (mac-entry &optional (display? t))
  (let* ((legend (list *probe* (car mac-entry) (cadr mac-entry))) 
	 (disp (fetch-plot legend)))
    (unless disp
      (setq disp (user::make-instance 'user::SME-match-display
				      :legend legend
				      :base (car mac-entry)
				      :target *probe*
				      :mhs (third mac-entry)
				      :gmaps nil)))
    (push disp *plots*)
    (user::send disp :setup)
    (when display? (user::send disp :refresh))
    (setq *current-plot* disp)
    (values disp legend)))


(defun hide-plots ()
  (dolist (plot *plots*) (user::send plot :deexpose)))

(defun clear-plots () (hide-plots)(setq *plots* nil))

;;;; Plotting FAC matches
;; ****** Should add a name to the display, and look up old ones rather than re-plotting.
;; ****** Also, this assumes litsim for fac.  Should be data-driven from the matcher.

(defvar *fac-plot* nil)

(defun plot-fac-match-menu ()
  (user::run-command-menu '(("Create plot" (show-fac-match *selected-fac-match* nil)
			     :documentation "Create plot of selected FAC match.")
			    ("Hide plot" (user::send *fac-plot* :deexpose))
			    ("Show best gamp" 
			     (progn (user::send *fac-plot*
						:show-gmap (first (third *selected-fac-match*))))
			     :documentation "Show only the best Gmap from the selected FAC match.")
			    ("Show all gmaps"
			     (progn (user::send *fac-plot* :refresh))
			     :documentation "Show all gmaps from the selected FAC match."))
			  "MAC/FAC: Plot FAC match menu"))

(defun show-fac-match (mac-entry &optional (display? t))
  (let* ((legend (list *probe* (car mac-entry) (cadr mac-entry)))
	 (disp (fetch-plot legend)))
    (unless disp
      (setq disp (user::make-instance 'user::SME-match-display
				      :legend legend
				      :base (car mac-entry)
				      :target *probe*
				      :mhs (third (third mac-entry))
				      :gmaps (second (third mac-entry)))))
    (push disp *plots*)
    (user::send disp :setup)
    ;;(user::send disp :refresh)
    (when display? (user::send disp :show-gmap (first (third mac-entry))))
    (setq *current-plot* disp)
    (setq *fac-plot* disp)
    (values disp legend)))
