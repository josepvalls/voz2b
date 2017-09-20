;;; -*- Mode: LISP; Syntax: Common-lisp; Package: SME; Base: 10; -*-
#-Symbolics
(in-package "SME" :use sme::*the-lisp-package*)

;;;; Structure-Mapping Engine  (module batch.lisp: running SME from a command file)
;;;
;;; Taken from suggestions by Ken Forbus and Janice Skorstad
;;;

(export '(language-file dgroup-directory dgroup-file rule-directory rule-file
	  rule-sets report-comments send-report-to run-matcher-on run-batch-file))

(defvar *sme-report-text-driver* :LPR "for example, :LPR, :TROFF, or :LATEX")
(defvar *sme-report-style* :STANDARD ":STANDARD or :TABULAR")

(defvar *sme-report-stream* nil)
(defvar *sme-report-comments* "")
(defvar *sme-report-rule-sets* nil "list of rule files to iterate over")

(defvar *sme-reports* nil "list of report information on each match performed")
(defvar *sme-current-report* nil "current storage for a match report being generated")

(defvar *sme-max-gmaps* 20 "max number of gmaps to be printed in tabular mode")
(defvar *sme-num-gmaps* 10 "number of gmaps to print if there's more than *sme-max-gmaps*")

(defvar *line-count* 0 "used in :TABULAR mode to indicate how many lines printed so far")

(defvar *user-report-postmatch-processor* nil "user function called after each sme-match")

;;;; User commands

;;; Language
;;;
(defmacro Language-File (name)
  `(load (setq sme::*sme-language-file* ,name) :verbose nil))

;;; Dgroups
;;;
(defmacro Dgroup-Directory (name)
  `(setq sme::*sme-dgroup-pathname* (format nil "~A*.dgroup.newest" ,name)))

(defmacro Dgroup-File (name) `(load (merge-pathnames ,name sme::*sme-dgroup-pathname*)
				    :verbose nil))

;;; Rules
;;;
(defmacro Rule-Directory (name)			;Directory for all rule files
  `(setq sme::*sme-rules-pathname* ,name))

(defmacro Rule-File (name)		;Run each match using a single rule file
  `(progn (setq sme::*sme-report-rule-sets* nil)
	  (load (merge-pathnames ,name sme::*sme-rules-pathname*) :verbose nil)))

(defmacro Rule-Sets (&rest rule-files)	;Run each match over a series of rule files
  `(setq sme::*sme-report-rule-sets* ',rule-files))

;;; Miscellaneous macros used in tabular mode
(defmacro Num-Gmaps (num)
  `(setq sme::*sme-num-gmaps* ,num))

(defmacro Max-Gmaps (num)
  `(setq sme::*sme-max-gmaps* ,num))

;;; Some user defined comment string to appear in the header of the report
;;;
(defmacro Report-Comments (comments)
  `(if (streamp sme::*sme-report-stream*)
       (sme::dump-report-comments sme::*sme-report-stream* ,comments)	;already opened
       (setq sme::*sme-report-comments* ,comments)))  ;wait until stream opened


;;; Full name of file that the report should go to
;;;    MUST APPEAR IN FILE BEFORE ANY OUTPUT REQUIRED
;;;
(defmacro Send-Report-To (fname &key (text-driver :LPR) (style :STANDARD))
  `(progn (setq sme::*sme-report-stream* (open ,fname :direction :output))
	  (setq sme::*sme-report-text-driver* ,text-driver)	;better be a keyword
	  (setq sme::*sme-report-style* ,style)
	  (sme::generate-sme-report-header sme::*sme-report-stream*)))


;;; Run a match when just a rule file is specified
;;;  or a series of matches when a set of rule files are specified
;;;
(defmacro Run-Matcher-on (base target)
  `(progn (if sme::*sme-report-rule-sets*
	      (dolist (rule-file sme::*sme-report-rule-sets*)
		(load (merge-pathnames rule-file sme::*sme-rules-pathname*) :verbose nil)
		(sme::run-sme-on ',base ',target))
	      (sme::run-sme-on ',base ',target))
	  (setq *line-count* 0)))

;;; Function called after each invocation of sme:match and its
;;;   associated report generation. This can allow user to run
;;;   other procedures, generate additional report info, etc.
;;;
(defmacro defPostMatcher (function-name)
  `(setq sme::*user-report-postmatch-processor* ',function-name))


;;;; Running a batch file

(defun run-batch-file (fname &key
			     dont-display-mhs dgroup-statistics
			     (gmap-display :all) (gmap-statistics :none))
    (setq *gmap-display* gmap-display)
    (setq *gmap-statistics* gmap-statistics)
    (setq *dgroup-statistics* dgroup-statistics)
    (setq *display-all-MH* (not dont-display-mhs))
    (setq *sme-reports* nil)
    (setq *sme-report-comments* nil)
    (setq *sme-report-stream* nil)
    (setq *sme-report-rule-sets* nil)
    (setq *sme-num-gmaps* 10)
    (setq *sme-max-gmaps* 20)
    (setq *user-report-postmatch-processor* nil)
    (unless *initialized?* (sme-init))
    (load fname :verbose nil)			;run those commands....
    (conclude-sme-report))


;;; Actually run SME on a base/target Dgroup pair and produce
;;;   any desired intermediate report output
;;;
(defun run-sme-on (base target)
  (let ((base-dgroup (fetch-dgroup base))
	(target-dgroup (fetch-dgroup target)))
    (unless (and base-dgroup target-dgroup)
        (error "Dgroup ~A has been referenced but is not yet defined."
	       (if base-dgroup target base)))
    (multiple-value-bind (total-time bms-time)
	 (match base target nil)   ;don't display yet
       (if *user-report-postmatch-processor* (funcall *user-report-postmatch-processor*))
       (generate-match-report base-dgroup target-dgroup total-time bms-time))))


;(defun generate-match-report (base-dgroup target-dgroup total-time bms-time)
;  (let ((*sme-output-stream* *sme-report-stream*))
;    (case *sme-report-text-driver*
;      (:LPR (sme-print (string #\Page))
;	    (display-match base-dgroup target-dgroup total-time bms-time))
;      (:TROFF (sme-print ".bp")
;	      (display-match base-dgroup target-dgroup total-time bms-time))
;      (:LATEX (sme-print "\\newpage")
;	      (display-match base-dgroup target-dgroup total-time bms-time)))))


;;; This is where data for each individual match is printed out or collected.
;;;  Right now, the known report styles and text-drivers allow us to
;;;    just print it out now. Some more numerical or database type report
;;;    formats might prefer to just collect this info now and print it
;;;    all out in conclude-sme-report or print out a tabular style at
;;;    each match by bypassing the standard display-match function.
;;;
(defun generate-match-report (base-dgroup target-dgroup total-time bms-time)
  (let ((*sme-output-stream* *sme-report-stream*))
    (case *sme-report-style*
      (:STANDARD (display-match base-dgroup target-dgroup total-time bms-time)
		 (case *sme-report-text-driver*
		   (:LPR (sme-print (string #\Page)))
		   (:TROFF (sme-print ".bp"))
		   (:LATEX (sme-print "\\newpage"))))
      (:TABULAR (setq *line-count* 0)		; keep track of number of lines printed
		(tabular-header)
		(tabular-stats total-time bms-time)
		(sme-format "~%~73,,,'=A" '=))
      (:USER ()))))


;;;; Gathering statistics

;;; Right now, the standard SME output routines suffice for
;;;   on-the-fly print out and statistics.
;;;  The global variables *sme-reports* and *sme-current-report*
;;;   are provided to enable one to gather and store info on each
;;;   match for possible summarization in conclude-sme-report.



;;;; Report headers

(defun generate-sme-report-header (stream)
  (case *sme-report-text-driver*
    (:LPR (lpr-report-header stream))
    (:TROFF (troff-report-header stream))
    (:LATEX (latex-report-header stream))
    (T (error "~%~%~S is an unknown format for SME reports.~%" *sme-report-text-driver*))))

(defun lpr-report-header (stream)
  (format stream "~%~20@TSME Statistical Report")
  (multiple-value-bind (sec min hour date month year) (get-decoded-time)
    (format stream "~%~23@T~2,'0D/~2,'0D/~2D ~2,'0D:~2,'0D:~2,'0D~%~%"
	           month date (mod year 100) hour min sec))
  (if *sme-report-comments* (dump-report-comments stream *sme-report-comments*)))


(defun troff-report-header (stream)
  (multiple-value-bind (sec min hour date month year) (get-decoded-time)
    (format stream ".he '\\fBSME Statistical Report\\fP'%'\\fB~2,'0D/~2,'0D/~2D  ~2,'0D:~2,'0D:~2,'0D'\\fP"
	    month date (mod year 100) hour min sec))
  (format stream "~%.nf~%.ft TA~%.ss 22~%.po 0.5i~%.ll 6.9i~%.sz 9~%")
  (if *sme-report-comments* (dump-report-comments stream *sme-report-comments*)))


(defun latex-report-header (stream)
  (format stream "~%\\batchmode~%\\documentstyle[alltt]{article}~%\\pagestyle{myheadings}")
  (format stream "~%\\markboth{SME Statistical Report}{SME Statistical Report}")
  (format stream "~%\\setlength{\\oddsidemargin}{-0.4in}~%\\setlength{\\evensidemargin}{0in}")
  (format stream "~%\\setlength{\\textwidth}{6.8in}~%\\setlength{\\topmargin}{-0.3in}")
  (format stream "~%\\setlength{\\textheight}{9in}~%\\begin{document}~%\\begin{small}")
  (format stream "~%{\\center {\\bf SME Statistical Report \\\\")
  (multiple-value-bind (sec min hour date month year) (get-decoded-time)
    (format stream "~%              ~2,'0D/~2,'0D/~2D ~2,'0D:~2,'0D:~2,'0D \\\\[2ex] }}"
	    month date (mod year 100) hour min sec))
  (format stream "~%\\begin{alltt}")
  (if *sme-report-comments* (dump-report-comments stream *sme-report-comments*)))
 

(defun dump-report-comments (stream comment-string)
  (case *sme-report-text-driver*
    ((:LPR :TROFF :LATEX) (format stream "~%~A~%" comment-string))))


;;;; Report conclusions

;;; Any concluding, summarizing remarks?
;;;  e.g., a database report format might want to calculate some overall numbers
;;;
(defun conclude-sme-report ()
  (if (eq *sme-report-text-driver* :LATEX)
      (format *sme-report-stream* "~%\\end{alltt}~%\\end{small}~%\\end{document}~%"))
  (close *sme-report-stream*))			;nothing right now...


;;; Miscellaneous functions for :TABULAR mode of sme-report

;;; Display desired number of gmaps in tabular format.
;;; The desired number of gmaps are indicated by setting the
;;;   variable "num-gmaps" in the batch file.
(defun tabular-gmaps (&optional (percentage-range 0.02))
  (let ((gmaps *gmaps*))
    (cond ((equal *gmap-display* ':BEST)
	   (setq gmaps (best-gmaps gmaps percentage-range)))
	  ((equal *gmap-display* ':ALL)
	   ; are there more gmaps than we want to print?  Then just print the best.
	   (if (> (length gmaps) *sme-max-gmaps*)	      
	       (setq gmaps (n-best-gmaps gmaps *sme-num-gmaps* percentage-range))))
	  (t (return-from tabular-gmaps  "You requested no gmaps be printed")))
    (dolist (gmap gmaps)      
      (sme-print
	(with-output-to-string (stream)
	  (let ((*standard-output* stream))
	    (format stream "~30T~3D:~16A ~2D:~8A ~10,3F"	; print out the gmaps
		    (gm-id gmap) (gmap-order-list gmap (analyze-gmap-depth gmap))
		    (length (gm-inferences gmap))
		    (or (getf (gm-plist gmap) :ci-orders) "()")
		    (node-belief+ (gm-bms-node gmap)))		    
	    (format stream "~%~73,,,'-A" '-)
	    (setq *line-count* (+ 2 *line-count*)))))
      (if (>= *line-count* 55)			; check for end of page
	     (tabular-header)))))


; Done for each set of match rules or when 50 lines of data have been printed.
(defun tabular-header ()
  (case *sme-report-text-driver*
    (:LPR (sme-print (string #\Page))
	  (setq *line-count* 11))
    (:TROFF (sme-print ".bp")
	    (setq *line-count* 17))
    (:LATEX (sme-print "\\newpage")
	    (setq *line-count* 17)))
  (sme-print
    (with-output-to-string (stream)
	(let ((*standard-output* stream))	
	  (format stream "~%~30TSME Version ~A" *version*)
	  (format stream "~%~15TAnalogical Match from ~A to ~A"
		  (dgroup-name *base*) (dgroup-name *target*))
	  (if (not (= 0 *line-count*))                    ; continuation?
	      (format stream ", CONTINUED."))
	  (format t "~3%~3TRule File:  ~18A          Total Run Time:  ~7,3F"
		  (pathname-name *match-rules-file*)  *total-run-time*)
	  (format stream "~%~3TBest Gmaps: ~18A          Bms Run Time:    ~7,3F"
		  (if (= (length (best-gmaps *gmaps*)) (length *gmaps*))
		      "ALL" (best-gmaps *gmaps*)) *bms-run-time*)	 
	  (format stream "~%~73,,,'-A" '-)
	  (format stream "~%|   Base & Target    |#Gmaps|     Gmap ID#:      |   ~
                  #CIs:   |   SUM    |")
	  (format stream "~%|  Predicate Orders  |      |     Pred Order     |   ~
                  Order   |  Weight  |")
	  (format stream "~%~73,,,'-A" '-))))
  (setq *line-count* 11))

;;;  Display the SME rules file along with the total and bms run times
(defun tabular-stats (&optional (total-time 0.0) (bms-time 0.0) (percentage-range .02))
  (if (>= *line-count* 50)		; enough room left to print out some stats?
      (tabular-header))
  (sme-print
    (with-output-to-string (stream)
      (let ((*standard-output* stream))
	(format stream "~21A~6D" (dgroup-order-list *base* (analyze-dgroup-depth *base*))
		(length *gmaps*))
	(format stream "~%~21A" (dgroup-order-list *target*
						   (analyze-dgroup-depth *target*))))))
  (tabular-gmaps percentage-range))

;;; Get the n best gmaps from the given list of gmaps
;;; If there aren't "num-gmaps" within the specified percentage-range,
;;; the standard of what "best" means is lowered in order to get the desired
;;; number of gmaps.
(defun n-best-gmaps (&optional (gmaps *gmaps*) (num-gmaps 10) (percentage-range 0.02))
  (let ((gms gmaps))
    (do* ((n-best-gmaps nil (append n-best-gmaps best-gmaps))
	  (best-gmaps (best-gmaps gms percentage-range)
		      (best-gmaps gms percentage-range))
	  (cnt (length best-gmaps) (+ cnt (length best-gmaps))))
	 ((= cnt num-gmaps) (append n-best-gmaps best-gmaps))
      (if (> cnt num-gmaps)	; if there are more best gmaps than desired, remove extras
	  (return (subseq (append n-best-gmaps best-gmaps) 0 num-gmaps))	  
	  (dolist (gm best-gmaps)  ; remove the best from gms & get some new best gmaps
	    (setq gms (remove gm gms)))))))
