;;; -*- Mode: LISP; Syntax: Common-lisp; Package: SME; Base: 10; Fonts: CPTFONT,TR12I -*-
#-Symbolics
(in-package "SME" :use (list sme::*the-lisp-package*))

;;; Structure-Mapping Engine  (module display.lisp: machine-independent printout)

;;; Copyright (C) 1986,  Brian C. Falkenhainer, University of Illinois
;;; All rights reserved

;;; Display / User Interface routines for SME

(export '(sme-format sme-print sme-terpri compare-gmaps describe-dgroup display-match
	  *sme-output-stream* *dgroup-statistics* *gmap-display* *gmap-statistics*
          *display-all-MH*))

(defvar *sme-output-stream* T "pane (scroll or window) or stream where output goes")

(defvar *dgroup-statistics* t "Display the Dgroup statistics table")
(defvar *gmap-display* :all "Only display the best Gmap, not MH's or other Gmaps")
(defvar *gmap-statistics* :none "Display the Gmap statistics table")
(defvar *display-all-MH* t "Display All the Match Hypotheses?")

(defSME-Parameter *display-all-MH* "PRINT> Display all the Match Hypotheses?" :boolean)
(defSME-Parameter *dgroup-statistics* "PRINT> Display the Dgroup statistics table?" :boolean)
(defSME-Parameter *gmap-statistics* "PRINT> Display the Gmap statistics table?"
                  :assoc (("All Gmaps" . :all)("Best Gmaps" . :best)("No Gmaps" . :none)))
(defSME-Parameter *gmap-display* "PRINT> Which Gmaps to display"
                  :assoc (("All Gmaps" . :all)("Best Gmaps" . :best)("No Gmaps" . :none)))



;;;; General output interface (sme-format, etc.)

;;; Under this scheme, all output goes to *sme-output-stream* and is
;;;  placed there according to its type. Thus, all print routines need
;;;  to go through the handler, rather than format, etc. 


;;; Allow the use of regular format-like calls to go to *sme-output-stream*
;;;
(defmacro sme-format (format-string &rest args)
  `(sme-print (format nil ,format-string ,@args)))


;;; Given a string with potential line-feeds in it, print out each line
;;;   to *sme-output-stream*. If *sme-output-stream* is a scroll-window,
;;;   it will append the items to the scroll items list. If it is not a
;;;   scroll window, it will merely use format.
;;;
;;;     A new line is always printed at the end of the string, whether
;;;     or not one was actually there....
;;;
(defun sme-print (string)
  (let ((append? (and (not (eq *sme-output-stream* T))
		      #+Symbolics (scl:get-handler-for *sme-output-stream* :append-item)
		      #-Symbolics NIL	;assume there will never be an :append handler
		      )))
    (with-input-from-string (stream string)
          (do ((line (read-line stream nil) (read-line stream nil)))
	      ((null line))
	    (if append?
		(funcall *sme-output-stream* :append-item (list line))
		(format *sme-output-stream* "~A~%" line))))))


(defun sme-terpri (&optional (n 1))
  (dotimes (i n) (sme-print (string #\newline))))


;;;; Miscellaneous printout routines

;;; Provide the user with the list of currently loaded
;;;   description-groups and textually describe the one choosen.
;;;
(defun describe-dgroup (dgroup &optional terse?)
  (sme-print (with-output-to-string (stream)
	       (format stream "~%~15TDescription of ~A~%" dgroup)))
  (display-dgroup-statistics dgroup)
  (sme-print
    (with-output-to-string (stream)
      (cond (terse?
	     (dolist (exp (sort (copy-list (dgroup-roots dgroup)) #'>
				 :key #'expression-order))
	       (format stream "~%~A~20T(~A):~25T" (expression-name exp)
		                                  (expression-order exp))
	       (format stream (special-output
				(subseq (with-output-to-string (stream2)
					  (pprint (fully-expand-expression-form
						    (expression-original-lisp-form exp)
						    dgroup)
						  stream2))
					1)
				25)))
	     (format stream "~%~%ENTITIES: ~A" (dgroup-entities dgroup)))
	    (t
	     (dolist (exp (sort (copy-list (dgroup-expressions dgroup)) #'>
				 :key #'expression-order))
	       (format stream "~%~A~20T(~A)~25TUSER LISP FORM: ~A"
		       (expression-name exp) (expression-order exp)
		       (expression-original-lisp-form exp))
	       (format stream "~%~25Tsys lisp form : ~A"
		       (expression-sys-lisp-form exp)))
	     (format stream "~%~%ENTITIES: ~A" (dgroup-entities dgroup))
	     (format stream "~%ROOTS: ~A~%~%" (dgroup-roots dgroup)))))))


;;; Dgroup statistics - depth
;;;  Returns (1) the maximum expression order and
;;;          (2) the average expression order, where entities are
;;;              included as 0 order expressions
;;;
(defun analyze-dgroup-depth (dgroup)
  (do ((exps (dgroup-expressions dgroup) (cdr exps))
       (max 0)
       (total 0)
       (cnt 0 (1+ cnt)))
      ((null exps)
       (values max
	       (/ total (+ (float cnt) (length (dgroup-entities dgroup))))))
    (incf total (expression-order (car exps)))
    (if (> (expression-order (car exps)) max) (setq max (expression-order (car exps))))))

;;; Return a list of the predicate order distribution of this Dgroup.
;;;   Format: (#entities  #1st order  ....)
;;;
(defun dgroup-order-list (dgroup max-order)
  (let ((olist (make-sequence 'list (1+ max-order) :initial-element 0)))
    (setf (car olist) (length (dgroup-entities dgroup)))  ;zero-order entities
    (dolist (exp (dgroup-expressions dgroup))
       (incf (nth (expression-order exp) olist)))
    olist))


;;; Dgroup statistics - breadth
;;;   Max breadth is defined as the largest size of the tree at a
;;;     particular level in the tree
;;;
(defun analyze-dgroup-breadth (dgroup)
  (let ((max-breadth 0) (total 0) (breadth 0))
    (dolist (root (dgroup-roots dgroup))
      (setq breadth (get-expression-breadth root))
      (if (> breadth max-breadth)
	  (setq max-breadth breadth))
      (incf total breadth))
    (values max-breadth (/ total (float (length (dgroup-roots dgroup)))))))


(defun get-expression-breadth (root)
  (do ((current-level (expression-arguments root) next-level)
       (next-level nil nil)
       (max-b (length (expression-arguments root)))	;size of current level
       (current-b 0 0))
      ((null current-level) max-b)
    (dolist (item current-level)		;do next level
       (when (expression? item)
	 (incf current-b (length (expression-arguments item)))
	 (setq next-level (append (expression-arguments item) next-level))))
    (if (> current-b max-b) (setq max-b current-b))))



;;;; Match display routines
;;;    The idea here is to modularize the different report generation
;;;     facilities so that the user may pick and choose what the desired
;;;     output should be from among these possibilities


;;; Display the results of a match
;;;
(defun display-match (base target &optional (total-run-time 0) (bms-run-time 0))
  (display-match-header base target total-run-time bms-run-time)
  (when *display-all-MH*
    (sme-print				;Print out all the MH's
      (with-output-to-string (stream)
	(let ((*standard-output* stream))
	  (format stream "~%Match Hypotheses:")
	  (dolist (mh *match-hypotheses*)
	    (format stream "~%~5T(~,4F  ~,4F)  ~A"
		    (node-belief+ (mh-bms-node mh))  (node-belief- (mh-bms-node mh))
		    (mh-printer mh)))))))
  (case *gmap-display*
    (:all  (dolist (gmap *gmaps*) (display-a-gmap gmap)))         ;Print all GMaps
    (:best (dolist (gmap (best-gmaps)) (display-a-gmap gmap))))	  ;Print only the best Gmap(s)
  (sme-terpri))


(defun display-match-header (base target &optional (total-run-time 0) (bms-run-time 0))
  (sme-print					;Print header of stats
    (with-output-to-string (stream)
      (format stream "~%~15TSME Version ~A" *version*)
      (format stream "~%~5TAnalogical Match from ~A to ~A.~%"
	      (dgroup-name base) (dgroup-name target))
      (format stream "~%Rule File: ~A" *match-rules-file*)))
  (if *dgroup-statistics* (display-dgroup-statistics base target))   ;Dgroup stats
  (case *gmap-statistics*			                    ;Gmap stats
    (:all  (display-gmap-statistics *gmaps*))
    (:best (display-gmap-statistics (best-gmaps))))
  (display-match-statistics)			                    ;Match stats
  (sme-print
    (with-output-to-string (stream)
      (format stream "Total Run Time: ~4D Minutes, ~6,3F Seconds"
	      (floor (/ total-run-time 60))
	      (mod total-run-time 60))
      (format stream "~%BMS Run Time: ~6D Minutes, ~6,3F Seconds"
	      (floor (/ bms-run-time 60))
	      (mod bms-run-time 60))
      (let ((best (best-gmaps)))
	(when best
	  (format stream "~%Best Gmaps: { ~D" (gm-id (car best)))
	  (dolist (a-best (cdr best)) (format stream ", ~D" (gm-id a-best)))
	  (format stream " }"))))))


;;; Dgroup statistics
;;;   Table showing Dgroup stats like number of entities and expressions,
;;;     average item orders, etc.
;;;
(defun display-dgroup-statistics (&rest dgroups)
  (sme-print					;Print header of stats
    (with-output-to-string (stream)
      (let ((*standard-output* stream))
	(format stream "~95,,,'-A" "")		;string of x number of "-"
	
	(format stream "~% Dgroup Name      | # Entities | # Expr. | Max/Ave Depth | ~
                          M/A Breadth | Predicate Orders     |")
	(dolist (dgroup dgroups)
	  (multiple-value-bind (d-max d-ave) (analyze-dgroup-depth dgroup)
	    (multiple-value-bind (b-max b-ave) (analyze-dgroup-breadth dgroup)
	      (format stream "~%~17A |    ~4D    |   ~4D  |  ~3D /~5,2F   | ~
                            ~3D /~5,2F  | ~20A |"
		      (if (> (length (symbol-name (dgroup-name dgroup))) 17)
			  (subseq (symbol-name (dgroup-name dgroup)) 0 17)
			  (dgroup-name dgroup))
		      (length (dgroup-entities dgroup))  (length (dgroup-expressions dgroup))
		      d-max d-ave b-max b-ave (dgroup-order-list dgroup d-max)))))
	(format stream "~%~95,,,'-A" "")))))
  

;;; Gmap statistics
;;;   Table of information on each Gmap individually
;;;
(defun display-gmap-statistics (gmaps)
  (sme-print
    (with-output-to-string (stream)
      (format stream "~113,,,'-A" "")
      (format stream "~%Gmap | Weight | #MH's | #Emaps | Max/Ave Depth | M/A Breadth ~
                        | Predicate Orders     | #CI's/Order               |")
      (dolist (gmap gmaps)
	(multiple-value-bind (d-max d-ave) (analyze-gmap-depth gmap)
	  (multiple-value-bind (b-max b-ave) (analyze-gmap-breadth gmap)
	    (format stream "~% ~3D | ~6,2F | ~4D  |  ~4D  |  ~3D /~5,2F   | ~3D /~5,2F  ~
                              | ~20A | ~3D: ~20A |"
		  (gm-id gmap)
		  (node-belief+ (gm-bms-node gmap))
		  (length (gm-elements gmap))
		  (length (gm-emaps gmap))
		  d-max  d-ave
		  b-max  b-ave
		  (gmap-order-list gmap d-max)
		  (length (gm-inferences gmap))
		  (or (getf (gm-plist gmap) :ci-orders) "()")))))
      (format stream "~%~113,,,'-A" ""))))


;;; Match statistics
;;;   Match summarizing information, such as number of Gmaps,
;;;     what match modes where set at, etc.
;;;
(defun display-match-statistics ()
  (labels ((second-score (best gmaps)
	     (let ((lower-bound (- best (* best 0.02)))
		   (ceiling 0.0))
	       (dolist (gmap gmaps)
		 (when (and (< (node-belief+ (gm-bms-node gmap)) lower-bound)
			    (> (node-belief+ (gm-bms-node gmap)) ceiling))
		   (setq ceiling (node-belief+ (gm-bms-node gmap)))))
	       (if (= ceiling 0) best ceiling)))
	   (worst-score (best gmaps)
	     (dolist (gmap gmaps best)
	       (if (<  (node-belief+ (gm-bms-node gmap))  best)
		   (setq best (node-belief+ (gm-bms-node gmap)))))))
    (sme-print
      (with-output-to-string (stream)
	(multiple-value-bind (ignore best-score) (best-gmaps)
	  (format stream "~93,,,'-A" "")
	  (format stream "~%# MH's | # Gmaps |      1st,2nd,Worst    |  STD  ~
                            | Merge Step 3 |   CI   | RelGroups |  1-1 |")
	  (format stream "~% ~4D  |  ~4D   |~6,2F /~6,2F /~6,2F | ~5,2F | ~8@A     ~
                            | ~6@A |  ~6@A   | ~4@A |"
		  (length *match-hypotheses*)
		  (length *gmaps*)
		  best-score
		  (if *gmaps* (second-score best-score *gmaps*) 0)
		  (if *gmaps* (worst-score best-score *gmaps*) 0)
		  0.0				;Eventually calculate standard deviation
		  (if *Gmap-merge-step3?* 'active 'off)
		  (if *CI-flag* 'active 'off)
		  (if *rel-groups?* 'active 'off)
		  (if *one-to-one?* 'full 'user))
	  (format stream "~%~93,,,'-A" ""))))))


;;; Display a single GMap
;;;
(defun display-a-gmap (gmap &aux index clength slength)
  (sme-print
    (with-output-to-string (stream)
      (let ((*standard-output* stream))
	(setq index 0)
	(format stream "~%Gmap #~A:~10T" (gm-id gmap))
	(dolist (mh (gm-elements gmap))
	  (incf index)
	  (cond ((> index 3)
		 (format stream "~%~11T ~A" (mh-printer mh))
		 (setq index 1))
		((format stream "  ~A" (mh-printer mh)))))
	(setq clength 0)
	(format stream "~%  Emaps:")
	(dolist (emap (gm-emaps gmap))		;Emaps
	  (setq slength (+ 4 (length (symbol-name (entity-name (second emap))))
			     (length (symbol-name (entity-name (third emap))))))
	  (cond ((> (+ clength slength) 90)
		 (format stream "~%~9T (~A ~A)" (entity-name (second emap))
			                        (entity-name (third emap)))
		 (setq clength slength))
		(T (incf clength slength)
		   (format stream " (~A ~A)" (entity-name (second emap))
			                     (entity-name (third emap))))))
	(format stream "~%  Weight: ~,4F" (node-belief+ (gm-bms-node gmap)))
	(format stream "~%  || # MH's: ~D || # Emaps: ~D"
		(length (gm-elements gmap))  (length (gm-emaps gmap)))
	(multiple-value-bind (gm-max gm-ave) (analyze-gmap-depth gmap)
	  (format stream " || Max/Ave Order: ~D/~,2F" gm-max gm-ave)
	  (format stream " || Predicate Orders: ~A ||" (gmap-order-list gmap gm-max)))
	(format stream "~%  Candidate Inferences:")
	(cond ((gm-inferences gmap)		;Candidate Inferences
	       (format stream "  ")
	       (format stream (special-output (subseq (with-output-to-string (stream2)
							(pprint (car (gm-inferences gmap))
								stream2))
						      1)))
	       (dolist (ci (cdr (gm-inferences gmap)))
		 (format stream (special-output (with-output-to-string (stream2)
						  (pprint ci stream2))))))
	      ((format stream "  { }"))))))
  (sme-terpri))


;;; Gmap statistics
;;;  Returns (1) the maximum MH order and (2) the average MH order
;;;
(defun analyze-gmap-depth (gmap)
  (do ((mhs (gm-elements gmap) (cdr mhs))
       (max 0)
       (total 0)
       (cnt 0 (1+ cnt)))
      ((null mhs)
       (values max (/ total (float cnt))))
    (incf total (mh-order (car mhs)))
    (if (> (mh-order (car mhs)) max) (setq max (mh-order (car mhs))))))


;;; Gmap statistics - breadth
;;;
(defun analyze-gmap-breadth (gmap)
  (let ((max-breadth 0) (total 0) (breadth 0))
    (dolist (mh (gm-root-elements gmap))
      (setq breadth (get-mh-breadth mh))
      (if (> breadth max-breadth) (setq max-breadth breadth))
      (incf total breadth))
    (values max-breadth (/ total (float (length (gm-root-elements gmap)))))))

(defun get-mh-breadth (mh-root)
  (do ((current-level (mh-justifies mh-root) next-level)
       (next-level nil nil)
       (max-b (length (mh-justifies mh-root)))
       (current-b 0 0))
      ((null current-level) max-b)
    (dolist (item current-level)		;do next level
       (when (eq :expression (mh-type item))
	 (incf current-b (length (mh-justifies item)))
	 (setq next-level (append (mh-justifies item) next-level))))
    (if (> current-b max-b) (setq max-b current-b))))



;;; Return a list of the predicate order distribution of this Gmap.
;;;   Format: (#emaps  #1st order  ....)
;;;
(defun gmap-order-list (gmap max-order)
  (let ((olist (make-sequence 'list (1+ max-order) :initial-element 0)))
    (dolist (mh (gm-elements gmap))
       (incf (nth (mh-order mh) olist)))
    olist))


(defun special-output (string &optional (tab 26))
  (apply #'concatenate
	 (cons 'string
	       (map 'list
		    #'(lambda (ch) (if (eql #\return ch) (format nil "~%~~~DT" tab)
				                         (string ch)))
		    string))))


(defun mh-printer (mh)
  (format nil "(~A ~A)"
	  (if (expression? (mh-base-item mh))
	      (expression-name (mh-base-item mh))
	      (entity-name (mh-base-item mh)))
	  (if (expression? (mh-target-item mh))
	      (expression-name (mh-target-item mh))
	      (entity-name (mh-target-item mh)))))


;;; Given two Gmaps, identify the two Match Hypothesis sets:
;;;    (Gmap1 - Gmap2) and (Gmap2 - Gmap1)
;;;
(defun compare-gmaps (gmap1 gmap2 &optional display?)
  (labels ((display-mhs (mhs)
	     (sme-print
	       (with-output-to-string (stream)
		 (let ((*standard-output* stream)
		       (index 0))
		   (format stream "~%~4T")
		   (dolist (mh mhs)
		     (incf index)
		     (cond ((> index 3)
			    (format stream "~%~5T~A" (mh-printer mh))
			    (setq index 1))
			   ((format stream " ~A" (mh-printer mh))))))))))
    (unless (and gmap1 gmap2) (return-from compare-gmaps nil))
    (let ((cmark1 (list t t))
	  (cmark2 (list t t))
	  uniquely1 uniquely2)
      (dolist (mh (gm-elements gmap1))
	(setf (getf (mh-plist mh) :mark1) cmark1))
      (dolist (mh (gm-elements gmap2))
	(setf (getf (mh-plist mh) :mark2) cmark2)
	(unless (eq (getf (mh-plist mh) :mark1) cmark1)
	  (push mh uniquely2)))
      (dolist (mh (gm-elements gmap1))
	(unless (eq (getf (mh-plist mh) :mark2) cmark2)
	  (push mh uniquely1)))
      (when display?
	(sme-format "~%Matches uniquely part of Gmap ~D:" (gm-id gmap1))
	(if uniquely1
	    (display-mhs uniquely1)
	    (sme-format "~%  <empty>"))
	(sme-format "~%Matches uniquely part of Gmap ~D:" (gm-id gmap2))
	(if uniquely2
	    (display-mhs uniquely2)
	    (sme-format "~%  <empty>")))
      (values uniquely1 uniquely2))))
