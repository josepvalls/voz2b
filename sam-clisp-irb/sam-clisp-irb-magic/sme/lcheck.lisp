;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: user; -*-

;;;; Checking language files

;;; Builds up a glossary from a language file (or a set of
;;;  of them) to look for multiple and inconsistent definitions

;; Should add something to gather statistics on language files: how many functors of
;; each type, etc.

(defvar *sme-glossary* nil) ;; A hash table

(defun clear-glossary ()
  (cond (*sme-glossary* (clrhash *sme-glossary*))
	(t (setq *sme-glossary* (make-hash-table)))))

(defun load-language-file (lfile &aux marker)
  (setq marker (list nil))
  (with-open-file (in lfile :direction :input)
   (do ((form (read in nil  marker) (read in nil marker)))
       ((eq form marker) lfile)
     (when (listp form)
       (case (car form)
	 (defSMEType (add-to-glossary (cadr form) lfile form))
	 (sme::defPredicate (add-to-glossary (cadr form) lfile form))
	 (t (format t "~% Ignoring ~A.." form)))))))

(defun add-to-glossary (key subkey item)
  (let ((entry (gethash key *sme-glossary*)))
    (unless entry
      (setq entry (list key))
      (setf (gethash key *sme-glossary*) entry))
    (setf (cdr entry) (cons (list subkey item) (cdr entry)))))

(defun delete-from-glossary (subkey)
  (maphash #'(lambda (key value)
	       (let ((subentry (assoc subkey value)))
		 (when subentry
		   (setf (gethash key *sme-glossary*)
			 (delete subentry (cdr value))))))
	   *sme-glossary*))

;;;; Looking for collisions and inconsistencies

;; Three kinds: redundant definition within a file,
;; inconsistent definitions within a file,
;; and inconsistent definitions across files

(defun check-glossary ()
  (maphash #'(lambda (key value)
	       ;; build up an alist of alternate definitions
	       (let ((alist nil)
		     (entry nil))
		 (dolist (oentry (cdr value))
		   (setq entry (assoc (car oentry) alist :test #'string=))
		   (unless entry
		     (setq entry (cons (car oentry) nil))
		     (push entry alist))
		   (setf (cdr entry) (cons (cdr oentry) (cdr entry))))
		 ;; Look for redundant definitions
		 (dolist (entry alist)
		   (when (cddr entry)
		     ;; See if consistent or not
		     (cond ((same-sme-definitions? (cdr entry))
			    (format t "~% Redundant definitions of ~A in ~A"
				    key (car entry)))
			   (t (format t "~% Inconsistent definitions of ~A within ~A"
				      key (car entry))))))
		 ;; Look for conflicts between files
		 (when (cdr alist) ;; defined in more than one place
		   (do ((entries alist (cdr entries))
			(lost? nil))
		       ((null (cdr entries))
			(unless lost?
;			  (format t "~% ~A defined consistently across ~A."
;				  key (mapcar #'car alist))
))
		     (unless (same-sme-definitions? (list (cadr (car entries))
							  (cadr (cadr entries))))
		       (format t "~% ~A defined inconsistently between ~A and ~A."
			       key (caar entries) (caadr entries))
		       (setq lost? t))))))
	   *sme-glossary*))

(defun same-sme-definitions? (def-list)
  (cond ((null def-list) t)
	((null (cdr def-list)) t)
	((equal (car def-list) (cadr def-list))
	 (same-sme-definitions? (cdr def-list)))
	(t nil)))

;;;; Looking for differences between language files

(defun check-language-differences (file1 file2 &optional (stream *standard-output*))
  (let ((1-not-2 nil)
	(2-not-1 nil)
	(common nil)
	(neither nil))
    (maphash #'(lambda (key value)
		 (let ((entry1 (assoc file1 (cdr value) :test #'string=))
		       (entry2 (assoc file2 (cdr value) :test #'string=)))
		   (cond (entry1
			  (cond (entry2 (push key common))
				(t (push key 1-not-2))))
			 (entry2 (push key 2-not-1))
			 (t (push key neither)))))
	     *sme-glossary*)
    (format stream "~% Comparison of language files ~A and ~A.~%" file1 file2)
    (format stream "~%~%In ~A but not in ~A:" file1 file2)
    (format stream "~%          ~{~<~%          ~1:; ~S~>~^,~}.~%" (sort 1-not-2 #'string<))
    (format stream "~%~%In ~A but not in ~A:" file2 file1)
    (format stream "~%          ~{~<~%          ~1:; ~S~>~^,~}.~%" (sort 2-not-1 #'string<))
    (format stream "~%~%In both ~A and ~A:" file1 file2)
    (format stream "~%          ~{~<~%          ~1:; ~S~>~^,~}.~%" (sort common #'string<))
    (format stream "~%~%In neither ~A nor ~A:" file1 file2)
    (format stream "~%          ~{~<~%          ~1:; ~S~>~^,~}.~%" (sort neither #'string<))))
