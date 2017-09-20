;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: mf; -*-

;;;; MAC/FAC utilities

;; Various things for setting up dgroups, rule sets, etc.
;; Assumes charsme package.

(in-package 'mf)

(defvar *dgroup-directories* #+KDF-RT '("/u/dgroups/basic/" "/u/dgroups/islike/")
  #+QRG-RT '("/usr/analogy/dgroups/basic/" "/usr/analogy/dgroups/islike/")
  #+UIUC '("rube:>dgroups>basic>" "rube:>dgroups>islike>")
  ) ;; Motivation: there will presumably be LOTS of data...

(defun load-dgroup-directory (path)
  (dolist (file (directory (concatenate 'string path
					"*." sme::*dgroup-extension-name*)))
    (load file)))

(defun load-rule-directory (path)
  (dolist (file (directory (concatenate 'string path
					"*." sme::*rule-extension-name*)))
    (sme::load-rule-set (pathname-name file) file)))

(defun load-matchers-directory (path)
  (dolist (file (directory (concatenate 'string path
					"*." *matchers-extension*)))
    (load file)))

(defun load-selectors-directory (path)
  (dolist (file (directory (concatenate 'string path
					"*." *selectors-extension*)))
    (load file)))

(defvar *rules-pathname* #+KDF-RT "/u/macfac/rules/"
  #+QRG-RT "/usr/analogy/macfac/rules/"
  #+UIUC "rube:>macfac>rules>"
  )

(defun load-default-data ()
  (dolist (dgroup-dir *dgroup-directories*)
    (load-dgroup-directory dgroup-dir))
  (load-rule-directory *rules-pathname*)
  (load-matchers-directory *matchers-pathname*)
  (load-selectors-directory *selectors-pathname*))

(defun gm-score (x) (sme::node-belief+ (sme::gm-bms-node x)))
