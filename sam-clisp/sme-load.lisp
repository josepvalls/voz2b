;;; -*- Mode: LISP; Syntax: Common-lisp; Package: SME; Base: 10; -*-

(defpackage "SME" (:use "COMMON-LISP")) 
(in-package "SME")

;;;; Structure-Mapping Engine  (Site configuration declarations)




(defvar *the-user-package* "COMMON-LISP-USER")		;this machine's user package
(defvar *the-lisp-package* "COMMON-LISP")	;this machine's lisp package
     ;IMPORTANT - modify in-package at top of this file
     ; :use there must match the lisp one here.


(setq *sme-dgroup-pathname* "sme/")
(setq *sme-rules-pathname* "")
(setq *sme-language-file* "sme-language")
(setq *sme-default-rules* "riu-sme-match.rules")
(setq *sme-system-pathname* "sme/")

(defvar *sme-files* '("defs" "bits" "bms" "bms-tre" "sme"
                      "match" "match-rules-support" "display"
                      "generalize" "batch"   ;optional utilities
;;                      "cmenu" ;; KDF: character-oriented menu system
;;                      "charsme" ;; KDF: Some useful utilities.
                      ;; Call (sme::toplevel) to operate.
                      ;;; Any volunteers to write a MAC-based windowing system?
;; DO NOT LOAD THIS BY DEFAULT!  It assumes the menus utilties
;;    and ZGRAPH! -- KDF 
;;;		      "windowing"  ;optional windowing system
		      ))



(defun load-sme (&optional (sources? nil))
  (dolist (file *sme-files*)
          (load (format nil "~A~A.~A"
			*sme-system-pathname* file
			(if sources? "lisp" "rbin"))))
  ;; WARNING!  DO NOT AUTOMATICALLY LOAD GRAPHICS!
    )


(defun compile-sme ()
  (dolist (file *sme-files*)
    (compile-file (format nil "~A~A" *sme-system-pathname* file))
    (load (format nil "~A~A" *sme-system-pathname* file))))

;;; Symbol hacking
;;;   We want these symbols accessible in SME package, yet also accessible
;;;   as the same symbol in user package, since things like dgroup and rule
;;;   definitions are written in the user package.
;;;   This is here in the first file since having them in the same file as
;;;   their use seems to be a loss when using shadowing import, no idea why...
;;;
(eval-when (compile load eval)	;want to bring in user's symbols, rather than ship ours out.
  (dolist (sym-name '("ENTITIES" "EXPRESSIONS"	;for defDescription (defs)
		      "MH" "GMAP" "CI"		;for rule triggering (defs)
		      "AND" "OR" "IMPLIES"))	;for rule triggering (bms)
    (format t "~%Shadowing ~A" sym-name)
    (shadowing-import (intern sym-name (find-package *the-user-package*))
		      (find-package "SME"))))


(load-sme t)
(sme-init)
(in-package "COMMON-LISP-USER")
(setf *sme-loaded* t)
