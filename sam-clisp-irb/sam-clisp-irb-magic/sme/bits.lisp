;;; -*- Mode: LISP; Syntax: Common-lisp; Package: SME; Base: 10; Fonts: CPTFONT,TR12I; -*-
#-Symbolics
(in-package "SME" :use (list sme::*the-lisp-package*))

;;; Structure-Mapping Engine  (bit-vector handler)

;;;    Adapted from deKleer's ATMS


(defconstant *max-bit* 30 "largest bit which can be set without going bignum")
(defconstant *max-num* (ash 1 *max-bit*) "largest single-bit number on this machine")


;;; Given a bit vector counter (one which represents the next bit vector to be given out),
;;;    return that bit vector and update the counter.
;;;
(defmacro next-identifier (identifier-counter)
  `(prog1 (append (cdr ,identifier-counter) (list (car ,identifier-counter)))
	  (cond ((= (car ,identifier-counter) *max-num*)
		 (push 0 (cdr ,identifier-counter))
		 (setf (car ,identifier-counter) 1))
		((setf (car ,identifier-counter) (ash (car ,identifier-counter) 1))))))


;;; Test two bit vectors for equality
(defun vector-equal (l1 l2)
  (do ((l1 l1 (cdr l1))
       (l2 l2 (cdr l2)))
      ((eq l1 l2) T)
    (unless (eq (car l1) (car l2)) (return nil))))


;;; Return the union of the two bit vectors
(defun vector-union (v1 v2 &aux result)
  (do ((v1 v1 (cdr v1))
       (v2 v2 (cdr v2)))
      (nil)
    (if v1
	(if v2
	    (push (logior (car v1) (car v2)) result)
	    (return (nreconc result v1)))
	(return (nreconc result v2)))))


;;; a destructive version of vector-union.  Stores union in first argument (vector)
;;;
(defun vunionf (vector new-vector)
  (do ((v1 vector (cdr v1))
       (v2 new-vector (cdr v2)))
      (nil)
    (if v1
	(if v2
	    (setf (car v1) (logior (car v1) (car v2)))
  	    (return vector))
	(return (nconc vector (fcopy v2))))))


;;; Returns T if the two vectors have a non-empty intersection.
(defun vector-intersection? (v1 v2)
  (do ((v1 v1 (cdr v1))
       (v2 v2 (cdr v2)))
      ((or (null v1) (null v2)) nil)
    (unless (zerop (logand (car v1) (car v2))) (return T))))


(defun vector-intersection (v1 v2)
  (mapcar #'logand v1 v2))



;;;  Is the set described by vector v1 a subset of v2?
;;;    This uses definition of subset from set notation rather than deKleer's notion of subsumption
;;;
(defun vector-subset? (v1 v2)
  (do ((v1 v1 (cdr v1))
       (v2 v2 (cdr v2)))
      ((null v1) T)
    (if (or (null v2) (not (zerop (logandc2 (car v1) (car v2))))) (return nil))))


(defun vector-difference (v1 v2 &aux result)
  (do ((v1 v1 (cdr v1))
       (v2 v2 (cdr v2)))
      (nil)
    (if v1
	(if v2
	    (push (logandc2 (car v1) (car v2)) result)
	    (return (nreconc result v1)))
	(return (nreverse result)))))


(defun vector-complement (v)
  (mapcar #'lognot v))


(defun vector-memq (offset bits v &aux word)
  (if (setq word (nthcdr offset v)) (/= 0 (logand bits (car word)))))


(defun bit-number (v &aux (num 0))
  (dolist (bits v)
    (cond ((zerop bits)
	   (incf num *max-bit*))
	  (t (incf num (integer-length bits))
	     (return num)))))
