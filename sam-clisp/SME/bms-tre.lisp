;;; -*- Mode: LISP; Syntax: Common-lisp; Package: SME; Fonts: CPTFONT,TR12I -*-
#-Symbolics
(in-package "SME" :use (list sme::*the-lisp-package*))

;;; Trivial Rule Engine for Belief Maintenence System
;;;
;;;   based on Ken Forbus' TRE-J


(export '(rule sme-assert! rsme-assert! initial-sme-assertion tre-save-rules tre-init restore-rules
          fetch referent unify *tre-rules-saver* *initial-sme-assertions*))

(defstruct (sme-class (:predicate sme-class?)
                  (:print-function (lambda (c st ignore)
				     (format st "#<sme-class ~A>"
					     (sme-class-name c)))))
  name			;the sme-class name (the symbol this sme-class represents)
  sme-assertions		;the sme-assertions in this sme-class
  rules			;the rules which apply to facts in this sme-class
  hash)			;the optional hash table for the sme-class

(defstruct (rule (:print-function (lambda (r st ignore)
				    (format st "#<RULE ~A>"
					    (rule-counter r)))))
  counter		;unique "name"
  sme-class			;the sme-class this rule applies to
  matcher		;the rule's match function
  body)			;the rule's body function

(defstruct (sme-assert (:predicate sme-assertion?)
		   (:print-function (lambda (a st ignore)
				      (format st "#<sme-assert ~A>"
					      (sme-assert-counter a)))))
  counter		;unique "name"
  lisp-form		;external expression
  bms-node		;BMS node for this sme-assertion (or nil)
  sme-class			;sme-class this fact belongs in
  plist)		;stuff


;;;; Global parameters

(defvar *rules-queue* nil)			;rules to be run
(defvar *sme-class* nil)
(defvar *tre-rules* nil)
(defvar *sme-assert* nil)
(defvar *tre-rule-counter* 0)
(defvar *debug* nil) ;show extra information for debugging
(defvar *bound-vars* nil)
(defvar *rule-name-counter* 0)
(defvar *dynamic-rule-file-identifier* "" "compile-time rule file name")
(defvar *sme-assert-counter* 0)

(defvar *default-sme-assert-hash-size* 2029)
(defvar *general-sme-assert-hash*  (make-hash-table :test #'equal :size 4003))

(defvar *tre-rules-saver* nil)
(defvar *initial-sme-assertions* nil)

(defun tre-init ()
  (setq *rules-queue* nil)
  (setq *sme-assert-counter* 0)
  (dolist (sme-class *sme-class*)
    (cond ((sme-class-hash sme-class)
	   (clrhash (sme-class-hash sme-class))
	   (setf (sme-class-sme-assertions sme-class) nil)
	   (setf (sme-class-rules sme-class) nil))
	  ((setf (get (sme-class-name sme-class) :bms-sme-class) nil))))
  (setq *sme-class* (delete-if-not #'sme-class-hash *sme-class*)
	*tre-rules* nil
	*sme-assert* nil)
  (bms-init)
  (restore-rules)				     ;restore the original rules from the rules file
  (dolist (form *initial-sme-assertions*) (eval form)))   ;any sme-assertions from the rules file


;;; Speed hack.  Eliminate the need to reload the rules file each time we run SME
;;;  The variable *tre-rules-saver* is used to store the status of the BMS as it
;;;   appears immediately after loading a new rules file.
;;;     *tre-rules-saver* = (<rule counter integer> <rule name counter integer> . <list of rules being saved>)
;;;
(defun restore-rules ()
  (setq *tre-rules* (mapcar #'(lambda (rule-item)	;the pair (rule-struct . sme-class-name)
			    (let ((sme-class (get-sme-class (cdr rule-item))))
			      (setf (rule-sme-class (car rule-item)) sme-class)
			      (push (car rule-item) (sme-class-rules sme-class))
			      (car rule-item)))
			(cddr *tre-rules-saver*)))
  (setq *tre-rule-counter* (first *tre-rules-saver*))
  (setq *rule-name-counter* (second *tre-rules-saver*)))


(defmacro tre-rules-file (&optional (file-identifier-string ""))
  (setq *dynamic-rule-file-identifier* file-identifier-string)	      ;compile-time information
  `(progn (setq *tre-rules* nil)
	  (setq *rule-name-counter* 0)
	  (setq *tre-rule-counter* 0)
	  (setq *tre-rules-saver* nil)))


(defun tre-save-rules ()
  (setq *tre-rules-saver* (mapcar #'(lambda (rule)
				      (cons rule (sme-class-name (rule-sme-class rule))))
				  *tre-rules*))
  (push *rule-name-counter* *tre-rules-saver*)
  (push *tre-rule-counter* *tre-rules-saver*))


(defmacro initial-sme-assertion (form)
  `(pushnew ',form *initial-sme-assertions* :test #'equal))


;;;; User interface

(defun sme-assert! (fact &optional (belief+ 1.0) (belief- 0.0))
  (let* ((node (bms-sme-assert fact belief+ belief-))
	 (sme-assert (insert fact node)))
    sme-assert))

;;;
(defmacro rsme-assert! (fact &optional (belief+ 1.0) (belief- 0.0))
  `(sme-assert! ,(quotize fact) ,belief+ ,belief-))


;;; Automatic-sme-assert.  sme-asserts the given fact and automatically run the rules
;;;
(defun auto-sme-assert (fact &optional (belief+ 1.0) (belief- 0.0))
  (sme-assert! fact belief+ belief-)
  (run-rules))

;;;
(defun quotize (pattern)
  (cond ((null pattern) nil)
	((variable? pattern) pattern)
	((numberp pattern) pattern)
	((atom pattern) (list 'QUOTE pattern))
	(t `(cons ,(quotize (car pattern)) ,(quotize (cdr pattern))))))


(defmacro enqueue (new queue) `(push ,new ,queue))
(defmacro dequeue (queue) `(pop ,queue))


;;;; Database system

;;; An sme-assertion is just the lisp form itself.  The "sme-class" of an sme-assertion is the leftmost constant
;;;  symbol in the form.  Classes are used to store rules and data.
;;;     - It is assumed that no bms-nodes exist prior to the creation of the sme-class, except perhaps the
;;;       bms node causing the creation of this sme-class.
;;;
(defmacro defsme-Class (name &optional (hashed? nil) (hash-size *default-sme-assert-hash-size*))
  `(let ((sme-class (get-sme-class ',name)))
    (if ,hashed?
	(setf (sme-class-hash sme-class) (make-hash-table :test #'equal :size ,hash-size)))))

(defun get-sme-class (fact)
  (cond ((consp fact)
	 (get-sme-class (car fact)))
	((variable? fact)
	 (if (boundp fact)
	     (get-sme-class (symbol-value fact))
	     (error "~%sme-class unbound: ~A" fact)))
	((symbolp fact)
	 (or (get fact :bms-sme-class)
	     (let ((sme-class (make-sme-class :name fact)))
	       (setf (get fact :bms-sme-class) sme-class)
	       (push sme-class *sme-class*)
	       sme-class)))
	(t (error "Bad sme-class type: ~A" fact))))


(defun fetch (pattern &aux bindings unifiers)
  (dolist (candidate (sme-class-sme-assertions (get-sme-class pattern)) unifiers)
    (setq bindings (unify pattern (sme-assert-lisp-form candidate)))
    (unless (eq bindings 'FAIL)
      (push (sme-assert-lisp-form candidate) unifiers))))


;;; Take datum, return sme-assert structure for it.
;;;
(defun referent (fact)
  (let ((sme-class (get-sme-class fact)))
    (if (sme-class-hash sme-class)
	(gethash fact (sme-class-hash sme-class))
	(dolist (candidate (sme-class-sme-assertions sme-class))
	   (when (equal (sme-assert-lisp-form candidate) fact)
	      (return candidate))))))


;;; Fetch the bms node (if it exists) for the given data form
;;;
(defun referent-bms-node (fact)
  (let ((sme-assert (referent fact)))
    (if sme-assert (sme-assert-bms-node sme-assert))))


;;; Make the given data form and its bms node known to the sme-assert data base.
;;;
(defun insert (fact node &aux sme-assert)
  (setq sme-assert (referent fact))
  (cond (sme-assert (values sme-assert t))
	(t (setq sme-assert (make-sme-assert :counter (incf *sme-assert-counter*)
				     :Lisp-form fact
				     :sme-class (get-sme-class fact)
				     :bms-node node))
	   (push sme-assert (sme-class-sme-assertions (sme-assert-sme-class sme-assert)))
	   (if (sme-class-hash (sme-assert-sme-class sme-assert))
	       (setf (gethash fact (sme-class-hash (sme-assert-sme-class sme-assert))) sme-assert))
	   (push sme-assert *sme-assert*)
           (try-rules sme-assert) 
	   (values sme-assert nil))))


(defun show-data (&optional (stream t) &aux counter)
  (setq counter 0)
  (dolist (sme-class *sme-class* counter)
    (dolist (datum (sme-class-sme-assertions sme-class))
      (incf counter)
      (format stream "~%~A" (sme-assert-lisp-form datum)))))


;;;; Variables and unification

(defun variable? (x)
  (and (symbolp x)			;A variable is a symbol whose first character is "?"
       (string-equal "?" (symbol-name x) :end2 1)))

(defun unify (a b) (unify1 a b nil))

(defun unify1 (a b bindings)
  (cond ((equal a b) bindings)
	((variable? a) (unify-variable a b bindings))
	((variable? b) (unify-variable b a bindings))
	((or (atom a) (atom b))  'FAIL)
	((not (eq 'FAIL (setq bindings
			      (unify1 (car a) (car b) bindings))))
	 (unify1 (cdr a) (cdr b) bindings))
	(t 'FAIL)))

(defun unify-variable (var exp bindings &aux binding)
  (setq binding (assoc var bindings))
  (cond (binding (unify1 (cdr binding) exp bindings))
	((free-in? var exp bindings) (cons (cons var exp) bindings))
	(t 'FAIL)))

(defun free-in? (var exp bindings)
  (cond ((null exp) t)
	((equal var exp) nil)
	((variable? exp) (free-in? var (cdr (assoc exp bindings)) bindings))
	((not (listp exp)) t)
	((free-in? var (car exp) bindings)
	 (free-in? var (cdr exp) bindings))))


;;;; Building and installing rules (lexical closure version)

;;;
(defmacro rule (triggers &rest body)
  (setq *bound-vars* nil)
  (multiple-value-bind (match-function body-function insert-form)
        (build-rule (car triggers)
		    (parse-nesting (cdr triggers) (subst 'internal-rule 'rule body)))
    `(progn ,match-function
	    ,body-function
	    ,insert-form)))


;;; non-nested version of rule:
;;;     (rule (:intern (foo ?x))
;;;         (do-something))
;;;
(defmacro rule1 (trigger &rest body)
  (build-rule trigger body t))


(defun parse-nesting (triggers body)
  (if triggers
      (list `(rule1 ,(car triggers) ,@ (parse-nesting (cdr triggers) body)))
      body))


(defmacro internal-rule (triggers &rest body)
  (car (parse-nesting triggers body)))



;;;; Building a rule

;;; Building a rule involves analyzing the trigger to see what
;;;  special-purpose unifier is needed, constructing a function
;;;  to do the work of the body, and building a form to index it.

(defun build-rule (trigger body &optional (internal? nil)
			   &aux match-function body-function)
  (multiple-value-bind (pattern condition var test)
      (parse-rule-trigger trigger)
    (if (and (member condition '(:BELIEF+ :BELIEF-)) (eq (car pattern) 'implies))
		(error "Cannot use :BELIEF triggers with implies forms: ~A" trigger))
    (setq match-function (generate-match-function pattern condition var test))
    (setq body-function (generate-body-function pattern condition var trigger body))
    (cond (internal?
	   `(insert-rule (get-sme-class ,(get-trigger-sme-class pattern))
			 (setf (symbol-function ',(second match-function))
			       (function (lambda (p) ,@ (cdddr match-function))))
			 (setf (symbol-function ',(second body-function))
			       (function (lambda ,(third body-function)
					   ,@ (cdddr body-function))))))
	  (t (values match-function
		     body-function
		     `(insert-rule (get-sme-class ,(get-trigger-sme-class pattern))
				   (function ,(second match-function))
				   (function ,(second body-function))))))))


;;; Trigger syntax is now (<condition> <pattern>), with optional :VAR and :TEST keywords after <pattern>
;;;
(defun parse-rule-trigger (trigger)
  (values (second trigger)
	  (cond ((member (car trigger) '(:INTERN :BELIEF+ :BELIEF-))  (car trigger))
		((error "~%~A bad condition in ~A." (car trigger) trigger)))
	  (second (member ':VAR (cddr trigger)))
	  (second (member ':TEST (cddr trigger)))))

;;;
(defun get-trigger-sme-class (trigger)
  (cond ((variable? trigger)
	 (if (member trigger *bound-vars*) trigger
	     (error "~%Trigger sme-class is unbound -- ~A." trigger)))
	((atom trigger) (list 'QUOTE trigger))
	(t (get-trigger-sme-class (car trigger)))))


;;;; Generating the body function

;;;
(defun generate-body-function (pattern condition var trigger body)
  (let* ((newly-bound 
  			(if var
			  (cons var (pattern-free-variables pattern))
			  (pattern-free-variables pattern)))
	 	 (fname (generate-rule-function-name pattern))
	 	 (*bound-vars* (union *bound-vars* newly-bound))
	 	 (body (fully-expand-body body)))
    (unless (eq condition ':INTERN) (push '*THE-NODE* newly-bound))
    `(defun ,fname ,newly-bound
       ,@ (case condition
	    (:INTERN  body)			                ;just execute the body
	    (:BELIEF+ (let ((threshold (third trigger)))	;execute the body when applicable
			(if (and (numberp threshold) (>= threshold 0.0) (<= threshold 1.0))
			    `((cond ((> (node-belief+ *THE-NODE*) ,threshold)
				     ,@ body)
				    ((push (list ',fname ,@ newly-bound)
					   (node-positive-belief-rules *THE-NODE*)))))
			    (error "~% Incorrect :BELIEF+ threshold given in pattern ~A"
				   trigger))))
	    (:BELIEF- (let ((threshold (third trigger)))	;execute the body when applicable
			(if (and (numberp threshold) (>= threshold 0.0) (<= threshold 1.0))
			    `((cond ((> (node-belief- *THE-NODE*) ,threshold)
				     ,@ body)
				    ((push (list ',fname ,@ newly-bound)
					   (node-negative-belief-rules *THE-NODE*)))))
			    (error "~% Incorrect :BELIEF- threshold given in pattern ~A"
				   trigger))))))))


(defun generate-rule-function-name (pattern)
  (intern (format nil "~A-~A-~A" pattern
		                 (incf *rule-name-counter*)
				 *dynamic-rule-file-identifier*)))


;;;;  Open-coding unification

;;;
(defun generate-match-function (pattern condition var test)
  (let ((free-variables (pattern-free-variables pattern)))
    (multiple-value-bind (tests binding-specs)
	(generate-match-body pattern free-variables test)
      `(defun ,(generate-rule-function-name pattern) (p)
	 (if (and ,@ tests)
	     (values T                                                  ;match?
		     (list ,@ (if var '(p)) ,@ (reverse binding-specs))	;bindings
		     ,(unless (eq condition ':INTERN) t)                ;save node?
		     ,(null free-variables)))))))                       ;dead rule?

;;;
(defun generate-match-body (pattern vars extra-test
				    &aux structure-tests equal-tests binding-specs var-alist)
  (dolist (test (generate-unify-tests pattern vars nil 'P)
		(values (nconc structure-tests equal-tests
			       (if extra-test  (list (sublis var-alist extra-test))))
			binding-specs))
    (cond ((variable? (car test))		    ;test looks like (?x (nth p) (nth p) ...)
	   (setq equal-tests (nconc (generate-pairwise-tests (cdr test))
 				    equal-tests))
	   (push (car (last test)) binding-specs) ; due to "push", last is the first instance of var
	   (push (cons (car test) (car binding-specs))  var-alist))
	  (t (push test structure-tests)))))


(defun generate-pairwise-tests (tests)
  (cond ((or (null tests) (null (cdr tests)))  nil)
	(t (cons `(equal ,(first tests) ,(second tests))
		 (generate-pairwise-tests (cdr tests))))))


;;;
(defun generate-unify-tests (pattern vars tests path)
  (cond ((null pattern) (cons `(null ,path) tests))	;this is the end
	((member pattern vars)	                  
         ; must see if the pattern has been bound elsewhere, and if it has,
         ; test to see if the element here is consistent with that earlier binding.
	 (let ((previous (assoc pattern tests)))
	   (cond (previous			;add this position to test it
		  (push path (cdr previous))
		  tests)
		 (t (cons (list pattern path) tests)))))
	 ; if variable, it must be bound so test against the current value.
	((variable? pattern) (cons `(equal ,pattern ,path) tests))
         ; if not a list, then see if equal
	((numberp pattern) (cons `(and (numberp ,path) (= ,pattern ,path)) tests))
	((atom pattern) (cons `(equal ',pattern ,path) tests))
         ; recurse on a list
	(t (generate-unify-tests (cdr pattern) vars
				 (generate-unify-tests (car pattern) vars
						    (cons `(consp ,path) tests) ;avoid lisp errors
						    (list 'car path))	        ;extend the path
				 (list 'cdr path)))))	   ;extend path in other direction


(defun pattern-free-variables (pattern) (pattern-free-vars1 pattern nil))

(defun pattern-free-vars1 (pattern vars)
  (cond ((null pattern) vars)
	((variable? pattern)
	 (if (or (member pattern vars) (member pattern *bound-vars*))
	     vars
	     (cons pattern vars)))
	((atom pattern) vars)
	(t (pattern-free-vars1 (cdr pattern)
			       (pattern-free-vars1 (car pattern) vars)))))


;;;; Do-it-yourself macro expansion

;;; Apparently the compiler can't be relied on to do depth-first macro-expansion
;;;  so *bound-vars* gets skrewed if we don't force depth-first expansion
;;;
(defun fully-expand-body (body)
  (cond 
  	((atom body) body)
	((symbolp (car body))
	 (case (car body)
	   (QUOTE body)
	   (SETF body)	;; Added by Santiago Ontanon to make SME work on CLISP (which expands SETF in a way that this function doesn't like)
	   (LAMBDA `(lambda ,(cadr body)
		      ,@ (fully-expand-body (cddr body))))
	   (DO `(do ,(cadr body) ,(caddr body)
		  ,@ (cdddr body)))
	   ((DOTIMES DOLIST LET PROG PROGN PROGV FLET LABELS)
	    `(,(car body) ,(cadr body)
	      ,@ (fully-expand-body (cddr body))))
	   (T (let ((new-body (macroexpand body)))
		(cons (fully-expand-body (car new-body))
		      (fully-expand-body (cdr new-body)))))))
	(T (let ((new-body (macroexpand body)))
	     (cons (fully-expand-body (car new-body))
		   	   (fully-expand-body (cdr new-body)))))))

;;;; Running rules

(defun insert-rule (sme-class matcher body &aux rule)
  (setq rule (make-rule :matcher matcher
			:body body
			:sme-class sme-class
			:counter (incf *tre-rule-counter*)))
  (push rule *tre-rules*)
  (push rule (sme-class-rules sme-class))		;indexing
  (dolist (candidate (sme-class-sme-assertions sme-class))
    (try-rule-on rule candidate)))


(defun try-rules (fact)
  (dolist (rule (sme-class-rules (sme-assert-sme-class fact)))
    (try-rule-on rule fact)))

(defun try-rule-on (rule sme-assert)
  (multiple-value-bind (okay? bindings node? dead?)
      (funcall (rule-matcher rule) (sme-assert-lisp-form sme-assert))
    (when okay?
      (when node? 
	(setq bindings (cons (sme-assert-bms-node sme-assert) bindings)))
      (enqueue (cons (rule-body rule) bindings) *rules-queue*)
      (when dead?
	(setf (sme-class-rules (rule-sme-class rule))  (delete rule (sme-class-rules (rule-sme-class rule))))
	(setf *tre-rules* (delete rule *tre-rules*))))))

(defun run-rules ()
  (do ((form (dequeue *rules-queue*) (dequeue *rules-queue*))
       (counter 0 (1+ counter)))
      ((null form))
    (apply (car form) (cdr form))))



;;;; Query interface to the BMS

(defun true? (fact)
  (let ((r (referent fact)))
    (if r
	(if (sme-assert-bms-node r)
	    (true-node? (sme-assert-bms-node r))	;if it has a node, check node
	    t))))				;else true if referent found

(defun false? (fact)
  (let ((r (referent fact)))
    (cond ((null r) t)
	  ((sme-assert-bms-node r)
	   (false-node? (sme-assert-bms-node r))))))        ;if it has a node, check node, else not false

(defun unknown? (fact)
  (let ((r (referent fact)))
    (if (and r (sme-assert-bms-node r))
	(unknown-node? (sme-assert-bms-node r)))))


(defun support-for (fact)
  (let ((r (referent fact)))
    (cond ((null r) 0.0)
	  ((sme-assert-bms-node r)
	   (support-for-node (sme-assert-bms-node r)))
	  (t 1.0))))

(defun support-against (fact)
  (let ((r (referent fact)))
    (cond ((null r) 1.0)
	  ((sme-assert-bms-node r)
	   (support-against-node (sme-assert-bms-node r)))
	  (t 0.0))))

(defun possible-true (fact)
  (let ((r (referent fact)))
    (cond ((null r) 0.0)
	  ((sme-assert-bms-node r)
	   (possible-true-node (sme-assert-bms-node r)))
	  (t 1.0))))

(defun possible-false (fact)
  (let ((r (referent fact)))
    (cond ((null r) 1.0)
	  ((sme-assert-bms-node r)
	   (possible-false-node (sme-assert-bms-node r)))
	  (t 0.0))))

(defun belief-uncertainty (fact)
  (let ((r (referent fact)))
    (if (and r (sme-assert-bms-node r))
	(node-belief-uncertainty (sme-assert-bms-node r))
	0.0)))

(defun absolutely-unknown? (fact)
  (let ((r (referent fact)))
    (if (and r (sme-assert-bms-node r))
	(absolutely-unknown-node? (sme-assert-bms-node r)))))

(defun absolutely-true? (fact)
  (let ((r (referent fact)))
    (if r
	(if (sme-assert-bms-node r)
	    (absolutely-true-node? (sme-assert-bms-node r))	   ;if it has a node, check node
	    t))))				          ;else true if referent found

(defun absolutely-false? (fact)
  (let ((r (referent fact)))
    (cond ((null r) t)
	  ((sme-assert-bms-node r)
	   (absolutely-false-node? (sme-assert-bms-node r)))))) ;if has a node, check node, else not false

;;; Other goodies
;;;
(defun premises (fact)
  (let ((r (referent fact)))
    (if (and r (sme-assert-bms-node r))
	(premises-node (sme-assert-bms-node r))
	(format t "~% ~A, from ~A" (sme-assert-lisp-form r) 'USER))))

(defun why (fact &optional (stream *standard-output*))
  (let ((r (referent fact)))
    (if (and r (sme-assert-bms-node r))
	(why-node (sme-assert-bms-node r) stream))))


;;; remove its sme-assert thing too
;;;
(defun forget (fact)
  (let ((r (referent fact)))
    (when (and r (sme-assert-bms-node r))
      (forget-premise (sme-assert-bms-node r)))))


;;; To make a contradiction, sme-assert the negation of the fact
;;;
;;(defun contradiction (fact)
;;  (sme-assert! (list 'not fact)))
