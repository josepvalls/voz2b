
(proclaim '(optimize (debug 1)))

;; Riu

(defvar *sme-loaded* nil)
(defvar *riu-debug* '())
(setf *riu-debug* '())
;; (setf *riu-debug* '(analogy discourse retrieval prediction intentionality))
;; (setf *riu-debug* '(prediction))

(unless *sme-loaded* (load "sme-load.lisp"))

(load "riu-utils.lisp")
(load "riu-retrieval.lisp")
(load "riu-analogy.lisp")
(load "riu-discourse.lisp")
(load "riu-scene-instantiation.lisp")
(load "riu-bdi.lisp")

(defun generate-story ()
	
	(load "voz/story1-complete-levinverb_nofunc_roleexp_prep_sty.lisp")
	(setf complete-story1 *story*)
    
	(setf complete-stories (list complete-story1))

	(load "voz/story3-partial-levinverb_nofunc_roleexp_prep_sty.lisp")
	(setf partial-story *story*)

	(setf retrieved-stories (retrieve-K-memories partial-story complete-stories 3 nil))
	(format t "~a~%" (length retrieved-stories))

	(setf *riu-debug* '(show-final-mapping))
	(dolist (source retrieved-stories)
		(generate-analogical-text-general partial-story source)
	)

)

(generate-story)
    