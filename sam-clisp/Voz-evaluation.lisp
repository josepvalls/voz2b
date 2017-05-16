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
	;; Load the evening tide story:
	(load "story1-complete.lisp")
	(setf complete-story1 *story*)
	(load "stories/EveningTide/mem02")
	(setf complete-story2 *story*)
	(load "stories/EveningTide/mem03")
	(setf complete-story3 *story*)
	(load "stories/EveningTide/mem05")
	(setf complete-story4 *story*)
	(load "stories/EveningTide/mem06")
	(setf complete-story5 *story*)
	(setf complete-stories (list complete-story1 complete-story2 complete-story3 complete-story4 complete-story5))

	(load "story2-complete.lisp")
	(setf partial-story *story*)


	(setf retrieved-stories (retrieve-K-memories partial-story complete-stories 3 nil))
	(format t "~a~%" (length retrieved-stories))

	(setf *riu-debug* '(show-final-mapping))
	(dolist (source retrieved-stories)
		(generate-analogical-text-general partial-story source)
	)

)

(generate-story)