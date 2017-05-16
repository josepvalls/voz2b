;; Riu

; (change-directory (get-working-directory))
; (change-directory "/Users/santi/Dropbox/Riu/Riu-clisp")
(defvar *sme-loaded* nil)
(defvar *riu-debug* '())
(setf *riu-debug* '())
;; (setf *riu-debug* '(analogy discourse retrieval prediction intentionality))
(setf *riu-debug* '())

(unless *sme-loaded* (load "sme-load.lisp"))

;; (load "riu-memory.lisp")
(load "stories/riu-memory-Hunter-complete.lisp")

;; (load "riu-story.lisp")
;; (load "Hunter/story-theft.lsp")
;; (load "Hunter/story-factoryjob.lsp")
;; (load "Hunter/story-aristoreward.lsp")
(load "stories/riu-story-Hunter-complete.lisp")



(load "riu-utils.lisp")
(load "riu-retrieval.lisp")
(load "riu-analogy.lisp")
(load "riu-discourse.lisp")
(load "riu-scene-instantiation.lisp")
(load "riu-bdi.lisp")

(defun RIU ()
	;; For now this is a test:
	(let* ((story (list *riu-STORY-START* *riu-STORY-DAG*))
		   (memory *riu-MEMORY*)
		   (ales (init-ales-BDI)))
		(format t "RIU - Version 1.0~%")
		(format t "Jichen Zhu and Santiago Ontanon~%")
		(format t "Story by: Jichen Zhu, Santiago Ontanon and J. Hunter Sizemore~%~%")
		
;		(format t "Initializing ALES BDI model...~%");
;		(print-BDI ales)

;		(format t "Precomputing the retrieval keywords...~%")
		(setf (second story) (compute-keywords (second story)))
		(setf memory (compute-keywords memory))

		(format t "~%");
		(run-story story memory ales)
	)
)
