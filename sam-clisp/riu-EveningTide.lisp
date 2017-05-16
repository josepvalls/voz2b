;; Riu

; (change-directory (get-working-directory))
(change-directory "/Users/santi/Dropbox/Riu/Riu")
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

(defun RIU ()
	;; Load the evening tide story:
	(setf *riu-STORY-DAG* nil)
	(setf *riu-STORY-START* 'STORY-01)
	(setf *riu-GOALS* nil)
	(setf *riu-MEMORY* nil)
	(setf *riu-GOALS*
		'((happy 
			(happy fun play cute)
			(sad dead bored work force rusty awkward))
		  (sad 
			(sad dead bored work force rusty awkward)
			(happy fun play cute))
		)
	)
	(load "stories/EveningTide/S01")
	(setf *riu-STORY-DAG* (cons *story* *riu-STORY-DAG*))
	(load "stories/EveningTide/S04")
	(setf *riu-STORY-DAG* (cons *story* *riu-STORY-DAG*))
	(load "stories/EveningTide/S16")
	(setf *riu-STORY-DAG* (cons *story* *riu-STORY-DAG*))

	(load "stories/EveningTide/mem01_V2")
	(setf *riu-MEMORY* (cons *story* *riu-MEMORY*))
#|
	(load "stories/EveningTide/mem02")
	(setf *riu-MEMORY* (cons *story* *riu-MEMORY*))
	(load "stories/EveningTide/mem03")
	(setf *riu-MEMORY* (cons *story* *riu-MEMORY*))
	(load "stories/EveningTide/mem05")
	(setf *riu-MEMORY* (cons *story* *riu-MEMORY*))
	(load "stories/EveningTide/mem06")
	(setf *riu-MEMORY* (cons *story* *riu-MEMORY*))
	(load "stories/EveningTide/mem09")
	(setf *riu-MEMORY* (cons *story* *riu-MEMORY*))
	(load "stories/EveningTide/mem10")
	(setf *riu-MEMORY* (cons *story* *riu-MEMORY*))
	(load "stories/EveningTide/mem12")
	(setf *riu-MEMORY* (cons *story* *riu-MEMORY*))
	(load "stories/EveningTide/mem18")
	(setf *riu-MEMORY* (cons *story* *riu-MEMORY*))
	(load "stories/EveningTide/mem19")
	(setf *riu-MEMORY* (cons *story* *riu-MEMORY*))
	(load "stories/EveningTide/mem20")
	(setf *riu-MEMORY* (cons *story* *riu-MEMORY*))
|#

	(let* ((story (list *riu-STORY-START* *riu-STORY-DAG*))
		   (memory *riu-MEMORY*)
		   (julian (init-BDI "Julian" () (list 'happy) 1.00)))
		(format t "Evening Tide~%")
		(format t "RIU - Version 1.0~%")
		(format t "Jichen Zhu and Santiago Ontanon~%")
		(format t "~%")
		
;		(format t "Initializing ALES BDI model...~%");
;		(print-BDI ales)

;		(format t "Precomputing the retrieval keywords...~%")
		(setf (second story) (compute-keywords (second story)))
		(setf memory (compute-keywords memory))

		(format t "~%");
		(run-story story memory julian)
	)
)
