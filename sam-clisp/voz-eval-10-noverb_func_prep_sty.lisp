
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
	
	(load "voz/story1-complete-noverb_func_prep_sty.lisp")
	(setf complete-story1 *story*)
    
	(load "voz/story2-complete-noverb_func_prep_sty.lisp")
	(setf complete-story2 *story*)
    
	(load "voz/story3-complete-noverb_func_prep_sty.lisp")
	(setf complete-story3 *story*)
    
	(load "voz/story4-complete-noverb_func_prep_sty.lisp")
	(setf complete-story4 *story*)
    
	(load "voz/story5-complete-noverb_func_prep_sty.lisp")
	(setf complete-story5 *story*)
    
	(load "voz/story6-complete-noverb_func_prep_sty.lisp")
	(setf complete-story6 *story*)
    
	(load "voz/story7-complete-noverb_func_prep_sty.lisp")
	(setf complete-story7 *story*)
    
	(load "voz/story8-complete-noverb_func_prep_sty.lisp")
	(setf complete-story8 *story*)
    
	(load "voz/story9-complete-noverb_func_prep_sty.lisp")
	(setf complete-story9 *story*)
    
	(load "voz/story11-complete-noverb_func_prep_sty.lisp")
	(setf complete-story11 *story*)
    
	(load "voz/story12-complete-noverb_func_prep_sty.lisp")
	(setf complete-story12 *story*)
    
	(load "voz/story13-complete-noverb_func_prep_sty.lisp")
	(setf complete-story13 *story*)
    
	(load "voz/story14-complete-noverb_func_prep_sty.lisp")
	(setf complete-story14 *story*)
    
	(load "voz/story15-complete-noverb_func_prep_sty.lisp")
	(setf complete-story15 *story*)
    
	(load "voz/story16-complete-noverb_func_prep_sty.lisp")
	(setf complete-story16 *story*)
    
	(load "voz/story17-complete-noverb_func_prep_sty.lisp")
	(setf complete-story17 *story*)
    
	(load "voz/story18-complete-noverb_func_prep_sty.lisp")
	(setf complete-story18 *story*)
    
	(load "voz/story19-complete-noverb_func_prep_sty.lisp")
	(setf complete-story19 *story*)
    
	(load "voz/story20-complete-noverb_func_prep_sty.lisp")
	(setf complete-story20 *story*)
    
	(setf complete-stories (list complete-story1 complete-story2 complete-story3 complete-story4 complete-story5 complete-story6 complete-story7 complete-story8 complete-story9 complete-story11 complete-story12 complete-story13 complete-story14 complete-story15 complete-story16 complete-story17 complete-story18 complete-story19 complete-story20))

	(load "voz/story10-partial-noverb_func_prep_sty.lisp")
	(setf partial-story *story*)

	(setf retrieved-stories (retrieve-K-memories partial-story complete-stories 3 nil))
	(format t "~a~%" (length retrieved-stories))

	(setf *riu-debug* '(show-final-mapping))
	(dolist (source retrieved-stories)
		(generate-analogical-text-general partial-story source)
	)

)

(generate-story)
    