
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
	
	(load "voz/story2-complete-levinverb_nofunc_roleexp_prep_voz.lisp")
	(setf complete-story2 *story*)
    
	(load "voz/story3-complete-levinverb_nofunc_roleexp_prep_voz.lisp")
	(setf complete-story3 *story*)
    
	(load "voz/story4-complete-levinverb_nofunc_roleexp_prep_voz.lisp")
	(setf complete-story4 *story*)
    
	(load "voz/story5-complete-levinverb_nofunc_roleexp_prep_voz.lisp")
	(setf complete-story5 *story*)
    
	(load "voz/story6-complete-levinverb_nofunc_roleexp_prep_voz.lisp")
	(setf complete-story6 *story*)
    
	(load "voz/story7-complete-levinverb_nofunc_roleexp_prep_voz.lisp")
	(setf complete-story7 *story*)
    
	(load "voz/story8-complete-levinverb_nofunc_roleexp_prep_voz.lisp")
	(setf complete-story8 *story*)
    
	(load "voz/story9-complete-levinverb_nofunc_roleexp_prep_voz.lisp")
	(setf complete-story9 *story*)
    
	(load "voz/story10-complete-levinverb_nofunc_roleexp_prep_voz.lisp")
	(setf complete-story10 *story*)
    
	(load "voz/story11-complete-levinverb_nofunc_roleexp_prep_voz.lisp")
	(setf complete-story11 *story*)
    
	(load "voz/story12-complete-levinverb_nofunc_roleexp_prep_voz.lisp")
	(setf complete-story12 *story*)
    
	(load "voz/story13-complete-levinverb_nofunc_roleexp_prep_voz.lisp")
	(setf complete-story13 *story*)
    
	(load "voz/story14-complete-levinverb_nofunc_roleexp_prep_voz.lisp")
	(setf complete-story14 *story*)
    
	(load "voz/story15-complete-levinverb_nofunc_roleexp_prep_voz.lisp")
	(setf complete-story15 *story*)
    
	(load "voz/story16-complete-levinverb_nofunc_roleexp_prep_voz.lisp")
	(setf complete-story16 *story*)
    
	(load "voz/story17-complete-levinverb_nofunc_roleexp_prep_voz.lisp")
	(setf complete-story17 *story*)
    
	(load "voz/story18-complete-levinverb_nofunc_roleexp_prep_voz.lisp")
	(setf complete-story18 *story*)
    
	(load "voz/story19-complete-levinverb_nofunc_roleexp_prep_voz.lisp")
	(setf complete-story19 *story*)
    
	(load "voz/story20-complete-levinverb_nofunc_roleexp_prep_voz.lisp")
	(setf complete-story20 *story*)
    
	(load "voz/story21-complete-levinverb_nofunc_roleexp_prep_voz.lisp")
	(setf complete-story21 *story*)
    
	(load "voz/story22-complete-levinverb_nofunc_roleexp_prep_voz.lisp")
	(setf complete-story22 *story*)
    
	(load "voz/story23-complete-levinverb_nofunc_roleexp_prep_voz.lisp")
	(setf complete-story23 *story*)
    
	(load "voz/story24-complete-levinverb_nofunc_roleexp_prep_voz.lisp")
	(setf complete-story24 *story*)
    
	(load "voz/story25-complete-levinverb_nofunc_roleexp_prep_voz.lisp")
	(setf complete-story25 *story*)
    
	(load "voz/story26-complete-levinverb_nofunc_roleexp_prep_voz.lisp")
	(setf complete-story26 *story*)
    
	(load "voz/story27-complete-levinverb_nofunc_roleexp_prep_voz.lisp")
	(setf complete-story27 *story*)
    
	(load "voz/story28-complete-levinverb_nofunc_roleexp_prep_voz.lisp")
	(setf complete-story28 *story*)
    
	(load "voz/story29-complete-levinverb_nofunc_roleexp_prep_voz.lisp")
	(setf complete-story29 *story*)
    
	(load "voz/story30-complete-levinverb_nofunc_roleexp_prep_voz.lisp")
	(setf complete-story30 *story*)
    
	(load "voz/story31-complete-levinverb_nofunc_roleexp_prep_voz.lisp")
	(setf complete-story31 *story*)
    
	(load "voz/story32-complete-levinverb_nofunc_roleexp_prep_voz.lisp")
	(setf complete-story32 *story*)
    
	(load "voz/story33-complete-levinverb_nofunc_roleexp_prep_voz.lisp")
	(setf complete-story33 *story*)
    
	(load "voz/story34-complete-levinverb_nofunc_roleexp_prep_voz.lisp")
	(setf complete-story34 *story*)
    
	(load "voz/story35-complete-levinverb_nofunc_roleexp_prep_voz.lisp")
	(setf complete-story35 *story*)
    
	(load "voz/story36-complete-levinverb_nofunc_roleexp_prep_voz.lisp")
	(setf complete-story36 *story*)
    
	(load "voz/story37-complete-levinverb_nofunc_roleexp_prep_voz.lisp")
	(setf complete-story37 *story*)
    
	(load "voz/story38-complete-levinverb_nofunc_roleexp_prep_voz.lisp")
	(setf complete-story38 *story*)
    
	(load "voz/story39-complete-levinverb_nofunc_roleexp_prep_voz.lisp")
	(setf complete-story39 *story*)
    
	(load "voz/story40-complete-levinverb_nofunc_roleexp_prep_voz.lisp")
	(setf complete-story40 *story*)
    
	(setf complete-stories (list complete-story2 complete-story3 complete-story4 complete-story5 complete-story6 complete-story7 complete-story8 complete-story9 complete-story10 complete-story11 complete-story12 complete-story13 complete-story14 complete-story15 complete-story16 complete-story17 complete-story18 complete-story19 complete-story20 complete-story21 complete-story22 complete-story23 complete-story24 complete-story25 complete-story26 complete-story27 complete-story28 complete-story29 complete-story30 complete-story31 complete-story32 complete-story33 complete-story34 complete-story35 complete-story36 complete-story37 complete-story38 complete-story39 complete-story40))

	(load "voz/story1-partial-levinverb_nofunc_roleexp_prep_voz.lisp")
	(setf partial-story *story*)

	(setf retrieved-stories (retrieve-K-memories partial-story complete-stories 3 nil))
	(format t "~a~%" (length retrieved-stories))

	(setf *riu-debug* '(show-final-mapping))
	(dolist (source retrieved-stories)
		(generate-analogical-text-general partial-story source)
	)

)

(generate-story)
    