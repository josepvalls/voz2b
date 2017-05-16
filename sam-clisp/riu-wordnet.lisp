#|

This file contains functions to integrate WordNet information into Riu

|#


(defun wordnet-similarity (term1 term2 wn)
    (if (eq term1 term2)
		1.0
		(let* ((wn1 (rest (assoc term1 wn)))
		   	   (wn2 (rest (assoc term2 wn)))
		   	   (features (remove-duplicates (append (mapcar 'car wn1) (mapcar 'car wn2))))
		   	   (accum nil))		
			(dolist (feature features 
				 	 (let ((tmp (reduce #'(lambda (x y) (list (+ (first x) (first y)) (+ (second x) (second y)))) accum :initial-value '(0 0))))
						(if (= (first tmp) 0.0) 
							0.0
							(float (/ (first tmp) (second tmp))))
				 	 )
					)
				(let* ((v1 (rest (assoc feature wn1)))
				  	   (v2 (rest (assoc feature wn2)))
				   	   (u (union v1 v2))
				   	   (i (intersection v1 v2)))
					(when u (setf accum (cons (list (length i) (length u)) accum)))
				)
			)
		)
	)
)



(defun concept-hierarchy-similarity (h1 h2 wn)
	(max
		(apply 'max
			(mapcar 
				#'(lambda (t1) 
					(wordnet-similarity t1 (first h2) wn)
				)
				h1
			)
		)
		(apply 'max
			(mapcar 
				#'(lambda (t2) 
					(wordnet-similarity (first h1) t2 wn)
				)
				h2
			)
		)
	)
)


;; Given a term and a scene, returns the list of concepts that describe such term, e.g. for "ales", it might be (ales robot animate), for the ID of an expression, it might just be the verb, e.g.: for M1-P1-WALKS it can be (walks)

(defun concept-hierarchy-from-scene (term scene additional-entities additional-expressions)
	(let ((structure (rest (assoc ':structure (Rest scene)))))
		(if (assoc term structure) (list term 'phase)
			(progn
				(dolist (entity additional-entities)
					(when (eq (car entity) term) (return-from concept-hierarchy-from-scene (cons (first entity) (concept-hierarchy-from-scene (third entity) scene additional-entities additional-expressions))))
				)
				(dolist (expression additional-expressions)
					(when (eq (third expression) term) (return-from concept-hierarchy-from-scene (list (caar expression))))
				)
				(dolist (phase structure (list term))
					(let ((entities (rest (assoc ':entities (rest phase))))
						  (expressions (rest (assoc ':expressions (rest phase)))))
						(dolist (entity entities)
							(when (eq (car entity) term) (return-from concept-hierarchy-from-scene (cons (first entity) (concept-hierarchy-from-scene (third entity) scene additional-entities additional-expressions))))
						)
						(dolist (expression expressions)
							(when (eq (third expression) term) (return-from concept-hierarchy-from-scene (list (caar expression))))
						)
					)
				)
			)
		)
	)
)


(defun scene-similarity-WordNet (scene1 scene2)
	(let* ((kw1l (compute-block-keywords scene1))
	   	   (kw2l (compute-block-keywords scene2))
		   (matrix 
			(if (> (length kw1l) (length kw2l))
				(mapcar #'(lambda (kw2) 
							(mapcar #'(lambda (kw1) 
										(wordnet-similarity kw1 kw2 *wordnet-associations*)
									  )
									kw1l))
						kw2l)
				(mapcar #'(lambda (kw1) 
							(mapcar #'(lambda (kw2) 
										(wordnet-similarity kw1 kw2 *wordnet-associations*)
									  )
									kw2l))
						kw1l)
				))
			)
		(/ (* 2 (apply '+ (mapcar #'(lambda (row) (apply 'max row)) matrix))) (float (+ (length kw1l) (length kw2l))))
	)
)


;; This method first computes a "wordnet-score" for each mapping as the average score of each of the mappings, then mutiplies the wordnet-score by the SME score to rank the different mappings given by SME. It returns the highest ranked mapping.

(defun analogy-wordnet (story-state phase memory &key 
				additional-entities-story
				additional-expressions-story
				additional-entities-memory
				additional-expressions-memory)
	(let ((analogies (get-all-analogies story-state phase memory 
										:additional-entities-story additional-entities-story
										:additional-expressions-story additional-expressions-story
										:additional-entities-memory additional-entities-memory
										:additional-expressions-memory additional-expressions-memory
										))
		  (selected nil)
		  (selected-weight 0))
    	(dolist (analogy analogies selected)
			(let* ((mappings (second analogy))
			       (strengths
					(mapcar #'(lambda (mapping)
							(let* ((term1 (first mapping))
								   (term2 (second mapping))
								   (h1 (concept-hierarchy-from-scene term1 memory additional-entities-memory additional-expressions-memory))
								   (h2 (concept-hierarchy-from-scene term2 story-state additional-entities-story additional-expressions-story))
								  )
;;								(format t "WORDNET:  ~a - ~a -> ~a~%" term1 term2 s)
								(concept-hierarchy-similarity h1 h2 *wordnet-associations*)
							)
						)
						mappings
					))
				   (wordnet-score (/ (apply '+ strengths) (length strengths)))
				   (joint-score (* (first analogy) wordnet-score)))
				(when (member 'analogy-selection *riu-debug*) 
					(format t "DEBUG:   ~a -> WORDNET SCORE: ~a  (~a)~%" analogy wordnet-score joint-score)
				)
				(when (or (null selected)
	                  	  (> joint-score selected-weight))
						(setf selected analogy
							  selected-weight joint-score))
			)
	    )
	)
)
