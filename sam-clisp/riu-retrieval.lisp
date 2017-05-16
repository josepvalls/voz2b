#|

This file contains functions related to retrieving specific memories from memory

|#

;; Adds the automatically generated keywords to a story or memory
(defun compute-keywords (story)
	(mapcar 
		#'(lambda (block)
			(let ((keywords (compute-block-keywords block)))
			  	(when (member 'retrieval *riu-debug* ) (format t "DEBUG: compute-keywords ~a -> ~a~%" (first block) keywords))
			
				`(,(first block) (:keywords ,@keywords) ,@(rest block))
			)
		)
		story
	)
)

;; This method computes automatically the keywords of a particular story block
(defun compute-block-keywords (block)
	(if (rest (assoc ':keywords (rest block)))
		(rest (assoc ':keywords (rest block)))
		(let ((keywords nil)
			  (structure (rest (assoc ':structure (rest block)))))
		  
			(dolist (phase structure)
			
				(let ((entities (rest (assoc ':entities (rest phase))))
					  (expressions (rest (assoc ':expressions (rest phase)))))
					;; get keywords of entities:
					(dolist (entity entities)
						(setf keywords (cons (first entity) keywords))
					)
				
					;; get keywords of relations
					(dolist (expression expressions)
						(setf keywords (cons (first (first expression)) keywords))
					)
				)
			)		
		
			(remove-duplicates keywords)
		)
	)
)


;; This method retrieves K memories from the memory according to the keywords similarity
(defun retrieve-K-memories (block memories k already-retrieved &key (similarity #'scene-similarity-keywords))
	(let ((memories-score 
			(mapcar
				#'(lambda (memory)
					(most-similar-instantiation block memory :similarity similarity)
				)
				;; All of those memories with an ID equal to a retrieved memory are not considered:
				(remove-if #'(lambda (memory) (member (car memory) (mapcar #'car already-retrieved))) memories)
			)))
		
		(setf memories-score (sort memories-score #'(lambda (m1 m2) (> (second m1) (second m2)))))
		(setf memories-score (subseq memories-score 0 (min k (length memories-score))))
		(when (member 'retrieval *riu-debug*) (format t "DEBUG: retrieve-K-memories ~a~%" (mapcar #'(lambda (x) (list (caar x) (second x))) memories-score)))		
		(mapcar 'first memories-score)
	)
)


(defun scene-similarity-random (scene1 scene2)
	(random 1.0)
)

(defun scene-similarity-keywords (scene1 scene2)
	(let* ((kw1 (compute-block-keywords scene1))
		   (kw2 (compute-block-keywords scene2))
		   (common (intersection kw1 kw2))
		   (similarity (/ (float (* 2 (length common))) (float (+ (length kw1) (length kw2))))))
		similarity
	)
)


(defun most-similar-instantiation (block memory &key (similarity #'scene-similarity-keywords))
	(let* ((instantiations (all-scene-instantiations memory))
		   (instantiations-score
			(mapcar
				#'(lambda (instantiation)
					(let ((kw1 (compute-block-keywords block))
						  (kw2 (compute-block-keywords instantiation))
						  (similarity (funcall similarity block instantiation)))
						(when (member 'retrieval *riu-debug*) 
							(format t "DEBUG: most-similar-instantiation ~a -> kw1 ~a kw2 ~a -> ~a~%" (first instantiation) kw1 kw2 similarity)
						)
						(list instantiation similarity)
					)
				)
				instantiations)
		   ))
		(first (sort instantiations-score #'(lambda (m1 m2) (> (second m1) (second m2)))))
	)
)
