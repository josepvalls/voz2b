;; This method takes as input a complete story DAG and runs it beginning to end
(defun run-story (story memory ales)
	(let ((new-story (run-story-block story memory ales)))
		(when new-story (run-story new-story memory ales))
	)
)


;; This method takes as input a complete story DAG and runs only the next block, returning the new story state or nil if the story is over
(defun run-story-block (story memory ales)
	(let* ((block-name (first story))
		   (story-dag (second story))
		   (block (assoc block-name story-dag)))
		(when (member 'discourse *riu-debug*) (format t "DEBUG: run-story-block block-name ~s~%" block-name))
		(if block
			(list (run-block block memory ales) story-dag)
			()
		)
	)
)


;; This method takes a story representation as input, and runs the clauses to generate text output
(defun run-block (story-block memory ales)
	(let* ((discourse (rest (assoc ':discourse (rest story-block))))
		   (clauses (rest (assoc ':clauses discourse)))
		   (templates (rest (assoc ':templates discourse)))
		   (current-clause (second (first clauses))))

		   (when (member 'discourse *riu-debug*) (format t "DEBUG: run-block current-clause ~s~%" current-clause))
		
		(run-clause current-clause clauses templates story-block memory ales)
	)
)

(defun run-clause (clause clauses templates block memory ales)
	(when (member 'discourse *riu-debug*) (format t "running clause ~a~%" clause))
	(cond
		;; TEMPLATE or JUMP to another CLAUSE
		((not (listp clause))
			(let ((next-clause (assoc clause clauses))
				  (next-template (assoc clause templates)))
				(if next-clause
					(run-clause (second next-clause) clauses templates block memory ales)
					(let ((retval (run-template (rest next-template))))
						(format t "~%")
						retval
					)
				)
			)
		)
	
		;; SEQUENTIAL
		((equal (car clause) :s) 
			(let ((retval nil))
				(dolist (c (rest clause))
					(setf retval (run-clause c clauses templates block memory ales))
				)
				retval
			)
		)

		;; ALTERNATIVE
		((equal (car clause) :a) 
			(run-clause (nth (random (length (rest clause))) (rest clause)) clauses templates block memory ales)
		)
		
		;; REPETITION
		((equal (car clause) :r)
			(let ((retval nil))
				(dotimes (i (second clause))
					(setf retval (run-clause (third clause) clauses templates block memory ales))					
				)
				retval
			)
		)
		
		;; GO-TO
		((equal (car clause) :gt)
			(second clause)
        )

		;; USER-INPUT
		((equal (car clause) :c)
			(let* ((options (rest clause))
				   (ales-predictions (mapcar #'(lambda (action) (predict-outcome-of-action action block ales)) options))
				   (ales-predisposition (mapcar #'(lambda (prediction) 
													(if prediction
														(list (* (first prediction) (assess-goals ales prediction)) (undesired-effects ales (second prediction)))
														(list 0.0 nil))) ales-predictions))
				   (selected (select-option (mapcar #'(lambda (option predisposition) (list (car option) (first predisposition) 
																										 (second predisposition)
																										)) options ales-predisposition) ales)))				
				(when (member 'prediction *riu-debug*) 
					(dolist (prediction ales-predictions) 
						(format t "DEBUG: run-clause ales predicts ~a~%" prediction))
					(dolist (predisposition ales-predisposition) 
						(format t "DEBUG: run-clause ales predisposition ~a~%" predisposition))
				)
				(when (member 'discourse *riu-debug*) (format t "DEBUG: run-clause selected by user ~s~%" (assoc selected (rest clause))))
				(run-clause (second (assoc selected (rest clause))) clauses templates block memory ales)
			)
		)

		;; MEMORY RETRIEVAL
		((equal (car clause) :m)
			(when (evaluate-probability (probability-of-memory ales clause))
				(let* ((memories (retrieve-K-memories block memory 3 (get-beliefs ales)))
					   (memory-analogies (sort 
											(remove-if #'(lambda (x) (null (Second x))) 
												(mapcar #'(lambda (memory) (list memory (analogy block (second clause) memory))) memories))
											#'(lambda (x y) (> (first (second x)) (first (second y))))))
					   (selected-memory nil))
					
					(when memory-analogies
						(setf selected-memory (first (first memory-analogies)))
						(format t "*** MEMORY START ***~%")
						(run-block selected-memory memory ales)
						(format t "*** MEMORY END ***~%")
						
						(add-belief ales selected-memory)
					)									
				)
			)
			nil
		)
	)
)

(defun run-template (template)
	(dolist (element template)
		(if (listp element)
			(run-template (rest element))
			(format t "~a" element)
		)
	)
	()
)

;; This function evaluates all the options for continuing a story with the BDI model of ALES, 
;; and asks for user input in case ALES cannot decide which option to select.
(defun select-option (options-predispositions ales)
	(let ((options (mapcar 'car options-predispositions))
		  (classified-options (classify-options-by-predispositions options-predispositions)))
		  
;;		(when (member 'intentionality *riu-debug*) (format t "Classified Options: ~a~%" classified-options))

		(when (evaluate-probability (probability-of-adding-expression-to-action ales))
			;; Express rejection for the actions he does not want:
			(when (first classified-options)
				(format t "Ales did not feel like to ~a" (car (first (first classified-options))))
				(dolist (action (rest (first classified-options)))
					(format t " or ~a" (car action))
				)
				(format t "~%")
			)

			;; Express excitement for the actions he does want:
			(when (third classified-options)
				(format t "Ales felt like to ~a" (car (first (third classified-options))))
				(dolist (action (rest (third classified-options)))
					(format t " or ~a" (car action))
				)
				(format t "~%")
			)
		)
			
		(cond
			((third classified-options)
				;; There are some actions that ALES WANTS to execute:				
				(if (and (= (length (third classified-options)) 1)
						 (evaluate-probability (probability-of-executing-wanted-actions ales)))
					(progn 
						(format t "~a~%> ~a~%" options (caar (third classified-options)))
						(caar (third classified-options))
					)
					(if (evaluate-probability (probability-of-rejecting-unwanted-actions ales))
						(progn
							(format t "~a~%> " (mapcar 'car (third classified-options)))
							(user-input (mapcar 'car (third classified-options)))
						)
						(if (evaluate-probability (probability-of-rejecting-unwanted-actions-after-selected ales))
							(progn
								(format t "~a~%> " options)
								(user-input-rejecting (append (first classified-options) (second classified-options)) 
										    		  (mapcar 'first (third classified-options)))							
							)
							(progn
								(format t "~a~%> " options)
								(user-input options)							
							)
						)
					)
				)
			)
			((second classified-options)
				;; There are some actions that ALES does not care executing:
				(if (and (= (length (second classified-options)) 1)
						 (evaluate-probability (probability-of-executing-wanted-actions ales)))
					(progn 
						(format t "~a~%> ~a~%" options (caar (second classified-options)))
						(caar (second classified-options))
					)
					(if (evaluate-probability (probability-of-rejecting-unwanted-actions ales))
						(progn
							(format t "~a~%> " (mapcar 'car (second classified-options)))
							(user-input (mapcar 'car (second classified-options)))
						)
						(if (evaluate-probability (probability-of-rejecting-unwanted-actions-after-selected ales))						
							(progn
								(format t "~a~%> " options)
								(user-input-rejecting (first classified-options) 
													  (mapcar 'first (second classified-options)))	
							)
							(progn
								(format t "~a~%> " options)
								(user-input options)
							)
						)
					)
				)
			)
			(t
				;; ALES does NOT want to execute any of the actions:
				(if (= (length (first classified-options)) 1)
					(progn 
						(format t "~a~%> ~a~%" options (caar (first classified-options)))
						(caar (first classified-options))
					)
					(progn
						(format t "~a~%> " (mapcar 'car (first classified-options)))
						(user-input (mapcar 'car (first classified-options)))
					)
				)
			)
		)
	)
)

;; This function gets user input
(defun user-input (options)
	(when (member 'intentionality *riu-debug*) (format t "--- user-input ~a ---~%" options))
	(let ((input (read)))
		(if (member input options)
			input
			(progn
				(format t "That is not a valid option!~%> ")
				(user-input options)
			)
		)
	)
)

;; This function gets user input, but it will reject any action in the first group
(defun user-input-rejecting (options-to-reject options-to-accept)
	(when (member 'intentionality *riu-debug*) (format t "--- user-input-rejecting ~a ~a ---~%" options-to-reject options-to-accept))
	(let ((input (read)))
		(if (member input options-to-accept)
			input
			(if (assoc input options-to-reject)
				(progn
					(reject-option input (third (assoc input options-to-reject)))
					(format t "~a~%> " (append (remove input (mapcar 'first options-to-reject)) options-to-accept)) 
					(user-input-rejecting (remove (assoc input options-to-reject) options-to-reject) options-to-accept)
				)
				(progn
					(format t "That is not a valid option!~%> ")
					(user-input-rejecting options-to-reject options-to-accept)
				)
			)
		)
	)
)


(defun reject-option (option prediction)
	(format t "Ales did not want to ~a because he did not want " option)
	(let ((first t))
		(dolist (clause prediction)
			(unless first (format t " or "))
			(format t "~a to be ~a" (second (first clause)) (first (first clause)))
			(setf first nil)
		)
	)
	(format t "~%")
)

(defun classify-options-by-predispositions (options-predispositions)
	(let ((LOWER-THRESHOLD	-5.0)
		  (UPPER-THRESHOLD	 5.0)
		  (not-wanted ())
		  (indifferent ())
		  (wanted ()))
		(let ((minimum nil)
			  (maximum nil))
			(dolist (option options-predispositions)
				(when (or (eq minimum nil) (< (second option) minimum)) (setf minimum (second option)))
				(when (or (eq maximum nil) (> (second option) maximum)) (setf maximum (second option)))
				)
			(when (< maximum LOWER-THRESHOLD)
				(setf LOWER-THRESHOLD (/ (+ minimum maximum) 2.0)))
			(when (> minimum UPPER-THRESHOLD)
				(setf UPPER-THRESHOLD (/ (+ minimum maximum) 2.0)))
		)
		
		(when (member 'intentionality *riu-debug*) (format t "Thresholds: ~a - ~a ~%" LOWER-THRESHOLD UPPER-THRESHOLD))
		 
		(dolist (option options-predispositions)
			(if (<= (second option) LOWER-THRESHOLD) 
				(setf not-wanted (cons option not-wanted))
				(if (>= (second option) UPPER-THRESHOLD) 
					(setf wanted (cons option wanted))
					(setf indifferent (cons option indifferent))
				)
			)
		)
		(list not-wanted indifferent wanted)
	)
)


