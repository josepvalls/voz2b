(defun analogy (story-state phase memory &key 
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
		(when (member 'analogy-selection *riu-debug*) 
			(format t "DEBUG: ~a analogies found ~%" (length analogies))
	    	(dolist (analogy analogies)
				(format t "DEBUG:   ~a~%" analogy)
			)
		)
    	(dolist (analogy analogies selected)
			(when (or (null selected)
                  	  (> (first analogy) selected-weight))
					(setf selected analogy
						  selected-weight (first analogy)))
	    )
	)
)


(defun get-all-analogies (story-state phase memory &key 
				additional-entities-story
				additional-expressions-story
				additional-entities-memory
				additional-expressions-memory)
	(when (member 'analogy *riu-debug* ) (format t "analogy between:~a and ~a~%" (first story-state) 
																			     (first memory)))
	(when (member 'analogy *riu-debug* ) (format t "~a ~a ~a ~a~%" additional-entities-story additional-expressions-story additional-entities-memory additional-expressions-memory))
	
	(let* ((common (rest (assoc 'common (rest (assoc ':structure (rest story-state))))))
		   (input (rest (assoc phase (rest (assoc ':structure (rest story-state))))))
		   (entities (append (rest (assoc ':entities common)) (append (rest (assoc ':entities input)) additional-entities-story)))
		   (expressions (append (rest (assoc ':expressions common)) (append (rest (assoc ':expressions input)) additional-expressions-story))))
		
		(setf entities (append  '((phase :type inanimate) (phase1 :type phase) (phase2 :type phase)) entities))
				
		(when (member 'analogy *riu-debug* ) (format t "input for the story ~a ~a~%" entities expressions))
		(dolist (e entities)
			(when (member 'analogy *riu-debug* ) (format t "defining entity ~a for first term~%" e))
			(eval (cons 'sme:defEntity e))
		)
	
		(eval 
			(list 'sme:defDescription 'analogy-story
				'entities 
				(mapcar 'car entities)
				'expressions
				expressions
			)
		)
	)
	
	;; From the memory we always assume that the analogy is with phase1
	(let* ((common (rest (assoc 'common (rest (assoc ':structure (rest memory))))))
		   (input  (rest (assoc 'phase1 (rest (assoc ':structure (rest memory))))))
		   (entities (append (rest (assoc ':entities common)) (append (rest (assoc ':entities input)) additional-entities-memory)))
		   (expressions (append (rest (assoc ':expressions common)) (append (rest (assoc ':expressions input)) additional-expressions-memory))))

		(setf entities (append  '((phase :type inanimate) (phase1 :type phase)) entities))
		
		(when (member 'analogy *riu-debug* ) (format t "input for the memory ~a ~a~%" entities expressions))
		(dolist (e entities)
			(when (member 'analogy *riu-debug* ) (format t "defining entity ~a for second term~%" e))
			(eval (cons 'sme:defEntity e))
		)
		
		(eval
			(list 'sme:defDescription 'analogy-memory
				'entities 
				(mapcar 'car entities)
				'expressions
				expressions
			)
		)
	)
		 	
	;; Call SME and select a mapping:
	(sme:match 'analogy-story 'analogy-memory (member 'analogy *riu-debug* ))
	(let ((analogies nil))
		(dolist (gmap sme:*gmaps*)
			(when (member 'analogy *riu-debug* ) (format t "Processing gmap ~a~%" gmap))
			(let ((mappings nil))
			
				(dolist (mapping (sme:gm-elements gmap))
				    (when (equal (sme:mh-type mapping) ':expression)
						(let ((base-item   (sme:expression-name (sme:mh-base-item mapping)))
							  (target-item (sme:expression-name (sme:mh-target-item mapping))))
							(when (member 'analogy *riu-debug* ) (format t "mapping ~a -> ~a~%" base-item target-item))
							(setf mappings (cons (list target-item base-item) mappings))
						)
					)
				    (when (equal (sme:mh-type mapping) ':entity)
						(let ((base-item   (sme:entity-name (sme:mh-base-item mapping)))
							  (target-item (sme:entity-name (sme:mh-target-item mapping))))
							(when (member 'analogy *riu-debug* ) (format t "mapping ~a -> ~a~%" base-item target-item))
							(setf mappings (cons (list target-item base-item) mappings))
						)
					)
				)
				
				(setf analogies (cons (list (sme::node-belief+ (sme::gm-bms-node gmap)) mappings) analogies))	
			)
		)
		;; Return the score and mappings:
;		(when (or (member 'retrieval *riu-debug*) (member 'analogy *riu-debug*)) (format t "analogy result ~a~%" analogies))
		analogies
	)	
)



(defun find-text-for (token pattern)
;;  (format t "~a - ~a~%" token pattern)
  (if (and (listp pattern) (not (eq pattern nil)))
  	(if (equal token (first pattern))
 		pattern
    	(let ((tmp (find-text-for token (car pattern))))
			(if tmp
				tmp
				(find-text-for token (rest pattern))))
  	)
    nil
  )
)

(defun change-text (token new-text pattern)
;;  (format t "~a - ~a~%" token pattern)
  (when (and (listp pattern) (not (eq pattern nil)))
  	(if (equal token (first pattern))
 		(setf (rest pattern) new-text)
		(progn
			(change-text token new-text (car pattern))
			(change-text token new-text (rest pattern))
		)
  	)
  )
)

(defun remove-starting-with (command clauses)
	(remove nil
		(mapcar 
			(lambda (clause)
				(if (listp clause)
					(if (eq (car clause) command)
						nil
						(remove-starting-with command clause)
					)
					clause
				)
			)
		clauses)
	)
)


(defun replace-with-selected-action-clause (clauses action)
	(remove nil
		(mapcar 
			(lambda (clause)
				(if (listp clause)
					(if (eq (car clause) ':c)
						(if (member action clause :test 'equal) 
							(replace-with-selected-action-clause (second action) action)
							 nil)
						(replace-with-selected-action-clause clause action)
					)
					clause
				)
			)
		clauses)
	)
)


;; (generate-analogical-text  (first *riu-memory*)  (first *riu-story-dag*) (find-scene-choices (first *riu-story-dag*)))

(defun generate-analogical-text (original-memory original-scene original-action &key (analogy-method #'analogy))
	(format t "~%~%Evaluating ~a - ~a (~a) ~%" (car original-memory) (car original-scene) (car original-action))

	(let* ((memory (clone-structure original-memory))
		   (scene (clone-structure original-scene))
		   (action (clone-structure original-action))
		   (result (funcall analogy-method scene (third action) memory :additional-entities-story (fourth action) :additional-expressions-story (fifth action)))
		   (mappings (second result))
		   (memory-clauses (rest (assoc ':clauses (rest (assoc ':discourse (rest memory))))))
		   (memory-templates (rest (assoc ':templates (rest (assoc ':discourse (rest memory))))))
		   (memory-structure (rest (assoc ':structure (rest memory))))
		   (memory-common-structure (assoc 'common memory-structure))
		   (scene-structure (rest (assoc ':structure (rest scene))))
		   (scene-common-structure (assoc 'common scene-structure))
		
		   (transferred-clauses nil)
		   (transferred-templates nil)
		   (transferred-entities nil)
		   (transferred-expressions nil)
		   (additional-transferred-tokens nil)
		)
				
;		(format t "MEMORY STRUCTURE:~%~a~%" memory-structure)
;		(format t "SCENE STRUCTURE:~%~a~%" scene-structure)
		
		(setf memory-structure (remove memory-common-structure memory-structure))
		(setf scene-structure (remove scene-common-structure scene-structure))
		
		;; Add the "commmon" entities and expressions to all the phases:
		(dolist (phase memory-structure)
			(let ((entities (rest (assoc ':entities (rest memory-common-structure))))
				  (structure (rest (assoc ':expressions (rest memory-common-structure)))))
				(setf (rest (assoc ':entities (rest phase))) (append (rest (assoc ':entities (rest phase))) entities))
				(setf (rest (assoc ':entities (rest phase))) (append (rest (assoc ':entities (rest phase))) `((phase :type inanimate) (,(car phase) :type phase))))
				(setf (rest (assoc ':expressions (rest phase))) (append (rest (assoc ':expressions (rest phase))) structure))
			)
		)
		(dolist (phase scene-structure)
			(let ((entities (rest (assoc ':entities (rest scene-common-structure))))
				  (structure (rest (assoc ':expressions (rest scene-common-structure)))))
				(setf (rest (assoc ':entities (rest phase))) (append (rest (assoc ':entities (rest phase))) entities))
				(setf (rest (assoc ':entities (rest phase))) (append (rest (assoc ':entities (rest phase))) `((phase :type inanimate) (,(car phase) :type phase))))
				(setf (rest (assoc ':expressions (rest phase))) (append (rest (assoc ':expressions (rest phase))) structure))
			)
		)
		
;		(format t "MEMORY STRUCTURE:~%~a~%" memory-structure)
;		(format t "SCENE STRUCTURE:~%~a~%" scene-structure)
				
		;; Bring the additional phases from the memory to the scene:
		(dolist (phase memory-structure)
			(when (member 'eval *riu-debug*) (format t "P: considering phase: ~a~%" (first phase)))		
			(if (assoc (first phase) scene-structure)
				;; If the scene already has the phase:
				(when (member 'eval *riu-debug*) (format t "P: the scene already has the phase: ~a~%" (first phase)))		
				
				;; If the scene does not have it:
				(let ((transferred (clone-structure phase)))
				  (when (member 'eval *riu-debug*) (format t "P: transferring phase: ~a~%" (first transferred)))		
				  (setf scene-structure (append scene-structure (list transferred)))
				  (dolist (entity (rest (assoc ':entities (rest transferred))))
					(unless (member entity (rest (assoc ':entities (rest memory-common-structure))) :test #'equal)
						(when (member 'eval *riu-debug*) (format t "E: transferred: ~a~%" entity))
						(setf transferred-entities (cons entity transferred-entities))
					)
				  )
				  (dolist (expression (rest (assoc ':expressions (rest transferred))))
					(unless (member expression (rest (assoc ':expressions (rest memory-common-structure))) :test #'equal)
				    	(when (member 'eval *riu-debug*) (format t "C: transferred: ~a~%" expression))
						(setf transferred-expressions (cons expression transferred-expressions))
					)
				  )
				  (setf transferred-clauses (cons (assoc (car transferred) memory-clauses) transferred-clauses))
				)
			)
		)
		
		;; Consider as "transferred entities and expressions" everything added by the "action":
		(dolist (entity (fourth action))
			(dolist (mapping mappings)
				(when (equal (second mapping) (car entity))
					(setf additional-transferred-tokens (cons (car mapping) additional-transferred-tokens))
				)
			)
		)
		(dolist (expression (fifth action))
			(dolist (mapping mappings)
				(when (equal (second mapping) (car (last expression)))
					(setf additional-transferred-tokens (cons (car mapping) additional-transferred-tokens))
				)
			)
		)
		
		(when (member 'eval *riu-debug*) (format t "additional-transferred-tokens:~a~%" additional-transferred-tokens))
			
		;; Find the templates in the transferred clauses:
		(when (member 'eval *riu-debug*) (format t "Transferred clauses: ~a~%" transferred-clauses))
		(dolist (clause transferred-clauses)
			(setf transferred-templates (append (remove-if #'(lambda (template) (not (member (car template) (templates-in-clause clause)))) memory-templates) transferred-templates))
		)
		(setf transferred-templates (remove-duplicates transferred-templates))
		(when (member 'eval *riu-debug*) (format t "Transferred templates: ~a~%" transferred-templates))
		
		;; Apply the mappings to the structure:
		(dolist (mapping mappings)
			(when (member 'eval *riu-debug*)  (format t "Substitute ~a for ~a~%" (first mapping) (second mapping)))
	      	(setf scene-structure (replace-recursive-nondestructive (first mapping) (second mapping) scene-structure))
		)
		
		;; Apply the mappings to the templates:
		(let* ((scene-templates (rest (assoc ':templates (rest (assoc ':discourse (rest scene))))))
			   (extended-mapping 
					(remove 'nil 
						(mapcar (lambda (mapping)
								;; obtain the text for the mapping:
								(let ((text (rest (find-text-for (second mapping) scene-templates))))
									(unless (null text)
										(list (first mapping) (second mapping) (find-text-for (second mapping) scene-templates)))
										)
								) 
					 			mappings)
						)
					)
				(dictionary nil))
					
			(when (member 'eval *riu-debug*) (format t "Extended mapping: ~a~%" extended-mapping))
			
			(dolist (mapping extended-mapping)
				(change-text (first mapping) (rest (third mapping)) transferred-templates)
			)
			
			;; change the template names and create a dictionary for then changing the clauses:
			(setf transferred-templates 
				(mapcar (lambda (x) (let ((new-name (intern (string-upcase (format nil "TRANSFERRED-~a" (first x))))))
							(setf dictionary (cons (list (first x) new-name) dictionary))
							(cons new-name (rest x))
						))
					transferred-templates))
			
			(when (member 'eval *riu-debug*) (format t "Template dictionary: ~a~%" dictionary))

			(setf (rest (assoc ':templates (rest (assoc ':discourse (rest scene)))))
				  (append transferred-templates
						  (rest (assoc ':templates (rest (assoc ':discourse (rest scene)))))))
			
			;; translate the transferred clauses:
			(dolist (translation dictionary)
				(setf transferred-clauses (replace-recursive-nondestructive (first translation) (second translation) transferred-clauses))
			)
		)

		(when (member 'eval *riu-debug*) (format t "Mapped templates: ~a~%" transferred-templates))
						
		;; Remove any user input or memory trigger clause that might be in the scene:
		(setf (rest (assoc ':clauses (rest (assoc ':discourse (rest scene)))))
			(replace-with-selected-action-clause (rest (assoc ':clauses (rest (assoc ':discourse (rest scene))))) action)
		)
		(setf (rest (assoc ':clauses (rest (assoc ':discourse (rest scene)))))
			(remove-starting-with ':m (rest (assoc ':clauses (rest (assoc ':discourse (rest scene)))))))		
		(setf (rest (assoc ':clauses (rest (assoc ':discourse (rest scene)))))
			(remove-starting-with ':gt (rest (assoc ':clauses (rest (assoc ':discourse (rest scene)))))))		
		
		;; Add the sequence of transferred clauses in the proper order:
		;; This code assumes that the first clause of the memory contains the proper order of the phases we want:
		;; TO-DO: extract the set of temporal relations of all the phases in the resulting scene, and construct a new clause that implements them
		(let ((tmp (assoc ':clauses (rest (assoc ':discourse (rest scene))))))
			(setf (rest tmp)
				(cons (first memory-clauses)
					  (append (rest tmp) transferred-clauses))
			)
		)
;;		(let ((tmp (assoc ':clauses (rest (assoc ':discourse (rest scene))))))
;;			(setf (rest tmp)
;;				(cons (list 'c000 (append (list ':s (car (second tmp))) (mapcar #'car transferred-clauses)))
;;					(append (rest tmp) transferred-clauses))
;;			)
;;		)
		

		;; Always print the mappings:
		(print mappings)

		(when (member 'eval *riu-debug*) 	
			(print memory)
			(print scene))
		
		;; Generate discourse of the new story:
		(format t "~%~%")
		(run-block scene nil nil)
		
		mappings
	)
)


;;;;; -----------------------------------------------------------------------------------------------------
;;;;; -----    The methods here are equivalent to the method above, but they are more generic,        -----
;;;;; -----    and do not assume any number of phases or any other structure                          -----
;;;;; -----------------------------------------------------------------------------------------------------


(defun generate-story-by-choosing-action (original-scene action &optional (remove-memories nil))
;	(format t "scene: ~a~%" original-scene)
;	(format t "action: ~a~%" action)

    (if (equal action '(nil nil nil nil nil))
		original-scene
		(let ((scene (clone-structure original-scene)))
			;; remove choice block:
			(setf (rest (assoc ':clauses (rest (assoc ':discourse (rest scene)))))
				(replace-with-selected-action-clause (rest (assoc ':clauses (rest (assoc ':discourse (rest scene))))) action))
			(when remove-memories
				(setf (rest (assoc ':clauses (rest (assoc ':discourse (rest scene)))))
					(remove-starting-with ':m (rest (assoc ':clauses (rest (assoc ':discourse (rest scene)))))))
			)
	;		(format t "scene without choice: ~a~%" scene)
		
			;; add entities and expressions to phase:
			;  (FEED (S: T5B (GT RIU-STORY-1C-FEED-CAT)) PHASE1 NIL (((FEED ALES CAT) NAME S0-P1-FEED) ((FD-MOVE-TENDENCY S0-P1-AGONIST S0-P1-FEED PHASE1) NAME S0-P1-MOVE-AGONIST)))
			(let* ((structure (assoc ':structure (rest scene)))
				   (phase (assoc (third action) (rest structure)))
				   (entities (assoc ':entities (rest phase)))
				   (expressions (assoc ':expressions (rest phase))))
			
				(setf (rest entities) (append (rest entities) (fourth action)))
				(setf (rest expressions) (append (rest expressions) (fifth action)))
			)
		
	;		(run-block scene nil nil)
		
			scene
		)	
	)
)


(defun analogy-general (target source phase-pairing)
	(let ((analogies (get-all-analogies-general target source phase-pairing))
		  (selected nil)
		  (selected-weight 0))
		(when (member 'analogy-selection *riu-debug*) 
			(format t "DEBUG (analogy-general): phase-pairing ~a ~%" phase-pairing)
			(format t "DEBUG (analogy-general): ~a analogies found ~%" (length analogies))
;	    	(dolist (analogy analogies)
;				(format t "DEBUG (analogy-general):   ~a~%" analogy)
;			)
		)
    	(dolist (analogy analogies selected)
			(when (or (null selected)
                  	  (> (first analogy) selected-weight))
					(setf selected analogy
						  selected-weight (first analogy))
					(when (member 'analogy-selection *riu-debug*) 
						(format t "DEBUG (analogy-general):   ~a~%" selected)
						)
					)
	    )
	)
)


(defun get-all-analogies-general (target source phase-pairing)

	(let* ((common-target (rest (assoc 'common (rest (assoc ':structure (rest target))))))
	       (common-source (rest (assoc 'common (rest (assoc ':structure (rest source))))))
		   (entities-target (rest (assoc ':entities common-target)))
		   (entities-source (rest (assoc ':entities common-source)))
		   (expressions-target (rest (assoc ':expressions common-target)))
		   (expressions-source (rest (assoc ':expressions common-source)))
		   (translation-table nil))

		(setf entities-target (cons '(phase :type inanimate) entities-target))
		(setf entities-source (cons '(phase :type inanimate) entities-source))

		(dolist (pairing phase-pairing)
			(let* ((target-phase-id (first pairing))
				   (source-phase-id (rest pairing))
				   (target-phase (rest (assoc target-phase-id (rest (assoc ':structure (rest target))))))
				   (source-phase (rest (assoc source-phase-id (rest (assoc ':structure (rest source)))))))
				(setf entities-target (cons `(,target-phase-id :type phase) entities-target))
				(setf entities-source (cons `(,source-phase-id :type phase) entities-source))
				(setf entities-target (append entities-target (rest (assoc ':entities target-phase))))
				(setf entities-source (append entities-source (rest (assoc ':entities source-phase))))
				(setf expressions-target (append expressions-target (rest (assoc ':expressions target-phase))))
				(setf expressions-source (append expressions-source (rest (assoc ':expressions source-phase))))
			)
		)		

		;; rename all entities in the domains to avoid name clashes:
		(dolist (entity entities-target)
			(let* ((original (car entity))
			  	   (changed (read (make-string-input-stream (concatenate 'string "target-" (symbol-name (car entity)))))))
				(setf translation-table (cons (cons original changed) translation-table))
			)
		)

		(dolist (translation translation-table)
			(setf entities-target (replace-recursive-nondestructive (first translation) (rest translation) entities-target))
			(setf expressions-target (replace-recursive-nondestructive (first translation) (rest translation) expressions-target))
		)
		
		(when (member 'analogy *riu-debug* ) (format t "input for the target ~a ~a~%" entities-target expressions-target))
		(when (member 'analogy *riu-debug* ) (format t "input for the source ~a ~a~%" entities-source expressions-source))
		(dolist (e entities-target)
			(when (member 'analogy *riu-debug* ) (format t "defining entity ~a for target~%" e))
			(eval (cons 'sme:defEntity e))
		)
		(dolist (e entities-source)
			(when (member 'analogy *riu-debug* ) (format t "defining entity ~a for source~%" e))
			(eval (cons 'sme:defEntity e))
		)

		(eval 
			(list 'sme:defDescription 'analogy-target
				'entities 
				(mapcar 'car entities-target)
				'expressions
				expressions-target
			)
		)
		(eval
			(list 'sme:defDescription 'analogy-source
				'entities 
				(mapcar 'car entities-source)
				'expressions
				expressions-source
			)
		)
	
		;; Call SME and select a mapping:
		(sme:match 'analogy-target 'analogy-source (member 'analogy *riu-debug* ))
		(let ((analogies nil))
			(dolist (gmap sme:*gmaps*)
				(when (member 'analogy *riu-debug* ) (format t "Processing gmap ~a~%" gmap))
				(let ((mappings nil))
					(dolist (mapping (sme:gm-elements gmap))
					    (when (equal (sme:mh-type mapping) ':expression)
							(let ((base-item   (sme:expression-name (sme:mh-base-item mapping)))
								  (target-item (sme:expression-name (sme:mh-target-item mapping))))
								(when (member 'analogy *riu-debug* ) (format t "mapping ~a -> ~a~%" base-item target-item))
								(setf mappings (cons (list target-item base-item) mappings))
							)
						)
					    (when (equal (sme:mh-type mapping) ':entity)
							(let ((base-item   (sme:entity-name (sme:mh-base-item mapping)))
								  (target-item (sme:entity-name (sme:mh-target-item mapping))))
								(when (member 'analogy *riu-debug* ) (format t "mapping ~a -> ~a~%" base-item target-item))
								(setf mappings (cons (list target-item base-item) mappings))
							)
						)
					)

					(setf analogies (cons (list (sme::node-belief+ (sme::gm-bms-node gmap)) mappings) analogies))	
				)
			)

			;; undo the renamings:
			(dolist (translation translation-table)
				(setf analogies (replace-recursive-nondestructive (rest translation) (first translation) analogies))
			)
			analogies
		)
	)
)


;; This method is like the previous one, but for any two scenes regardless of their number of phases:
(defun generate-analogical-text-general (original-target original-source &key (pairings nil) (analogy-method #'analogy-general))

	(let* ((source (clone-structure original-source))
		   (target (clone-structure original-target))
		   (phases-source (remove 'common (mapcar #'first (rest (assoc ':structure (rest source))))))
		   (phases-target (remove 'common (mapcar #'first (rest (assoc ':structure (rest target)))))))
		
		(when (member 'eval *riu-debug*) (format t "phases source: ~a~%" phases-source))
		;; Rename all the phases from the source to avoid conflicts:
		(dolist (phase phases-source)
			(let ((changed (read (make-string-input-stream (concatenate 'string "source-" (symbol-name phase))))))
				(setf source (replace-recursive-nondestructive phase changed source))
				(setf pairings (mapcar #'(lambda (pairing) (mapcar #'(lambda (x) (if (equal phase (rest x)) (cons (first x) changed) x)) pairing)) pairings))
			)
		)
		(setf phases-source (remove 'common (mapcar #'first (rest (assoc ':structure (rest source))))))
		(when (member 'eval *riu-debug*) (format t "phases source (renamed): ~a~%" phases-source))

		(let* ((time-relations-source (phase-time-relations source))
			   (time-relations-target (phase-time-relations target))
			   (source-clauses (rest (assoc ':clauses (rest (assoc ':discourse (rest source))))))
			   (source-templates (rest (assoc ':templates (rest (assoc ':discourse (rest source))))))
			   (source-structure (rest (assoc ':structure (rest source))))
			   (source-common-structure (assoc 'common source-structure))
			   (target-structure (rest (assoc ':structure (rest target))))
			   (target-common-structure (assoc 'common target-structure))
			   (selected-pairing nil)
			   (phases-used-from-source nil)	;; the phases from source in the selected-pairing
			   (selected-analogy nil)
			   (mappings nil)
			   (transferred-clauses nil)
			   (transferred-templates nil)
			   (transferred-entities nil)
			   (transferred-expressions nil)
			  )
				
			;; Extract temporal relations among phases:
			(when (member 'analogy-text-generation *riu-debug*) (format t "source relations: ~a~%" time-relations-source))
			(when (member 'analogy-text-generation *riu-debug*) (format t "target relations: ~a~%" time-relations-target))
		
			;; If not given, generate pairings:
			(when (null pairings)
				(setf pairings (phase-pairings phases-target phases-source time-relations-target time-relations-source))
			)
			(when (member 'analogy-text-generation *riu-debug*) (format t "pairings: ~a~%" pairings))
		
			;; For each pairing generate an analogy:
			(dolist (pairing pairings)
				(let ((result (funcall analogy-method target source pairing)))
					(when (member 'analogy-text-generation *riu-debug*) (format t "Analogy result for pairing: ~a~%" result))
					(when (or (null selected-analogy)
		                  	  (and result 
		                  	  	   (> (first result) (first selected-analogy))))
							(setf selected-analogy result
								  selected-pairing pairing))
				)
			)
			(setf phases-used-from-source (mapcar #'rest selected-pairing))
			(setf mappings (second selected-analogy))
			(when (member 'analogy-general-selection *riu-debug*) (format t "Selected pairing: ~a~%" selected-pairing))
			(when (member 'analogy-general-selection *riu-debug*) (format t "Selected analogy: ~a~%" selected-analogy))
		
			;; create a first skeleton of the resulting story by taking the target story, and bringing all of the additional phases form the source:		
			;; Merge the "commmon" entities and expressions to all the phases (for ease of handling):
			(setf source-structure (remove source-common-structure source-structure))
			(setf target-structure (remove target-common-structure target-structure))
			(dolist (phase source-structure)
				(let ((entities (rest (assoc ':entities (rest source-common-structure))))
					  (structure (rest (assoc ':expressions (rest source-common-structure)))))
					(setf (rest (assoc ':entities (rest phase))) (append (rest (assoc ':entities (rest phase))) entities))
					(setf (rest (assoc ':entities (rest phase))) (append (rest (assoc ':entities (rest phase))) `((phase :type inanimate) (,(car phase) :type phase))))
					(setf (rest (assoc ':expressions (rest phase))) (append (rest (assoc ':expressions (rest phase))) structure))
				)
			)
			(dolist (phase target-structure)
				(let ((entities (rest (assoc ':entities (rest target-common-structure))))
					  (structure (rest (assoc ':expressions (rest target-common-structure)))))
					(setf (rest (assoc ':entities (rest phase))) (append (rest (assoc ':entities (rest phase))) entities))
					(setf (rest (assoc ':entities (rest phase))) (append (rest (assoc ':entities (rest phase))) `((phase :type inanimate) (,(car phase) :type phase))))
					(setf (rest (assoc ':expressions (rest phase))) (append (rest (assoc ':expressions (rest phase))) structure))
				)
			)
		
			;; Bring the additional phases from the source to the target:
			(dolist (phase source-structure)
				(when (member 'eval *riu-debug*) (format t "P: considering phase: ~a~%" (first phase)))		
				;; IfBring it if it has noe been used in the pairing:
				(when (not (member (first phase) phases-used-from-source))
					(let ((transferred (clone-structure phase)))
					  (when (member 'eval *riu-debug*) (format t "P: transferring phase: ~a~%" (first transferred)))		
					  (setf target-structure (append target-structure (list transferred)))
					  (dolist (entity (rest (assoc ':entities (rest transferred))))
						(unless (member entity (rest (assoc ':entities (rest source-common-structure))) :test #'equal)
							(when (member 'eval *riu-debug*) (format t "E: transferred: ~a~%" entity))
							(setf transferred-entities (cons entity transferred-entities))
						)
					  )
					  (dolist (expression (rest (assoc ':expressions (rest transferred))))
						(unless (member expression (rest (assoc ':expressions (rest source-common-structure))) :test #'equal)
					    	(when (member 'eval *riu-debug*) (format t "C: transferred: ~a~%" expression))
							(setf transferred-expressions (cons expression transferred-expressions))
						)
					  )
					  (setf transferred-clauses (cons (assoc (car transferred) source-clauses) transferred-clauses))
					)
				)
			)	
			
			;; Find the templates in the transferred clauses:
			(when (member 'eval *riu-debug*) (format t "Transferred clauses: ~a~%" transferred-clauses))
			(dolist (clause transferred-clauses)
				(setf transferred-templates (append (remove-if #'(lambda (template) (not (member (car template) (templates-in-clause clause)))) source-templates) transferred-templates))
			)
			(setf transferred-templates (remove-duplicates transferred-templates))
			(when (member 'eval *riu-debug*) (format t "Transferred templates: ~a~%" transferred-templates))
			
		
			;; Apply the mappings to the structure:
			(dolist (mapping mappings)
				(when (member 'eval *riu-debug*)  (format t "Substitute ~a for ~a~%" (first mapping) (second mapping)))
		      	(setf target-structure (replace-recursive-nondestructive (first mapping) (second mapping) target-structure))
			)

			;; Apply the mappings to the templates:
			(let* ((target-templates (rest (assoc ':templates (rest (assoc ':discourse (rest target))))))
				   (extended-mapping 
						(remove 'nil 
							(mapcar (lambda (mapping)
									;; obtain the text for the mapping:
									(let ((text (rest (find-text-for (second mapping) target-templates))))
										(unless (null text)
											(list (first mapping) (second mapping) (find-text-for (second mapping) target-templates)))
											)
									) 
						 			mappings)
							)
						)
					(dictionary nil))

				(when (member 'eval *riu-debug*) (format t "Extended mapping: ~a~%" extended-mapping))

				(dolist (mapping extended-mapping)
					(dolist (template transferred-templates)
						(when (member-recursive (first mapping) template)
;							(format t "Text change: ~%~w -> ~w~%~w~%" (first mapping) (rest (third mapping)) template)
;						    (dolist (template2 target-templates)
;								(when (member-recursive (first (third mapping)) template2)
;									(format t "Hint: ~w~%" template2)
;								)
;							)
							(change-text (first mapping) (rest (third mapping)) template)
						)
					)
				)

				;; change the template names and create a dictionary for then changing the clauses:
				(setf transferred-templates 
					(mapcar (lambda (x) (let ((new-name (intern (string-upcase (format nil "TRANSFERRED-~a" (first x))))))
								(setf dictionary (cons (list (first x) new-name) dictionary))
								(cons new-name (rest x))
							))
						transferred-templates))

				(when (member 'eval *riu-debug*) (format t "Template dictionary: ~a~%" dictionary))

				(setf (rest (assoc ':templates (rest (assoc ':discourse (rest target)))))
					  (append transferred-templates
							  (rest (assoc ':templates (rest (assoc ':discourse (rest target)))))))

				;; translate the transferred clauses:
				(dolist (translation dictionary)
					(setf transferred-clauses (replace-recursive-nondestructive (first translation) (second translation) transferred-clauses))
				)

			)
			(when (member 'eval *riu-debug*) (format t "Mapped templates: ~a~%" transferred-templates))

			;; Remove any user input, memory trigger, or go-to clause that might be in the target:
			(setf (rest (assoc ':clauses (rest (assoc ':discourse (rest target)))))
				(remove-starting-with ':m (rest (assoc ':clauses (rest (assoc ':discourse (rest target)))))))
			(setf (rest (assoc ':clauses (rest (assoc ':discourse (rest target)))))
				(remove-starting-with ':gt (rest (assoc ':clauses (rest (assoc ':discourse (rest target)))))))
			(setf (rest (assoc ':clauses (rest (assoc ':discourse (rest target)))))
				(remove-starting-with ':c (rest (assoc ':clauses (rest (assoc ':discourse (rest target)))))))

			;; Add the sequence of transferred clauses in the proper order:
			;; generate order of the pairings:			
			(let ((tmp (assoc ':clauses (rest (assoc ':discourse (rest target)))))
				  (phases-result phases-source)
				  (phase-order-clause (generate-clause-from-time-relations phases-source time-relations-source)))
				(when (member 'eval *riu-debug*) (format t "phases source: ~a~%" phases-source))
				(when (member 'eval *riu-debug*) (format t "time relations source: ~a~%" time-relations-source))
				(when (member 'eval *riu-debug*) (format t "source clause: ~a~%" phase-order-clause))
				(dolist (pair selected-pairing)
;					(format t "pair: ~a~%" pair)
					(setf phase-order-clause (replace-recursive-nondestructive (rest pair) (first pair) phase-order-clause))
					(setf phases-result (replace-recursive-nondestructive (rest pair) (first pair) phases-result))
				)
				(when (member 'eval *riu-debug*) (format t "resulting clause: ~a~%" phase-order-clause))
				; remove all the clauses for phases that are not present:
				(when (member 'eval *riu-debug*) (format t "phases result: ~a~%" phases-result))
				(setf (rest tmp) (remove-if #'(lambda (x) (not (member (car x) phases-result))) (rest tmp)))
				(setf (rest tmp)
					(cons (list 'c0 phase-order-clause)
						  (append (rest tmp) transferred-clauses))
				)
				(when (member 'eval *riu-debug*) (format t "resulting clauses: ~a~%" tmp))
			)

			(when (member 'show-final-mapping *riu-debug*) (format t "mapping: ~a~%" selected-analogy))
				
			(when (member 'eval *riu-debug*) 	
				(print source)
				(print target))

			;; Generate discourse of the new story:
			(format t "~%~%")
			(run-block target nil nil)
			target
		)
	)
)


;; This method obtains the set of time relations between all the phases in a scene:
(defun phase-time-relations (scene)
	(let* ((clauses (rest (assoc ':clauses (rest (assoc ':discourse (rest scene))))))
		   (phases (remove 'common (mapcar #'first (rest (assoc ':structure (rest scene))))))
		   (relations nil))
		(dolist (p1 phases (remove-duplicates relations :test #'equal))
			(dolist (p2 phases)
				(unless (eq p1 p2)
					(setf relations (append relations (phase-time-relations-clauses p1 p2 clauses)))
				)
			)
		)
	)
)


(defun phase-time-relations-clauses (phase1 phase2 clauses)
	(apply #'append (mapcar #'(lambda (c) (phase-time-relations-clause phase1 phase2 (second c))) clauses))
)


(defun phase-time-relations-clause (phase1 phase2 clause)
;	(format t "phase-time-relations-clause: ~a  ~a  ~a~%" phase1 phase2 clause)
	(cond
		((not (listp clause)) nil)
		((equal (car clause) :s) 
			(let* ((appearances (mapcar #'(lambda (tmp) (templates-in-clause tmp)) (rest clause)))
				   (index1 (member-sublist-index appearances phase1))
				   (index2 (member-sublist-index appearances phase2)))
				(cond
					((or (null index1) (null index2)) nil)
					((< index1 index2) `((before ,phase1 ,phase2)))
					((> index1 index2) `((before ,phase2 ,phase1)))
					(t (apply #'append (mapcar #'(lambda (c) (phase-time-relations-clause phase1 phase2 c)) (rest clause))))
				)
			)
		)
		((equal (car clause) :a) 
			(let* ((appearances (mapcar #'(lambda (tmp) (templates-in-clause tmp)) (rest clause)))
			       (index1 (member-sublist-index appearances phase1))
			       (index2 (member-sublist-index appearances phase2)))
				(cond
					((or (null index1) (null index2)) nil)
					((= index1 index2) (apply #'append (mapcar #'(lambda (c) (phase-time-relations-clause phase1 phase2 c)) (rest clause))))
					(t `((exclusion ,phase2 ,phase1)))
				)
			)
		)
		(t nil)
	)
)


(defun member-sublist-index (l e &optional (index 0))
	(if (null l) 
		nil
		(if (member e (first l))
			index
			(member-sublist-index (rest l) e (+ 1 index))
		)
	)
)


; (length phases1) < (length phases2)
(defun phase-pairings (phases1 phases2 relations1 relations2 &optional (pairings nil))
	(if (null phases1)
		(list pairings)
		(let ((phase1 (car phases1)))
			(apply #'append 
				(mapcar 
					#'(lambda (phase2)
						(when (valid-pairings (cons `(,phase1 . ,phase2) pairings) relations1 relations2)
							(phase-pairings (rest phases1) (remove phase2 phases2) relations1 relations2 (cons `(,phase1 . ,phase2) pairings))
						)
					) 
					phases2)
			)
		)
	)
)


(defun valid-pairings (pairings relations1 relations2)
;	(format t "testing validity of ~a~%" pairings)
	(dotimes (i1 (length pairings) t)
		(dotimes (i2 (length pairings))
			(when (> i2 i1)
				(let* ((p1 (nth i1 pairings))
					   (p2 (nth i2 pairings))
					   (ra (time-relation (first p1) (first p2) relations1))
					   (rb (time-relation (rest p1) (rest p2) relations2)))
;					(format t "relation: ~a - ~a~%" ra rb)
					(when (and (not (null ra)) (not (null rb)))
						(when (and (eq ra 'exclusion) (eq rb 'before)) (return-from valid-pairings nil))
						(when (and (eq ra 'exclusion) (eq rb 'after)) (return-from valid-pairings nil))
						(when (and (eq ra 'before) (eq rb 'exclusion)) (return-from valid-pairings nil))
						(when (and (eq ra 'after) (eq rb 'exclusion)) (return-from valid-pairings nil))
						(when (and (eq ra 'after) (eq rb 'before)) (return-from valid-pairings nil))
						(when (and (eq ra 'before) (eq rb 'after)) (return-from valid-pairings nil))
					)
				)
			)
		)
	)
)



(defun time-relation-clause (phase clause relations)
	(let ((c-phases (if (listp clause) (templates-in-clause clause) (list clause)))
		  (relation nil))
		(dolist (c-phase c-phases relation)
			(let ((r (time-relation phase c-phase relations)))
				(when (null r) (return-from time-relation-clause nil))
				(if (null relation)
					(setf relation r)
					(when (not (eq relation r)) (return-from time-relation-clause nil))
				)
			)
		)
	)

)


;; result: before, after, exclusion
(defun time-relation (phase1 phase2 relations)
	(if (null relations)
		nil
		(if (and (member phase1 (rest (first relations))) (member phase2 (rest (first relations)))) 
			(if (eq (car (first relations)) 'exclusion)
				'exclusion
				(if (eq (second (first relations)) phase1)
					'before
					'after
				)
			)
			(time-relation phase1 phase2 (rest relations))
		)
	)
)


(defun generate-clause-from-time-relations (phases relations &optional (clause nil))
	(if (null phases)
		clause
		(generate-clause-from-time-relations (rest phases) relations (add-phase-to-clause (first phases) clause relations))
	)
)


(defun add-phase-to-clause (phase clause relations)
	(cond
		((null clause) phase)
		((and (listp clause) (eq (first clause) ':s))
			(let ((relation (time-relation-clause phase clause relations)))
				(cond 
					((eq relation 'before) `(:s ,phase ,@(rest clause)))
					((eq relation 'after) `(:s ,@(rest clause) ,phase))
					((eq relation 'exclusion) `(:a ,phase ,clause))
					(t 
						(let ((relations (mapcar #'(lambda (element) (time-relation-clause phase element relations)) (rest clause))))
;							(format t ":s ->  ~a~%" relations)
							(if (member 'exclusion relations)
								(let* ((first-excl (position 'exclusion relations))
									   (last-excl (- (length relations) (position 'exclusion (reverse relations))))
									   (first-part (subseq (rest clause) 0 first-excl))
									   (middle-part (subseq (rest clause) first-excl last-excl))
									   (last-part (subseq (rest clause) last-excl (length (rest clause)))))
;									(format t "split: ~a - ~a - ~a~%" first-part middle-part last-part)
									`(:s ,@first-part ,(if (> (length middle-part) 1) 
																		`(:a ,phase (:s ,@middle-part))
																		(if (and (listp (car middle-part)) (eq (caar middle-part) ':a))
																		 	`(:a ,phase ,@(rest (first middle-part)))
																			`(:a ,phase ,(first middle-part)))) ,@last-part)
								)
								(format t "CASE NOT CONSIDERED 1 in 'add-phase-to-clause'!!!!!!!!!!!!!!! :a with order relations:  ~a~%" relations)  ;; <-- find the point where it changes from after to before, and inster there
							)
						)
					)
				)
			)
		)
		((and (listp clause) (eq (first clause) ':a))
			(let ((relation (time-relation-clause phase clause relations)))
				(cond 
					((eq relation 'before) `(:s ,phase ,clause))
					((eq relation 'after) `(:s ,clause ,phase))
					((eq relation 'exclusion) `(:a ,phase ,@clause))
					(t
						(let ((relations (mapcar #'(lambda (element) (time-relation-clause phase element relations)) (rest clause))))
							(format t "CASE NOT CONSIDERED 2 in 'add-phase-to-clause'!!!!!!!!!!!!!!! :a with order relations:  ~a~%" relations)
							clause
						)
					)
				)
			)
		)		
		(t 
			(cond
				((member (list 'before phase clause) relations :test 'equal) (list ':s phase clause))
				((member (list 'before clause phase) relations :test 'equal) (list ':s clause phase))
				((member (list 'exclusion phase clause) relations :test 'equal) (list ':a phase clause))
				((member (list 'exclusion clause phase) relations :test 'equal) (list ':a phase clause))
				(t (list ':s clause phase))
			)
		)
	)
)

