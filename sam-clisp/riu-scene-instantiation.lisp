;; This method takes one scene as input, and selects only one of the possible paths to run the scene according to the clauses. 
;; It returns the same scene, but without all the non referred expressions and entities.
(defun randomly-instantiate-scene (scene)
	(let* 
		((discourse (rest (assoc ':discourse (rest scene))))
		(clauses (rest (assoc ':clauses discourse)))
		(reduced-clauses (mapcar #'randomly-simplify-clause clauses)))
		
		(instantiate-scene scene reduced-clauses)
	)
)


(defun all-scene-instantiations (scene)
	(let*
		((discourse (rest (assoc ':discourse (rest scene))))
		(clauses (rest (assoc ':clauses discourse)))
	 	(individual-clause-instantiations (mapcar #'all-clause-instantiations clauses))
		(complete-instantiations (cartesian-product individual-clause-instantiations)))
		
		(mapcar #'(lambda (instantiation)
				(instantiate-scene scene instantiation)) complete-instantiations)
	)
)


(defun sub-clauses (clause)
	(remove (car clause) (templates-in-clause clause))
)


(defun reachable-clauses (clauses &optional (current (car clauses)) (closed nil))
	(let ((next (remove nil (mapcar #'(lambda (id) (assoc id clauses)) (sub-clauses current)))))
		(remove-duplicates 
			(cons current 
				(apply 'append 
					(mapcar #'(lambda (c) 
							(if (member c closed)
								closed
								(reachable-clauses clauses c (cons current closed))
							)
						  ) 
						next)
				)
			)
		)
	)

;	(dolist (c reachable))
;		(remove-duplicates (append (sub-clauses c) reachable))
;		)
)


(defun instantiate-scene (scene reduced-clauses-unfiltered)
	(let* 
		((discourse (rest (assoc ':discourse (rest scene))))
		(clauses (rest (assoc ':clauses discourse)))
		(templates (rest (assoc ':templates discourse)))
		(structure (rest (assoc ':structure (rest scene))))
		(reduced-clauses (reachable-clauses reduced-clauses-unfiltered))
		
		
		;; compute the set of templates used in the reduced clauses:
		(template-ids (remove-duplicates (apply #'append (mapcar #'templates-in-clause reduced-clauses))))
		(reduced-templates (remove-if #'(lambda (x) (not (member (car x) template-ids))) templates))
		(removed-templates (remove-if #'(lambda (x) (member (car x) template-ids)) templates))
		
		;; compute the set of entities and expressions referred to in the reduced set of templates
		(structure-ids (remove-duplicates (append (apply #'append (mapcar #'get-template-labels reduced-templates))
												  (mapcar #'car structure))))
									
		;; compute the set of entities and expressions appearing in the templates that we are not going to use:
		(structure-ids-in-removed (remove-duplicates (append (apply #'append (mapcar #'get-template-labels removed-templates))
												  			 (mapcar #'car structure))))
												
		;; the set of entities and expressions ONLY appearing in the removed templates are unwanted:
		(unwanted-structure-ids (remove-if #'(lambda (x) (member x structure-ids)) structure-ids-in-removed))
		
		;; Find those expressions that refer to wanted expressions:
		(additional-structure-ids (find-expressions-referring-to-wanted-ids (apply #'append (mapcar #'(lambda (phase) (rest (assoc ':expressions (rest phase)))) structure)) structure-ids))
		(final-structure-ids (remove-if #'(lambda (x) (member x unwanted-structure-ids)) additional-structure-ids))
		
		;; reconstruct the structure, trim each of the phases to include only entities and expressions included in the templates:
		(reduced-structure (mapcar #'(lambda (phase)
				(list (car phase) 
					(cons ':entities (reduced-entities (rest (assoc ':entities (rest phase))) final-structure-ids))
					(cons ':expressions (reduced-expressions (rest (assoc ':expressions (rest phase))) final-structure-ids)))
				) structure))
		)
		
;		(format t "reduced clauses: ~a~%" reduced-clauses)
;		(format t "reduced templates: ~a~%" reduced-templates)
;		(format t "removed templates: ~a~%" removed-templates)
;		(format t "unwanted IDs: ~a~%" unwanted-structure-ids)
;		(format t "final IDS: ~a~%" final-structure-ids)
;		(format t "reduced structure: ~a~%" reduced-structure)
		
		(list (first scene) 
			  (list ':discourse
			  	(cons ':clauses reduced-clauses)
			  	(cons ':templates reduced-templates)
			  ) 
		  	  (cons ':structure reduced-structure)
		)
	)
)


;; This method takes a clause as input, and selects only one of the possible paths to run the clause
(defun randomly-simplify-clause (clause)
	(cond
		((not (listp clause)) clause)
		((equal (car clause) :s) (mapcar #'randomly-simplify-clause clause)) 
		((equal (car clause) :a) (list :a (randomly-simplify-clause (nth (random (length (rest clause))) (rest clause)))))
		((equal (car clause) :gt) clause) 
		((equal (car clause) :c) clause) 
		((equal (car clause) :m) clause) 
		((equal (car clause) :r) (mapcar #'randomly-simplify-clause clause)) 
		(t (mapcar #'randomly-simplify-clause clause))
	)
)

(defun all-clause-instantiations (clause)
	(cond
		((not (listp clause)) (list clause))
		((equal (car clause) :s) (cartesian-product (mapcar #'all-clause-instantiations clause))) 
		((equal (car clause) :a) (apply #'append (mapcar #'(lambda (x) (cartesian-product (list '(:a) (all-clause-instantiations x)))) (rest clause))))
		((equal (car clause) :gt) (list clause)) 
		((equal (car clause) :c) (list clause)) 
		((equal (car clause) :m) (list clause)) 
		((equal (car clause) :r) (cartesian-product (mapcar #'all-clause-instantiations clause))) 
		(t (cartesian-product (mapcar #'all-clause-instantiations clause)))
	)
)

(defun templates-in-clause (clause)
	(cond
		((not (listp clause)) (list clause))
		((equal (car clause) :s) (apply #'append (mapcar #'templates-in-clause (rest clause)))) 
		((equal (car clause) :a) (apply #'append (mapcar #'templates-in-clause (rest clause))))  
		((equal (car clause) :gt) ()) 
		((equal (car clause) :c) ()) 
		((equal (car clause) :m) ()) 
		((equal (car clause) :r) (apply #'append (mapcar #'templates-in-clause (rest clause))))
		(t (apply #'append (mapcar #'templates-in-clause clause)))
		)
)

(defun get-template-labels (template)
	(cond
		((listp template) (apply #'append (mapcar #'get-template-labels template)))
		((stringp template) ())
		(t (list template))
	)
)

(defun find-expressions-referring-to-wanted-ids (expressions wanted-ids)
	(let ((extended-wanted-ids wanted-ids))
		(dolist (expression expressions)
			(when (and (not (member (third expression) wanted-ids)) 
					   (eval `(and ,@(mapcar #'(lambda (x) (if (member x extended-wanted-ids) t nil)) (rest (first expression))))))
				(setf extended-wanted-ids (cons (third expression) extended-wanted-ids))
			)
		)
		(if (> (length extended-wanted-ids) (length wanted-ids)) (find-expressions-referring-to-wanted-ids expressions extended-wanted-ids)
			wanted-ids)
	)
)

(defun reduced-entities (entities desired-ids)
	(remove-if #'(lambda (x) (not (member (car x) desired-ids))) entities)
)

(defun reduced-expressions (expressions desired-ids)
	(remove-if #'(lambda (x) (not (member (third x) desired-ids))) expressions)
)
