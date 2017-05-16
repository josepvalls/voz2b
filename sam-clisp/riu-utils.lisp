(defun replace-nondestructive (e1 e2 l)
	(if l
		(if (equal e1 (car l)) (cons e2 (replace-nondestructive e1 e2 (rest l)))
							   (cons (car l) (replace-nondestructive e1 e2 (rest l))))
		()
	)
)

(defun replace-recursive-nondestructive (e1 e2 l)
	(if l
		(if (equal e1 (car l)) 
			(cons e2 (replace-recursive-nondestructive e1 e2 (rest l)))
			(if (listp (car l))
				(cons (replace-recursive-nondestructive e1 e2 (car l)) (replace-recursive-nondestructive e1 e2 (rest l)))
				(cons (car l) (replace-recursive-nondestructive e1 e2 (rest l)))))
		()
	)
)

(defun remove-all (to-remove original-list)
	(let ((ret original-list))
		(dolist (element to-remove)
			(setf original-list (remove element original-list))
		)
		original-list
	)
)


(defun clone-structure (l)
	(if (listp l)
		(mapcar #'clone-structure l)
		l
	)
)

(defun member-recursive (e l)
  (if (equal e l)
 	t
    (when (and (listp l) (not (eq l nil))) (or (member-recursive e (car l)) (member-recursive e (rest l))))
  )
)


(defun cartesian-product (l)
	(let ((result (list ())))
		(dolist (set l result)
			(let ((tmp-result nil))
				(dolist (one-complete-instantiation result)
					(dolist (element set)
						(setf tmp-result (cons (append one-complete-instantiation (list element)) tmp-result))
					)
				)
			(setf result tmp-result)
			)
		)
	)
)


(defun find-named-substructure (structure id)
	(if (listp structure)
		(if (eq (car structure) id)
			structure
			(dolist (e structure nil)
				(let ((res (find-named-substructure e id)))
					(when (not (null res))
						(return-from find-named-substructure res)
					)
				)
			)
		)
		nil
	)
)
