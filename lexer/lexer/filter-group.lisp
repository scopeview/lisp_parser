(in-package :lexer)

(defclass filter-group ()
  ((filter-group-name :initarg :filter-group-name)
   (filter-list :initarg :filter-list :initform nil)))

(defmethod add-filter ((filter-group filter-group) filter)
  (let ((stack))
    (with-slots (filter-list) filter-group
      (setq stack (reverse filter-list))
      (pushnew filter stack)
      (setq filter-list (reverse stack)))))

(defmethod filter-group-filterate-single-pass ((filter-group filter-group) stream)
  (let ((result-status 'filter-list-end)
	(result-token))
    (with-slots (filter-list) filter-group
      (loop for filter in filter-list
	 do (destructuring-bind (status token) (funcall filter stream)
	      (ecase status
		('not-filtered nil)
		('filtered (progn
			     (setq result-status status)
			     (setq result-token token)
			     (return)))
		('eof (progn
			(setq result-status status)
			(assert (null token))
			(return)))))))
    (when (equal 'filter-list-end result-status)
      (let ((next (read-element stream)))
	(unless next
	  (setq result-status 'eof)
	  (setq result-token nil))))
    (list result-status result-token)))

(defmethod filter-group-filterate ((filter-group filter-group) stream)
  (let ((result-status 'not-filtered)
	(result-token))
    (loop 
       do (destructuring-bind (status token) (filter-group-filterate-single-pass filter-group stream)
	    (ecase status
	      ('filtered (when token
			   (setq result-status status)
			   (setq result-token token)
			   (return)))
	      ('eof (progn
		      (setq result-status status)
		      (assert (null token))
		      (return)))
	      ('filter-list-end (progn
				  (setq result-status status)
				  (assert (null token)))))))
    (list result-status result-token)))
