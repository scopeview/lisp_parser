(in-package :lexer)

(defclass lexer ()
  ((stream :initarg :stream :initform *standard-input* :accessor lexer-stream)
   (filter-group-list :initarg :filter-group-list)
   (filter-group-state-stack :initarg :filter-group-state-stack :initform nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod lexer-get-current-group ((lexer lexer))
  (let ((group-name)
	(group))
    (with-slots (filter-group-list filter-group-state-stack) lexer
      (assert filter-group-state-stack)
      (setq group-name (car filter-group-state-stack))
      (setq group (find-if #'(lambda (filter-group)
			       (with-slots (filter-group-name) filter-group
				 (equal group-name filter-group-name)))
			   filter-group-list))
      (assert group)
      group)))

(defmethod lexer-read-token ((lexer lexer))
  (let ((group (lexer-get-current-group lexer))
	(result-status)
	(result-token))
    (with-slots (stream) lexer
      (loop
	 do (destructuring-bind (status token) (filter-group-filterate group stream)
	      (ecase status
		('filtered (progn
			     (setq result-token token)
			     (return)))
		('eof (return))
		('not-filtered (error "lexer-read-token not-filtered")))))
      result-token)))

;; (defmethod lexer-unread-token ((lexer lexer) token)
;;   (with-slots (stream) lexer
;;     (with-slots (token-buf token-list-reduced-from) token
;;       (assert (null token-list-reduced-from))
;;       (unread-element stream #\space)
;;       (unread-element-seq stream token-buf))))
