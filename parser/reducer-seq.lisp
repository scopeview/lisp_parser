(in-package :parser)

(defclass seq-reducer ()
  ((token-type-reduced :initarg :token-type-reduced)
   (token-type-list-reduced-from :initarg :token-type-list-reduced-from)))

(set-pprint-dispatch 'seq-reducer
		     #'(lambda (stream structure)
			 (with-slots (token-type-reduced token-type-list-reduced-from) structure
			   (funcall (formatter "~@<#<~;~W ~W ~W>~:>") stream 'seq-reducer
				    token-type-reduced token-type-list-reduced-from))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sseq (token-type-reduced &rest token-type-list-reduced-from)
  (make-instance 'seq-reducer :token-type-reduced token-type-reduced
		 :token-type-list-reduced-from token-type-list-reduced-from))

(defmethod reducer-reduce ((seq-reducer seq-reducer) parser-streamer)
  (with-slots (token-type-reduced token-type-list-reduced-from) seq-reducer
    (let ((token-stack)
	  (complete t))
      (loop for token-type-reduced-from in token-type-list-reduced-from
	 with token
	 do (progn
	      (setq token (read-element parser-streamer))
	      (cond
		((null token) (progn
				(setq complete nil)
				(return)))
		((and token
		      (lexer:token-typep token token-type-reduced-from)) (push token token-stack))
		(t (progn
		     (unread-element parser-streamer token)
		     (setq complete nil)
		     (return))))))
      (cond
	(complete (list 'full-reduced (make-instance 'token
						     :token-type token-type-reduced
						     :token-list-reduced-from (reverse token-stack))))
	((and (not complete)
	      token-stack) (list 'partial-reduced (reverse token-stack)))
	((and (not complete)
	      (null token-stack)) (list 'not-reduced nil))))))
