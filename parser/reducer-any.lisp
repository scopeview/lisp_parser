(in-package :parser)

(defclass any-reducer ()
  ((is-match-empty :initarg :is-match-empty :initform nil)
   (token-type-reduced :initarg :token-type-reduced)
   (token-type-list-reduced-from :initarg :token-type-list-reduced-from)))

(set-pprint-dispatch 'any-reducer
		     #'(lambda (stream structure)
			 (with-slots (is-match-empty token-type-reduced token-type-list-reduced-from) structure
			   (funcall (formatter "~@<#<~;~W ~W ~W ~W>~:>") stream 'any-reducer
				    is-match-empty token-type-reduced token-type-list-reduced-from))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sany (token-type-reduced &rest token-type-list-reduced-from)
  (make-instance 'any-reducer :token-type-reduced token-type-reduced :is-match-empty nil
		 :token-type-list-reduced-from token-type-list-reduced-from))
(defun sany* (token-type-reduced &rest token-type-list-reduced-from)
  (make-instance 'any-reducer :token-type-reduced token-type-reduced :is-match-empty t
		 :token-type-list-reduced-from token-type-list-reduced-from))

(defmethod reducer-reduce ((any-reducer any-reducer) (parser-streamer parser-streamer))
  (with-slots (is-match-empty token-type-reduced token-type-list-reduced-from) any-reducer
    (let ((result-status 'not-reduced)
	  (result-token)
	  (token (read-element parser-streamer)))
      (when token
	(with-slots (lexer::token-type) token
	  (if (member lexer::token-type token-type-list-reduced-from)
	      (progn
		(setq result-status 'full-reduced)
		(setq result-token (make-instance 'token
						  :token-type token-type-reduced
						  :token-list-reduced-from (list token))))
	      (progn
		(unread-element parser-streamer token)
		(when is-match-empty
		  (setq result-status 'full-reduced))))))
      (list result-status result-token))))
