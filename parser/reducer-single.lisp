(in-package :parser)

(defclass single-reducer ()
  ((token-type-reduced :initarg :token-type-reduced)
   (token-type-reduced-from :initarg :token-type-reduced-from)))

(set-pprint-dispatch 'single-reducer
		     #'(lambda (stream structure)
			 (with-slots (token-type-reduced token-type-reduced-from) structure
			   (funcall (formatter "~@<#<~;~W ~W ~W>~:>") stream 'single-reducer
				    token-type-reduced token-type-reduced-from))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod reducer-reduce ((single-reducer single-reducer) (parser-streamer parser-streamer))
  (with-slots (token-type-reduced token-type-reduced-from) single-reducer
    (let ((result-status 'not-reduced)
	  (result-token)
	  (token (read-element parser-streamer)))
      (when token
	(if (lexer:token-typep token token-type-reduced-from)
	    (progn
	      (setq result-status 'full-reduced)
	      (setq result-token (make-instance 'token
						:token-type token-type-reduced
						:token-list-reduced-from (list token))))
	    (unread-element parser-streamer token)))
      (list result-status result-token))))
