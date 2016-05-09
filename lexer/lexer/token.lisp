(in-package :lexer)

(defclass token ()
  ((token-id)
   (token-type :initarg :token-type)
   (token-buf :initarg :token-buf :initform nil)
   (token-list-reduced-from :initarg :token-list-reduced-from :initform nil :accessor class-token-list-reduced-from)))

(set-pprint-dispatch 'token
		     #'(lambda (stream structure)
			 (with-slots (token-type token-buf token-list-reduced-from) structure
			   (funcall (formatter "~@<#<~;~W ~W ~W ~I~_~/pprint-fill/~;>~:>") stream 'token
				    token-type (if token-buf
						   (coerce token-buf 'string)
						   token-buf) token-list-reduced-from))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod token-to-string ((tok token))
  (with-slots (token-type token-buf token-list-reduced-from) tok
    (format nil "token(~A, ~A)" token-type (if token-buf
					       (coerce token-buf 'string)
					       (mapcar #'token-to-string token-list-reduced-from)))))

(defmethod token-type-equal ((l token) (r token))
  (utils:let-with-slots ((l-token-type token-type)) l
    (utils:let-with-slots ((r-token-type token-type)) r
      (equal l-token-type r-token-type))))

(defmethod token-typep ((tok token) expected-token-type)
  (with-slots (token-type) tok
    (assert (not (null expected-token-type)))
    (equal expected-token-type token-type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-lexer-token-packer-default (token-type)
  #'(lambda (filter-result-buf)
      (make-instance 'token :token-type token-type :token-buf filter-result-buf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod token-orignal-token-list ((token token))
  (let ((token-list))
    (with-slots (token-buf token-list-reduced-from) token
      (cond
	((and token-buf
	      (not token-list-reduced-from))
	 (setq token-list (list token)))
	((and (not token-buf)
	      token-list-reduced-from)
	 (setq token-list (reduce #'(lambda (l r) (concatenate 'list l r))
				  (mapcar #'(lambda (token) (token-orignal-token-list token))
					  token-list-reduced-from))))))
    token-list))

(defun token-orignal-token-list-from-list (token-list)
  (declare (type list token-list))
  (let ((orignal-token-list))
    (loop for token in token-list
       with list
       do (progn
	    (setq list (token-orignal-token-list token))
	    (setq orignal-token-list (concatenate 'list orignal-token-list list))))
    orignal-token-list))
