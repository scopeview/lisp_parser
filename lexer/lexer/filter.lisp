(in-package :lexer)

(defun make-lexer-filter (&key filter token-packer)
  (declare (type function filter)
	   (type function token-packer))
  #'(lambda (stream)
      (lexer-filterate stream filter token-packer)))

(defmacro define-lexer-filter (name &key filter token-type)
  `(progn
     (let ((filter-e ,filter)
	   (token-packer))
       (declare (type function filter-e))
       (if ,token-type
	   (setq token-packer (make-lexer-token-packer-default ,token-type))
	   (setq token-packer (make-lexer-token-packer-default ',name)))
       (defun ,name (stream)
	 (lexer-filterate stream :filter filter-e :token-packer token-packer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-token-packer (token-type)
  (if token-type
      #'(lambda (filter-result-buf)
	  (make-instance 'token
			 :token-type token-type
			 :token-buf filter-result-buf))
      #'(lambda (filter-result-buf)
	  nil)))

(defun token-filter (&key string char any-char many-any-char function others token-type)
  (when (< 1 (count-if #'utils:notnull (list string char any-char many-any-char function)))
    (error (format nil "only one key is allowed")))
  (cond
    (string (token-filter--string string token-type))
    (char (token-filter--char char token-type))
    (any-char (token-filter--any-char any-char token-type))
    (many-any-char (token-filter--many-any-char many-any-char token-type))
    (function (token-filter--function function token-type))
    (others (token-filter--others token-type))))

(defun token-filter--string (string token-type)
  (declare (type string string))
  (let* ((matcher (utils:make-matcher :string-pattern string))
	 (filter (utils:make-filter matcher))
	 (token-packer (make-token-packer token-type)))
    (make-lexer-filter :filter filter
		       :token-packer token-packer)))

(defun token-filter--char (char token-type)
  (declare (type character char))
  (let* ((matcher (utils:make-matcher :char-pattern char))
	 (filter (utils:make-filter matcher))
	 (token-packer (make-token-packer token-type)))
    (make-lexer-filter :filter filter
		       :token-packer token-packer)))

(defun token-filter--any-char (char-list token-type)
  (declare (type list char-list))
  (loop for char in char-list
     do (assert (equal 'standard-char (type-of char))))
  (let* ((matcher-list (loop for char in char-list
			  collect (utils:make-matcher :char-pattern char)))
	 (filter-list (loop for matcher in matcher-list
			 collect (utils:make-filter matcher)))
	 (filter (apply #'utils:any filter-list))
	 (token-packer (make-token-packer token-type)))
    (make-lexer-filter :filter filter
		       :token-packer token-packer)))

(defun token-filter--many-any-char (char-list token-type)
  (declare (type list char-list))
  ;; (loop for char in char-list
  ;;    do (assert (equal 'standard-char (type-of char))))
  (let* ((matcher (utils:make-matcher :any-char-pattern char-list))
	 (filter-any (utils:make-filter matcher))
	 (filter (utils:many filter-any))
	 (token-packer (make-token-packer token-type)))
    (make-lexer-filter :filter filter
		       :token-packer token-packer)))

(defun token-filter--function (function token-type)
  (declare (type function function))
  (let ((token-packer (make-token-packer token-type)))
    (make-lexer-filter :filter function
		       :token-packer token-packer)))

(defun token-filter--others (token-type)
  (labels ((others-filter (stream)
	     (let ((next (read-element stream)))
	       (if next
		   (list 'filtered (list next))
		   (list 'not-filtered nil)))))
    (let* ((token-packer (make-token-packer token-type)))
      (make-lexer-filter :filter #'others-filter
			 :token-packer token-packer))))
