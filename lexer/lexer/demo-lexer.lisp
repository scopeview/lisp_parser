(in-package :lexer)

(let ((c-code (make-instance 'filter-group :filter-group-name 'c-code))
      (c-comment (make-instance 'filter-group :filter-group-name 'c-comment))
      (lexer)
      (token)
      (input-string " int i = 1;")
      (streamer)
      (expected-token-type-list '(int string assign string semicomma))
      (token-stack))
  (add-filter c-code (token-filter :string "int" :token-type 'int))
  (add-filter c-code (token-filter :string "=" :token-type 'assign))
  (add-filter c-code (token-filter :string ";" :token-type 'semicomma))
  (add-filter c-code (token-filter :many-any-char (list #\space)))
  (add-filter c-code (token-filter :function (utils:many
  					      (utils:make-filter (utils:make-matcher :single-char-test #'alphanumericp)))
				   :token-type 'string))
  (add-filter c-code (token-filter :others t))
  (with-input-from-string (s input-string)
    (setq streamer (make-instance 'utils:cstreamer :source s))
    (setq lexer (make-instance 'lexer
			       :stream streamer
			       :filter-group-list (list c-code c-comment)
			       :filter-group-state-stack (list 'c-code)))
    (loop
       with token
       do (progn
    	    (setq token (lexer-read-token lexer))
    	    (if token
		(push token token-stack)
		(return))))
    (mapcar #'(lambda (token token-type)
    		(assert (token-typep token token-type))) (reverse token-stack) expected-token-type-list)))
