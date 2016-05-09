(in-package :lexer-test)

(define-lexer-filter config :filter (utils:make-filter
				     (utils:make-matcher :string-pattern "config")) :token-type 'config-group)
(define-lexer-filter sub :filter (utils:make-filter
				  (utils:make-matcher :string-pattern "sub")) :token-type 'config-group)
(define-lexer-filter variable-must :filter (utils:make-filter
					    (utils:make-matcher :string-pattern "&must")) :token-type 'config-variable-limits)
(define-lexer-filter variable-optional :filter (utils:make-filter
						(utils:make-matcher :string-pattern "&optional")) :token-type 'config-variable-limits)
(define-lexer-filter config-variable :filter (utils:make-filter #'utils:matcher-identifier :furthest t))
(define-lexer-filter l-brace :filter (utils:make-filter (utils:make-matcher :char-pattern #\{)))
(define-lexer-filter r-brace :filter (utils:make-filter (utils:make-matcher :char-pattern #\})))
(define-lexer-filter assign :filter (utils:make-filter (utils:make-matcher :char-pattern #\=)))
(define-lexer-filter deliminator :filter (utils:make-filter (utils:make-matcher :list-char-test #'utils:isspace) :furthest t))
(define-lexer-filter config-value :filter (utils:any (utils:make-filter #'utils:matcher-number :furthest t)
						     (utils:make-filter #'utils:matcher-string)))

(let* ((config-input "
config {
	var1 = value1
	sub {
		var2 = 123 &must
		var3 = \"hello\" &optional
	}
}
")
       (composed-filter (utils:any #'config
				   #'sub
				   #'variable-must
				   #'variable-optional
				   #'config-variable
				   #'l-brace
				   #'r-brace
				   #'assign
				   #'deliminator
				   #'config-value))
       (config-lexer (make-instance 'lexer :filter-stack (list composed-filter))))

  #+nil
  (with-input-from-string (s config-input)
    (loop for i below 100
  	 with result
       when (progn
  	      (setq result (funcall composed-filter s))
  	      (not (equal 'not-filtered (car result))))
       do (print (funcall composed-filter s)))
    )
  
  (with-input-from-string (s config-input)
    (setf (lexer-stream config-lexer) s)
    (loop for i below 100
  	 with result
       when (progn
  	      (setq result (lexer-read-token config-lexer))
  	      (not (equal 'not-filtered (car result))))
       do (print (format nil "(~A ~A)" (car result) (token-to-string (cdr result)))))))
