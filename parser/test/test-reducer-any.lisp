(in-package :parser-test)

(lisp-unit2:define-test parser-test::any-reducer-1
    (:tags '(parser-test::tag-reducer))
  (let* ((non-value (make-instance 'lexer:token
				   :token-type 'non-value))
	 (tok-string (make-instance 'lexer:token :token-type 'tok-assign :token-buf "abc"))
	 (tok-number (make-instance 'lexer:token :token-type 'tok-number :token-buf "123"))
	 (any-reducer (sany 'non-value 'tok-assign 'tok-number))
	 (token-list (list tok-string tok-number))
	 (token-streamer (make-instance 'lexer:token-streamer :source token-list))
	 (parser-streamer (make-instance 'parser-streamer :token-streamer token-streamer)))
    ;; reduce
    (destructuring-bind (status token-reduced) (reducer-reduce any-reducer parser-streamer)
      (lisp-unit2:assert-equal 'parser::full-reduced status)
      (with-slots (lexer::token-type lexer::token-list-reduced-from) token-reduced
	(lisp-unit2:assert-equal 'non-value lexer::token-type)
	(lisp-unit2:assert-equal (list tok-string) lexer::token-list-reduced-from)))))

