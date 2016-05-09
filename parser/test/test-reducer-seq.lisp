(in-package :parser-test)

(lisp-unit2:define-test parser-test::seq-reducer
    (:tags '(parser-test::tag-reducer))
  (let* ((tok-identifier (make-instance 'lexer:token :token-type 'tok-identifier :token-buf "a"))
	 (tok-assign (make-instance 'lexer:token :token-type 'tok-assign :token-buf "="))
	 (tok-number (make-instance 'lexer:token :token-type 'tok-number :token-buf "123"))
	 (expected-token-type-reduced 'non-assign-expression)
	 (token-list (list tok-identifier tok-assign tok-number))
	 (seq-reducer (make-instance 'seq-reducer :token-type-reduced expected-token-type-reduced
				     :token-type-list-reduced-from (list 'tok-identifier 'tok-assign 'tok-number)))
	 (token-streamer (make-instance 'lexer:token-streamer :source token-list))
	 (parser-streamer (make-instance 'parser-streamer :token-streamer token-streamer)))
    (destructuring-bind (status reduced-token) (reducer-reduce seq-reducer parser-streamer)
      (lisp-unit2:assert-equal 'parser::full-reduced status)
      (with-slots (lexer::token-type) reduced-token
	(lisp-unit2:assert-equal expected-token-type-reduced lexer::token-type)
	(lisp-unit2:assert-equal token-list-reduced-from token-list)))))

(lisp-unit2:define-test parser-test::seq-reducer-fail
    (:tags '(parser-test::tag-reducer))
  (let* ((tok-identifier (make-instance 'lexer:token :token-type 'tok-identifier :token-buf "a"))
	 (tok-assign (make-instance 'lexer:token :token-type 'tok-assign :token-buf "="))
	 (tok-number (make-instance 'lexer:token :token-type 'tok-number :token-buf "123"))
	 (expected-token-type-reduced 'non-assign-expression)
	 (token-list (list tok-identifier tok-assign))
	 (seq-reducer (make-instance 'seq-reducer :token-type-reduced expected-token-type-reduced
				     :token-type-list-reduced-from (list 'tok-identifier 'tok-assign 'tok-number)))
	 (token-streamer (make-instance 'lexer:token-streamer :source token-list))
	 (parser-streamer (make-instance 'parser-streamer :token-streamer token-streamer)))
    (destructuring-bind (status reduced-token) (reducer-reduce seq-reducer parser-streamer)
      (lisp-unit2:assert-equal 'parser::partial-reduced status)
      (lisp-unit2:assert-equal (list tok-identifier tok-assign) reduced-token))
    (lisp-unit2:assert-equal nil (read-element parser-streamer))))
