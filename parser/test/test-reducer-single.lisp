(in-package :parser-test)

(lisp-unit2:define-test parser-test::single-reducer-1
    (:tags '(parser-test::tag-reducer))
  (let* ((tok-string (make-instance 'lexer:token :token-type 'tok-string :token-buf "abc"))
	 (token-list (list tok-string))
	 (single-reducer (make-instance 'single-reducer
					:token-type-reduced 'non-identifier
					:token-type-reduced-from 'tok-string))
	 (token-streamer (make-instance 'lexer:token-streamer :source token-list))
	 (parser-streamer (make-instance 'parser-streamer :token-streamer token-streamer)))
    ;; reduce
    (destructuring-bind (status reduced-token) (reducer-reduce single-reducer parser-streamer)
      (lisp-unit2:assert-equal 'parser::full-reduced status)
      (with-slots (lexer::token-type) reduced-token
	(lisp-unit2:assert-equal 'non-identifier lexer::token-type)))))
