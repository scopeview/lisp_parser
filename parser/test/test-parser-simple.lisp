(in-package :parser-test)

;;; expression := var tok-assign val
;;; val := tok-string
;;; var := tok-string
(lisp-unit2:define-test parser-test::parser--simple
    (:tags '(parser-test::tag-parser))
  (let* ((tok-string-1 (make-instance 'lexer:token :token-type 'tok-string :token-buf "abc"))
	 (tok-assign (make-instance 'lexer:token :token-type 'tok-assign :token-buf "="))
	 (tok-string-2 (make-instance 'lexer:token :token-type 'tok-string :token-buf "123"))
	 (token-list (list tok-string-1 tok-assign tok-string-2))
	 (token-streamer (make-instance 'utils:streamer :source token-list))
	 (parser-streamer (make-instance 'parser-streamer :token-streamer token-streamer))

	 (single-reducer-var (make-instance 'single-reducer
					    :token-type-reduced 'var
					    :token-type-reduced-from 'tok-string))
	 (single-reducer-val (make-instance 'single-reducer
					    :token-type-reduced 'val
					    :token-type-reduced-from 'tok-string))
	 (seq-reducer (make-instance 'seq-reducer
				     :token-type-reduced 'expression
				     :token-type-list-reduced-from (list 'var 'tok-assign 'val)))
	 (reducer-list (list seq-reducer
			     single-reducer-val
			     single-reducer-var))
	 (parser (make-instance 'parser
				:parser-streamer parser-streamer
				:reducer-list reducer-list
				:token-type-final 'expression)))
    (destructuring-bind (status token-parsed) (parser-parse parser)
      (lisp-unit2:assert-equal 'parser::full-parsed status)
      (with-slots (lexer::token-type lexer::token-list-reduced-from) token-parsed
	(lisp-unit2:assert-equal 'expression lexer::token-type)
	(destructuring-bind (tok1 tok2 tok3) lexer::token-list-reduced-from
	  (lisp-unit2:assert-true (lexer:token-typep tok1 'var))
	  (lisp-unit2:assert-true (lexer:token-typep tok2 'tok-assign))
	  (lisp-unit2:assert-true (lexer:token-typep tok3 'val)))))))

