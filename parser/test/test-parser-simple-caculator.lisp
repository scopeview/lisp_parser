(in-package :parser-test)

;;; expr := (number | add-expr | p-expr)
;;; add-expr := expr tok-add expr
;;; p-expr := tok-lp expr tok-rp
;;; number := tok-number
;;;
;;; 1 + ( 2 + 3 ) + 4
(lisp-unit2:define-test parser-test::parser--simple
    (:tags '(parser-test::tag-parser))
  (let* ((tok-number-1 (make-instance 'lexer:token :token-type 'tok-number :token-buf "1"))
	 (tok-number-2 (make-instance 'lexer:token :token-type 'tok-number :token-buf "2"))
	 (tok-number-3 (make-instance 'lexer:token :token-type 'tok-number :token-buf "3"))
	 (tok-number-4 (make-instance 'lexer:token :token-type 'tok-number :token-buf "4"))
	 (tok-add (make-instance 'lexer:token :token-type 'tok-add :token-buf "+"))
	 (tok-lp (make-instance 'lexer:token :token-type 'tok-lp :token-buf "("))
	 (tok-rp (make-instance 'lexer:token :token-type 'tok-rp :token-buf ")"))
	 (token-list (list tok-number-1 tok-add
			   tok-lp tok-number-2 tok-add tok-number-3 tok-rp
			   tok-add tok-number-4 ))
	 ;; 
	 (token-streamer (make-instance 'utils:streamer :source token-list))
	 (parser-streamer (make-instance 'parser-streamer :token-streamer token-streamer))
	 ;; 
	 (single-reducer-number (make-instance 'single-reducer
					       :token-type-reduced 'number
					       :token-type-reduced-from 'tok-number))
	 (seq-reducer-p-expr (make-instance 'seq-reducer
					    :token-type-reduced 'p-expr
					    :token-type-list-reduced-from (list 'tok-lp 'expr 'tok-rp)))
	 (seq-reducer-add-expr (make-instance 'seq-reducer
					      :token-type-reduced 'add-expr
					      :token-type-list-reduced-from (list 'expr 'tok-add 'expr)))
	 (any-reducer-expr (make-instance 'any-reducer
					  :token-type-reduced 'expr
					  :token-type-list-reduced-from (list 'number 'add-expr 'p-expr)))
	 (parser (make-instance 'parser
				:parser-streamer parser-streamer
				:reducer-list (list single-reducer-number
						    seq-reducer-p-expr
						    seq-reducer-add-expr
						    any-reducer-expr)
				:token-type-final 'expression)))
    (destructuring-bind (status token-parsed) (parser-parse parser)
      (lisp-unit2:assert-equal 'parser::full-parsed status)
      ;; (with-slots (lexer::token-type lexer::token-list-reduced-from) token-parsed
      ;; 	(lisp-unit2:assert-equal 'expression lexer::token-type)
      ;; 	(destructuring-bind (tok1 tok2 tok3) lexer::token-list-reduced-from
      ;; 	  (lisp-unit2:assert-true (lexer:token-typep tok1 'var))
      ;; 	  (lisp-unit2:assert-true (lexer:token-typep tok2 'tok-assign))
      ;; 	  (lisp-unit2:assert-true (lexer:token-typep tok3 'val))))
      )))

