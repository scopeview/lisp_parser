(in-package :lexer)

(lisp-unit2:define-test lexer-test::token--token-orignal-token-list
    (:tags '(lexer-test::tag-token))
  (let* ((token-1 (make-instance 'token :token-type 'tok-string :token-buf (utils:string-to-list "abc")))
	 (token-2 (make-instance 'token :token-type 'tok-assign :token-buf (utils:string-to-list "=")))
	 (token-3 (make-instance 'token :token-type 'tok-l-brace :token-buf (utils:string-to-list "{")))
	 (token-4 (make-instance 'token :token-type 'tok-number :token-buf (utils:string-to-list "123")))
	 (token-5 (make-instance 'token :token-type 'tok-r-brace :token-buf (utils:string-to-list "}")))
	 (token-var (make-instance 'token :token-type 'var :token-list-reduced-from (list token-1)))
	 (token-val (make-instance 'token :token-type 'val :token-list-reduced-from (list token-3 token-4 token-5)))
	 (token-expr (make-instance 'token :token-type 'expr :token-list-reduced-from (list token-var token-2 token-val)))
	 (expected-token-list (list token-1 token-2 token-3 token-4 token-5)))
    (lisp-unit2:assert-equal expected-token-list (token-orignal-token-list token-expr))))
