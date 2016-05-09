(in-package :lexer-test)

(lisp-unit2:define-test lexer-test::token-streamer-1
    (:tags '(lexer-test::tag-token-streamer))
  (let* ((token-1 (make-instance 'token :token-type 'tok-l-brace :token-buf (utils:string-to-list "{")))
	 (token-2 (make-instance 'token :token-type 'tok-number :token-buf (utils:string-to-list "123")))
	 (token-3 (make-instance 'token :token-type 'tok-r-brace :token-buf (utils:string-to-list "}")))
	 (token-4 (make-instance 'token :token-type 'tok-string :token-buf (utils:string-to-list "abc")))
	 (token-list (list token-1 token-2 token-3))
	 (ts (make-instance 'token-streamer :source token-list)))
    ;; read-element
    (lisp-unit2:assert-equal token-1 (read-element ts))
    (lisp-unit2:assert-equal token-2 (read-element ts))
    (lisp-unit2:assert-equal token-3 (read-element ts))
    (lisp-unit2:assert-equal nil (read-element ts))

    ;; unread-element
    (unread-element ts token-3)
    (unread-element ts token-2)
    (unread-element ts token-1)
    (lisp-unit2:assert-equal token-1 (read-element ts))
    (lisp-unit2:assert-equal token-2 (read-element ts))
    (lisp-unit2:assert-equal token-3 (read-element ts))

    ;; unread-element-seq
    (unread-element-seq ts token-list)
    (lisp-unit2:assert-equal token-1 (read-element ts))
    (lisp-unit2:assert-equal token-2 (read-element ts))
    (lisp-unit2:assert-equal token-3 (read-element ts))

    ;; add-tail-element
    (add-tail-element ts token-4)
    (lisp-unit2:assert-equal token-4 (read-element ts))
    (lisp-unit2:assert-equal nil (read-element ts))

    ;; error
    ))
