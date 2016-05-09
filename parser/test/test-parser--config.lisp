(in-package :parser-test)

(define-parser-product start
    config-file)

(define-parser-product config-file 
    (many* config-block))

(define-parser-product config-block
    (seq tok-config-group tok-l-brace
	 config-block-body
	 tok-r-brace))

(define-parser-product config-block-body
    (many* (or expression config-block)))

(define-parser-product expression
    (seq variable tok-assign value))

(define-parser-product variable
    tok-config-variable)

(define-parser-product value
    tok-config-value)


"
config {
	var1 = value1
	sub {
		var2 = 123 &must
		var3 = \"hello\" &optional
	}
}
"
(let* ((token-list (list
		    (make-instance 'token :type 'tok-config-group :buf "config")
		    (make-instance 'token :type 'tok-l-brace :buf "{")
		    (make-instance 'token :type 'tok-config-variable :buf "var1")
		    (make-instance 'token :type 'tok-assign :buf "=")
		    (make-instance 'token :type 'tok-config-value :buf "value1")
		    (make-instance 'token :type 'tok-config-group :buf "sub")
		    (make-instance 'token :type 'tok-l-brace :buf "{")
		    (make-instance 'token :type 'tok-config-variable :buf "var2")
		    (make-instance 'token :type 'tok-assign :buf "=")
		    (make-instance 'token :type 'tok-config-value :buf "123")
		    (make-instance 'token :type 'tok-config-variable :buf "var3")
		    (make-instance 'token :type 'tok-assign :buf "=")
		    (make-instance 'token :type 'tok-config-value :buf "\"hello\"")
		    (make-instance 'token :type 'tok-r-brace :buf "}")
		    (make-instance 'token :type 'tok-r-brace :buf "}")))
       (token-stack (reverse token-list))
       (current-token))
  (setq current-token (pop token-stack))
  
  )
