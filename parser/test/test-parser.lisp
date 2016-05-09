(in-package :parser-test)

(define-parser-product start
  expression)

;; expression := var tok-assign val
;; var := tok-identifier
;; val := (tok-number | tok-string) 

(let* ((input "aaa=123")
       (token-list (list
		    (make-instance 'token :type 'tok-config-variable :buf "aaa")
		    (make-instance 'token :type 'tok-assign :buf "=")
		    (make-instance 'token :type 'tok-config-value :buf "123")))
       (filter (make-instance 'filter :))
       (parser (make-instance 'parser :streamer token-list)))
  
  )

