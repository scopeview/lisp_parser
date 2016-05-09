(asdf:defsystem :parser-test
  :version "0.0.1"
  :serial t
  :components ((:file "package")
	       (:file "test-reducer-single")
	       (:file "test-reducer-any")
	       (:file "test-reducer-seq")
	       (:file "test-parser-simple")
	       )
  :depends-on ("utils" "parser")
  )
