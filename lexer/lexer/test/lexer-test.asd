(asdf:defsystem :lexer-test
  :version "0.0.1"
  :serial t
  :components ((:file "package")
	       (:file "test-token")
	       (:file "test-token-streamer"))
  :depends-on ("utils" "lexer")
  )
