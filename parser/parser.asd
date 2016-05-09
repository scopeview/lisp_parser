(asdf:defsystem :parser
  :version "0.0.1"
  :serial t
  :components ((:file "package")
	       (:file "log")
	       (:file "parser-streamer" :depends-on ("log"))
	       (:file "matcher")
	       (:file "token-filter")
	       (:file "reducer-single")
	       ;; (:file "reducer-many")
	       (:file "reducer-any")
	       (:file "reducer-seq")
	       (:file "shifter" :depends-on ("log"))
	       (:file "parser" :depends-on ("log"))
	       )
  :depends-on ("utils" "lexer" "log4cl")
  )
