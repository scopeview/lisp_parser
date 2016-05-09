(in-package :cl-user)

(defpackage :lexer
  (:use cl)
  (:import-from utils
		#:read-element
		#:peek-element
		#:unread-element
		#:unread-element-seq
		#:add-tail-element
		#:delete-tail-element
		#:streamer-length)
  (:export :lexer-filterate
	   :make-lexer-filter
	   :define-lexer-filter
	   :token-filter

	   ;; filter-group
	   :add-filter

	   ;; token
	   :token
	   :token-type-equal
	   :token-typep
	   :token-to-string
	   :make-lexer-token-packer-default
	   :token-orignal-token-list
	   :token-orignal-token-list-from-list

	   ;; lexer
	   :lexer
	   :lexer-read-token
	   :lexer-unread-token

	   ;; generic streamer interface
	   ;; token-streamer, lexer-streamer
	   :token-streamer
	   :lexer-streamer
	   :read-element
	   :unread-element
	   :unread-element-seq
	   :add-tail-element
	   ))
