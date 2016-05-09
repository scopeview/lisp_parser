(in-package :cl-user)

(defpackage :parser
  (:use cl)
  (:import-from utils
		#:read-element
		#:peek-element
		#:read-tail-element
		#:read-element-all
		#:peek-element-all
		#:unread-element
		#:unread-element-seq
		#:add-tail-element
		#:delete-tail-element
		#:streamer-length)
  (:import-from lexer
		#:token)
  (:export :make-single-token-matcher

	   ;; non-token
	   ;; :non-token

	   ;; parser-streamer
	   :parser-streamer
	   :read-element
	   :unread-element
	   :unread-element-seq

	   ;; 
	   :parser-filterate
	   :make-token-filter
	   :make-parser-filter

	   ;; token-filter
	   :make-token-filter

	   ;; reducer-single/many/any/seq
	   :reducer-reduce
	   :undo
	   :reset
	   :single-reducer
	   ;; :many-reducer
	   ;; :smany
	   ;; :smany*
	   :any-reducer
	   :sany
	   :sany*
	   :seq-reducer
	   :sseq

	   ;; parser
	   :parser
	   :parser-reduce-1
	   :parser-parse
	   ))
