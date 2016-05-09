(in-package :parser)

(defun istoken (token &key token-type)
  (declare (type lexer:token token))
  (with-slots (lexer::token-type) token
    (equal token-type lexer::token-type)))

(defun make-single-token-matcher (token-type)
  (labels ((token-match (input)
	     (let ((length-input (length input))
		   (first-token (car input))
		   (is-first-token-match))
	       (assert (<= 1 length-input))
	       (setq is-first-token-match (istoken first-token :token-type token-type))
	       (cond
		 ((and is-first-token-match
		       (= 1 length-input)) 'utils::full-match)
		 ((and is-first-token-match
		       (< 1 length-input) 'utils::over-match))
		 (t 'utils::not-match)))))
    (utils:make-matcher :list-test #'token-match)))
