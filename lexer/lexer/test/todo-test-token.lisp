(in-package :lexer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; test (simple)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq config-text "
config {
var = value
sub {
var1 = value1
var2 = value2
}
}
")

(define-lexer-token 'config 'deliminator (many #'space-f))
(define-lexer-token 'config 'l-brace #\{)
(define-lexer-token 'config 'r-brace #\})
(define-lexer-token 'config 'assign #\=)
(define-lexer-token 'config 'var (seq #'alphanumeric-f
				      (many* #'alphanumeric-f)))
(define-lexer-token 'config 'value (many #'alphanumeric-f))


(with-input-from-string (s config-text)
  (with-lexer-token-stream ('config ts s)
    (loop for i below 100
       with result
       while (progn
	       (setq result (read-token ts))
	       (equal 'filtered (car result)))
       do (print (coerce (cdr result) 'string)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; test (complex)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-lexer-token 'code 'keyword "#include")
(define-lexer-token 'code 'keyword "int")
(define-lexer-token 'code 'keyword "char")
(define-lexer-token 'code 'keyword "return")

(define-lexer-token 'code 'operator "*")

(define-lexer-token 'code 'l-parenthesis #\()
(define-lexer-token 'code 'r-parenthesis #\))
(define-lexer-token 'code 'l-abrace #\<)
(define-lexer-token 'code 'r-abrace #\>)
(define-lexer-token 'code 'l-brace #\{)
(define-lexer-token 'code 'r-brace #\})

(define-lexer-token 'code 'deliminator (many #'space-f))
(define-lexer-token 'code 'deliminator #\;)
(define-lexer-token 'code 'deliminator #\,)

(define-lexer-token 'code 'var (seq #'alphanumeric-f
				      (many* #'alphanumeric-f)))
(define-lexer-token 'code 'value (many #'alphanumeric-f))

(with-input-from-string (s "#include <string> int main(int argc, char** argv) {return 0;}")
  (with-lexer-token-stream ('code ts s)
    (loop for i below 100
       with result
       while (progn
    	       (setq result (read-token ts))
    	       (equal 'filtered (car result)))
       do (print (coerce (cdr result) 'string)))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-token-filter (deliminator-list))

(with-input-from-string (s "#include <string> int main(int argc, char** argv) {return 0;}")
  (let* ((matcher-1 (make-matcher :list-char-test #'isspace))
	 (language-deliminator-list (string-to-list "<>{}[];,#"))
	 (matcher-2 (make-matcher :single-char-test #'(lambda (char) (member char language-deliminator-list))))
	 (matcher-3 (make-matcher :list-char-test #'isalnum))
	 (filter-1 (make-filter matcher-1))
	 (filter-2 (make-filter matcher-2))
	 (filter-3 (make-filter matcher-3 :furthest t))
	 (composed-filter (any (seq filter-1 filter-2)
			       (seq filter-1 filter-3)
			       (seq filter-3 filter-2)
			       (seq filter-2 filter-3))))
    (funcall composed-filter s)
    (funcall composed-filter s)
    (funcall composed-filter s)))
