(in-package :lexer-test)

;;; deliminator
;;; keyword
;;;	macro		#include, #define, #if
;;;	primitive-type	int, float
;;;	flow-control	for, return
;;; comment
(define-lexer-token-filter (c-language code)
  ('keyword-macro :string-pattern-list '("#include"))
  ('keyword-primitive-type :string-pattern-list '("int" "char"))
  ('keyword-flow-control :string-pattern-list '("return"))

  ('deliminator :single-char-test #'isspace)
  ('l-brace :char-pattern #\{)
  ('r-brace :char-pattern #\})
  ('l-parenthesis :char-pattern #\()
  ('r-parenthesis :char-pattern #\))
  ('l-squarebrace :char-pattern #\[)
  ('r-squarebrace :char-pattern #\])
  ('l-angle :char-pattern #\<)
  ('r-angle :char-pattern #\>)
  ('comma :char-pattern #\,)
  ('colon :char-pattern #\:)
  ('semicolon :char-pattern #\;)
  ('number :list-char-test #'isdigit)
  ('string :list-char-test #'isalpha)
  ('id :list-test #'(lambda (input) (if (and (isalpha (car input))
					     (equal 'full-matched (match (cdr input) :list-char-test #'isalnum)))
					'full-match
					'not-match))))

(define-lexer-token-filter (c-language comment)
  ('keyword-macro :string-pattern-list '("#include"))
  ('keyword-primitive-type :string-pattern-list '("int" "char"))
  ('keyword-flow-control :string-pattern-list '("return"))

  ('deliminator :single-char-test #'isspace)
  ('l-brace :char-pattern #\{)
  ('r-brace :char-pattern #\})
  ('l-parenthesis :char-pattern #\()
  ('r-parenthesis :char-pattern #\))
  ('l-squarebrace :char-pattern #\[)
  ('r-squarebrace :char-pattern #\])
  ('l-angle :char-pattern #\<)
  ('r-angle :char-pattern #\>)
  ('comma :char-pattern #\,)
  ('colon :char-pattern #\:)
  ('semicolon :char-pattern #\;)
  ('number :list-char-test #'isdigit)
  ('string :list-char-test #'isalpha)
  ('id :list-test #'(lambda (input) (if (and (isalpha (car input))
					     (equal 'full-matched (match (cdr input) :list-char-test #'isalnum)))
					'full-match
					'not-match))))
