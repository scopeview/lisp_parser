(in-package :cl-user)

(require 'lisp-unit2)

(defun load-lexer ()
  (load "~/develop/clisp/utils/utils.asd")
  (require 'utils)
  (load "~/develop/clisp/parser/lexer/lexer/lexer.asd")
  (require 'lexer))

(defun run-lexer-test ()
  (load-lexer)

  (load "~/develop/clisp/parser/lexer/lexer/test/lexer-test.asd")
  (require 'lexer-test)

  ;; (lisp-unit2:run-tests :package :lexer-test
  ;; 			:run-contexts #'lisp-unit2:with-summary-context)
  (lisp-unit2:run-tests :package :lexer-test
  			:run-contexts #'lisp-unit2:with-failure-debugging-context)
)

(load-lexer)
(run-lexer-test)
