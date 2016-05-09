(in-package :cl-user)

(require 'lisp-unit2)
(require 'log4cl)

(defun load-parser ()
  (load "~/dd/computer/github/lisp_utils/utils.asd")
  (require 'utils)
  (load "~/dd/computer/develop/clisp/parser/lexer/lexer/lexer.asd")
  (require 'lexer)
  (load "~/dd/computer/develop/clisp/parser/parser/parser.asd")
  (require 'parser))

(defun run-parser-test ()
  (load-parser)
  
  (lisp-unit2:reset-test-database)

  (load "~/dd/computer/develop/clisp/parser/parser/test/parser-test.asd")
  (require 'parser-test)

  ;; (lisp-unit2:run-tests :package :parser-test
  ;; 			:run-contexts #'lisp-unit2:with-summary-context)
  (lisp-unit2:run-tests :package :parser-test
  			:run-contexts #'lisp-unit2:with-failure-debugging-context)
  )

(load-parser)
(run-parser-test)
