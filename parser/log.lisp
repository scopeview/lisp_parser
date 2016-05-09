(in-package :parser)

(defvar *parser-logger*)
(setq *parser-logger* (log:category '(parser)))
;; (log:config *parser-logger* :daily "/tmp/parser.log" :debug :file)
(log:config *parser-logger* :console :sane)

(defmacro define-log-function (name1 name2)
  (let ((function-name (intern (format nil "~a-~a" (string `,name1) (string `,name2))))
	(log-function-name (read-from-string (format nil "~a:~a" (string `,name1) (string `,name2)))))
    `(defmacro ,function-name (&rest args)
       `(,',log-function-name :logger *parser-logger* ,@args))))

(define-log-function log info)
(define-log-function log warn)
(define-log-function log error)
(define-log-function log fatal)
(define-log-function log debug)
(define-log-function log d1)
(define-log-function log d2)
(define-log-function log d3)
(define-log-function log d4)
(define-log-function log d5)
(define-log-function log d6)
(define-log-function log d7)
(define-log-function log d8)
(define-log-function log d9)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *parser-logger-brief*)
(setq *parser-logger-brief* (log:category '(parser-brief)))
(log:config *parser-logger-brief* :daily "/tmp/parser-brief.log" :debug :file)

(defmacro log-debug-b (&rest args)
  `(log:debug :logger *parser-logger-brief* ,@args))



