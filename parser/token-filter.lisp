(in-package :parser)

(defun make-token-filter (token-type)
  (utils:make-filter (make-single-token-matcher token-type)))
