(in-package :lexer)

(defun lexer-filterate (stream filter token-packer)
  (declare (type function filter)
	   (type function token-packer))
  (destructuring-bind (status content) (funcall filter stream)
    (ecase status
      ('utils::not-filtered (list 'not-filtered nil))
      ('utils::filtered (list 'filtered (funcall token-packer content)))
      ('utils::eof (list 'eof nil))

      ('not-filtered (list 'not-filtered nil))
      ('filtered (list 'filtered (funcall token-packer content)))
      ('eof (list 'eof nil)))))
