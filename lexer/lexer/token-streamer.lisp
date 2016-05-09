(in-package :lexer)

(defclass token-streamer ()
  ((source :initarg :source :initform nil)
   (read-stack :initform nil)))

(defmethod read-element ((s token-streamer))
  (with-slots (source read-stack) s
    (let ((next-token (car source)))
      (when next-token
	(setq source (cdr source))
	(push next-token read-stack))
      next-token)))

(defmethod peek-element ((s token-streamer))
  (with-slots (source) s
    (car source)))

(defmethod unread-element ((s token-streamer) token)
  (declare (type token token))
  (with-slots (source read-stack) s
    (let ((last-read-token (car read-stack)))
      (unless (equal last-read-token token)
	(error (format nil "~A is not the last read token ~A" token last-read-token)))
      (push token source)
      (pop read-stack))))

(defmethod unread-element-seq ((s token-streamer) seq)
  (loop for i from (1- (length seq)) downto 0
     do (unread-element s (elt seq i))))

(defmethod add-tail-element ((s token-streamer) token)
  (declare (type token token))
  (with-slots (source) s
    (setq source (concatenate 'list source (list token)))))
