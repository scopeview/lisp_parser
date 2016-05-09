(in-package :lexer)

(defclass lexer-streamer ()
  ((parsed-token-streamer :initform (make-instance 'utils:streamer))
   (lexer :initarg :lexer :type lexer)))

(defmethod read-element ((lexer-streamer lexer-streamer))
  (with-slots (parsed-token-streamer lexer) lexer-streamer
    (let (next-token)
      (setq next-token (read-element parsed-token-streamer))
      (unless next-token
	(setq next-token (lexer-read-token lexer)))
      next-token)))

(defmethod unread-element ((lexer-streamer lexer-streamer) token)
  (declare (type lexer:token token))
  (with-slots (token-streamer parsed-token-streamer) lexer-streamer
    (unread-element parsed-token-streamer token)))

(defmethod unread-element-seq ((lexer-streamer lexer-streamer) seq)
  (loop for i from (1- (length seq)) downto 0
     do (unread-element lexer-streamer (elt seq i))))

(defmethod peek-element ((lexer-streamer lexer-streamer))
  (with-slots (parsed-token-streamer lexer) lexer-streamer
    (if (zerop (streamer-length parsed-token-streamer))
	(progn
	  (let ((token (lexer-read-token lexer)))
	    (when token
	      (unread-element parsed-token-streamer token))
	    token))
	(peek-element parsed-token-streamer))))
