(in-package :parser)

(defclass parser-streamer ()
  ((parsed-token-streamer :initform (make-instance 'utils:streamer))
   (token-streamer :initarg :token-streamer)
   (token-streamer-read-token-stack :initform nil) ;for parser-undo
   ;; (lexer-streamer)
   ))

(set-pprint-dispatch 'parser-streamer
		     #'(lambda (stream structure)
			 (with-slots (parsed-token-streamer token-streamer token-streamer-read-token-stack) structure
			   (funcall (formatter "~@<#<~;~W ~W ~W ~W ~W>~:>") stream 'parser-streamer
				    parsed-token-streamer 'token-streamer-next-token (peek-element token-streamer)
				    token-streamer-read-token-stack))))

(defmethod copy-class ((parser-streamer parser-streamer))
  (let ((new-obj (make-instance 'parser-streamer)))
    (utils:let-with-slots ((old-parsed-token-streamer parsed-token-streamer)
			   (old-token-streamer-read-token-stack token-streamer-read-token-stack)
			   (old-token-streamer token-streamer)) parser-streamer
      (with-slots (parsed-token-streamer token-streamer-read-token-stack token-streamer) new-obj
	(setq parsed-token-streamer old-parsed-token-streamer)
	(setq token-streamer-read-token-stack old-token-streamer-read-token-stack)
	(setq token-streamer old-token-streamer))
      new-obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod read-element ((parser-streamer parser-streamer))
  (with-slots (parsed-token-streamer token-streamer token-streamer-read-token-stack) parser-streamer
    (let (next-token)
      (setq next-token (read-element parsed-token-streamer))
      (unless next-token
	(setq next-token (read-element token-streamer))
	(when next-token
	  ;; (add-tail-element parsed-token-streamer next-token)
	  (push next-token token-streamer-read-token-stack)
	  ;; (setq next-token (read-element parsed-token-streamer))
	  ))
      next-token)))

(defmethod unread-element ((parser-streamer parser-streamer) token)
  (declare (type lexer:token token))
  (with-slots (token-streamer parsed-token-streamer) parser-streamer
    (unread-element parsed-token-streamer token)))

(defmethod unread-element-seq ((parser-streamer parser-streamer) seq)
  (loop for i from (1- (length seq)) downto 0
     do (unread-element parser-streamer (elt seq i))))

(defmethod unread-element-token ((parser-streamer parser-streamer) token &key type)
  (ecase type
    ('to-reduced-from (with-slots (lexer::token-list-reduced-from) token
			(if lexer::token-list-reduced-from
			    (unread-element-seq parser-streamer lexer::token-list-reduced-from)
			    (unread-element parser-streamer token))))
    ('to-all-orignal (with-slots (lexer::token-list-reduced-from) token
		       (if lexer::token-list-reduced-from
			   (mapcar #'(lambda (token)
				       (unread-element-token parser-streamer token :type 'to-all-orignal))
				   (reverse lexer::token-list-reduced-from))
			   (unread-element parser-streamer token))))))

(defmethod peek-element ((parser-streamer parser-streamer))
  (with-slots (parsed-token-streamer token-streamer) parser-streamer
    (if (zerop (streamer-length parsed-token-streamer))
	(peek-element token-streamer)
	(peek-element parsed-token-streamer))))

(defmethod parser-streamer-undo ((parser-streamer parser-streamer) &key type)
  (ecase type
    ('parsed-token-streamer
     (with-slots (parsed-token-streamer token-streamer token-streamer-read-token-stack) parser-streamer
       (let* ((token-list (read-element-all parsed-token-streamer))
	      (orignal-token-list (lexer:token-orignal-token-list-from-list token-list)))
	 (unread-element-seq token-streamer orignal-token-list)
	 ;; token-streamer-read-token-stack
	 ;; TODO
	 ;; (let ((length-1 (length token-streamer-read-token-stack))
	 ;;       (length-2 (length orignal-token-list)))
	 ;;   (assert (equal (reverse (subseq token-streamer-read-token-stack 0 length-2)) orignal-token-list))
	 ;;   (setq token-streamer-read-token-stack (subseq token-streamer-read-token-stack length-2 length-1)))
	 )))
    ('all
     (with-slots (token-streamer token-streamer-read-token-stack) parser-streamer
       (loop for token in token-streamer-read-token-stack
	  do (unread-element token-streamer token))))))
