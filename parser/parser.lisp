(in-package :parser)

(defvar *parser-id* 0)
(defvar *shifter-id* 0)
(setq *parser-id* 0)
(setq *shifter-id* 0)
(defun get-parser-shifter-id (&key type)
  (ecase type
    ('parser (incf *parser-id*))
    ('shifter (incf *shifter-id*))))

(defclass parser ()
  ((parser-streamer :initarg :parser-streamer)
   (reducer-list :initarg :reducer-list)
   (token-type-final :initarg :token-type-final)
   (token-reduced-or-shifted :initform nil)
   (final-token-checker-policy :initarg :final-token-checker-policy :initform 'parse-until-eof)
   (self-parsed-policy :initarg :self-parsed-policy :initform 'allow-self-parsed)
   (shifter-stack :initform nil)
   (reducer-index-stack :initarg :reducer-index-stack :initform '(0))
   (ignore-final-parsed :initform nil)
   (parser-status :initform nil)
   ;; debug
   (parser-id :initform (get-parser-shifter-id :type 'parser))
   (parser-shifter-level :initarg :parser-shifter-level :initform 0)
   (last-step-status :initform nil)
   (last-step-handle-result :initform nil)))

(set-pprint-dispatch 'parser
		     #'(lambda (stream structure)
			 (with-slots (parser-streamer token-type-final reducer-index-stack
						      final-token-checker-policy) structure
			   (funcall (formatter "~@<#<~;~W ~W ~W ~W ~W>~:>") stream 'parser
				    parser-streamer token-type-final reducer-index-stack
				    final-token-checker-policy))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod parser-current-reducer-type ((parser parser))
  (with-slots (reducer-list reducer-index-stack) parser
    (let ((reducer-index (car reducer-index-stack)))
      (if (or (null reducer-index)
	      (equal (length reducer-list) reducer-index))
	  'NULL
	  (type-of (elt reducer-list reducer-index))))))

(defmethod parser-is-current-reducer-seq-reducer ((parser parser))
  (equal 'seq-reducer (parser-current-reducer-type parser)))

(defmethod parser-is-next-token-first-token-of-seq-reducer ((parser parser))
  (assert (parser-is-current-reducer-seq-reducer parser))
  (with-slots (reducer-list reducer-index-stack parser-streamer) parser
    (let ((seq-reducer (elt reducer-list (car reducer-index-stack)))
	  (next-token (peek-element parser-streamer)))
      (with-slots (token-type-list-reduced-from) seq-reducer
	(when next-token
	  (log-debug "parser-is-next-token-first-token-of-seq-reducer" next-token (car token-type-list-reduced-from))
	  (lexer:token-typep next-token (car token-type-list-reduced-from)))))))

(defmethod parser-is-current-reducer-end ((parser parser))
  (with-slots (reducer-list reducer-index-stack) parser
    (and reducer-index-stack
	 (equal (length reducer-list) (car reducer-index-stack)))))

(defmethod parser-is-reducer-index-stack-empty ((parser parser))
  (with-slots (reducer-index-stack) parser
    (not reducer-index-stack)))

(defmethod parser-is-current-shifter-active ((parser parser))
  (with-slots (shifter-stack) parser
    (and shifter-stack
	 (shifter-activep (car shifter-stack)))))

(defmethod parser-has-token-reduced-or-shifted ((parser parser))
  (with-slots (token-reduced-or-shifted) parser
    token-reduced-or-shifted))

(defmethod parser-is-next-token-final-token ((parser parser))
  (with-slots (token-type-final parser-streamer) parser
    (let ((next-token (peek-element parser-streamer)))
      (when next-token
	(lexer:token-typep next-token token-type-final)))))

(defmethod parser-is-token-reduced-or-shifted-final-token ((parser parser))
  (with-slots (token-type-final token-reduced-or-shifted) parser
    (when token-reduced-or-shifted
      (lexer:token-typep token-reduced-or-shifted token-type-final))))

(defmethod parser-is-ignore-final-parsed ((parser parser))
  (with-slots (ignore-final-parsed) parser
    ignore-final-parsed))

(defmethod parser-set-ignore-final-parsed ((parser parser))
  (with-slots (ignore-final-parsed) parser
    (setq ignore-final-parsed t)))

(defmethod parser-clear-ignore-final-parsed ((parser parser))
  (with-slots (ignore-final-parsed) parser
    (setq ignore-final-parsed nil)))

(defmethod parser-set-reducer-index ((parser parser) &key type)
  (with-slots (reducer-list reducer-index-stack) parser
    (assert reducer-index-stack)
    (assert (not (equal (length reducer-list) (car reducer-index-stack))))
    (ecase type
      ('not-shifted (progn
		      (assert (parser-is-current-reducer-seq-reducer parser))
		      (incf (car reducer-index-stack))))
      ('full-shifted (progn
		       (assert (parser-is-current-reducer-seq-reducer parser))
		       (incf (car reducer-index-stack))
		       (push 0 reducer-index-stack)))
      ('not-reduced (progn
		      (assert (not (parser-is-current-reducer-seq-reducer parser)))
		      (incf (car reducer-index-stack))))
      ('full-reduced (progn
		       (assert (not (parser-is-current-reducer-seq-reducer parser)))
		       (incf (car reducer-index-stack))
		       (push 0 reducer-index-stack)))
      ('ignore-shifter (progn
			 (assert (parser-is-current-reducer-seq-reducer parser))
			 (incf (car reducer-index-stack)))))))

(defmethod parser-status-p ((parser parser) status)
  (with-slots (parser-status) parser
    (equal status parser-status)))

(defmethod parser-set-status ((parser parser) status)
  (with-slots (parser-status) parser
    (setq parser-status status)))

(defmethod parser-is-current-token-shifted-by-current-shifter ((parser parser))
  (with-slots (shifter-stack parser-streamer) parser
    (let ((shifter (car shifter-stack)))
      (with-slots (shift-reducer) shifter
	(with-slots (token-type-reduced) shift-reducer
	  (let ((current-token (peek-element parser-streamer)))
	    (lexer:token-typep current-token token-type-reduced)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod parser-check-create-shifter ((parser parser))
  (assert (not (parser-is-current-shifter-active parser)))
  (assert (parser-is-current-reducer-seq-reducer parser))
  (if (parser-is-next-token-first-token-of-seq-reducer parser)
      (progn
	(parser-create-shifter-internal parser)
	'shifter-created)
      (progn
	(parser-set-reducer-index parser :type 'ignore-shifter)
	'shifter-neednot-created)))

(defmethod parser-create-shifter-internal ((parser parser))
  (with-slots (reducer-list reducer-index-stack shifter-stack parser-shifter-level) parser
    (let ((reducer-index (car reducer-index-stack)))
      (push (make-instance 'shifter
			   :reducer-index reducer-index
			   :is-active t
			   :reducer-index-stack-length (length reducer-index-stack)
			   :shift-reducer (elt reducer-list reducer-index)
			   :parent-parser parser
			   :parser-shifter-level (+ 1 parser-shifter-level))
	    shifter-stack))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod parser-shift ((parser parser))
  (assert (parser-is-current-shifter-active parser))
  (assert (parser-is-current-reducer-seq-reducer parser))
  (assert (not (parser-has-token-reduced-or-shifted parser)))
  (parser-shift-internal parser))

(defmethod parser-shift-internal ((parser parser))
  (with-slots (shifter-stack token-reduced-or-shifted) parser
    (destructuring-bind (status token-shifted) (shifter-shift (car shifter-stack))
      (ecase status
	('not-shifted nil)
	('full-shifted (setq token-reduced-or-shifted token-shifted)))
      status)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod parser-reduce ((parser parser))
  (assert (not (parser-is-current-reducer-seq-reducer parser)))
  (assert (not (parser-is-current-shifter-active parser)))
  (assert (not (parser-has-token-reduced-or-shifted parser)))
  (parser-reduce-internal parser))

(defmethod parser-reduce-internal ((parser parser))
  (with-slots (parser-streamer reducer-list reducer-index-stack token-reduced-or-shifted) parser
    (destructuring-bind (status token-reduced) (reducer-reduce (elt reducer-list (car reducer-index-stack)) parser-streamer)
      (ecase status
	('not-reduced nil)
	('full-reduced (setq token-reduced-or-shifted token-reduced)))
      status)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod parser-undo ((parser parser) &key type)
  (ecase type
    ('all (parser-undo-all parser))
    ('parsed-token-streamer (parser-undo-parsed-token-streamer parser))
    ('current-reduced (parser-undo-current-reduced parser))))

(defmethod parser-undo-all ((parser parser))
  (assert (parser-is-reducer-index-stack-empty parser))
  (log-debug "parser-undo-all" parser)
  (with-slots (parser-streamer) parser
    (parser-streamer-undo parser-streamer :type 'all)))

(defmethod parser-undo-parsed-token-streamer ((parser parser))
  (assert (parser-is-reducer-index-stack-empty parser))
  (log-debug "parser-undo-parsed-token-streamer" parser)
  (with-slots (parser-streamer) parser
    (parser-streamer-undo parser-streamer :type 'parsed-token-streamer)))

(defmethod parser-undo-current-reduced ((parser parser))
  (assert (not (parser-is-current-reducer-seq-reducer parser)))
  ;; TODO last-step-status should be full-reduced
  (with-slots (reducer-index-stack parser-streamer) parser
    (let ((token-reduced (read-element parser-streamer)))
      (assert token-reduced)
      (log-debug "parser-undo-current-reduced " token-reduced)
      (unread-element-token parser-streamer token-reduced :type 'to-reduced-from))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod parser-ignore ((parser parser) &key type)
  (log-debug "parser-ignore " type)
  (ecase type
    ('last-one-last-layer (parser-ignore-last-one-last-layer parser))
    ('last-one-current-layer (parser-ignore-last-one-current-layer parser))
    ('current-one (parser-ignore-current-one parser))
    ('current-reduced (parser-ignore-current-reduced parser))
    ('current-shifted (parser-ignore-current-shifted parser))
    ('final-parsed (parser-ignore-final-parsed parser))
    ('final-parsed-as-reduced (parser-ignore-final-parsed-as-reduced parser))
    ('final-parsed-as-shifted (parser-ignore-final-parsed-as-shifted parser))))

(defmethod parser-ignore-last-one-last-layer ((parser parser))
  (assert (parser-is-current-reducer-end parser))
  (with-slots (reducer-index-stack) parser
    (pop reducer-index-stack)
    (if reducer-index-stack
	(parser-ignore parser :type 'last-one-current-layer)
	'ignore-nothing)))

(defmethod parser-ignore-last-one-current-layer ((parser parser))
  (with-slots (reducer-index-stack) parser
    (assert reducer-index-stack)
    (assert (not (zerop (car reducer-index-stack))))
    (decf (car reducer-index-stack))
    (parser-ignore parser :type 'current-one)))

(defmethod parser-ignore-current-one ((parser parser))
  (if (parser-is-current-reducer-seq-reducer parser)
      (parser-ignore parser :type 'current-shifted)
      (parser-ignore parser :type 'current-reduced)))

(defmethod parser-ignore-current-shifted ((parser parser))
  (assert (parser-is-current-reducer-seq-reducer parser))
  (assert (not (parser-is-current-shifter-active parser)))
  (assert (parser-is-current-token-shifted-by-current-shifter parser))
  (with-slots (reducer-index-stack shifter-stack) parser
    (assert shifter-stack)
    (let ((shifter (car shifter-stack)))
      (with-slots (reducer-index-stack-length) shifter
	(assert (equal reducer-index-stack-length (length reducer-index-stack)))
	(shifter-set-ignore-final-shifted shifter t)
	(shifter-activate shifter))))
  'shifted-ignored)

(defmethod parser-ignore-current-reduced ((parser parser))
  (assert (not (parser-is-current-reducer-seq-reducer parser)))
  (parser-undo parser :type 'current-reduced)
  (with-slots (reducer-index-stack) parser
    (incf (car reducer-index-stack)))
  'reduced-ignored)

(defmethod parser-ignore-final-parsed ((parser parser))
  (if (parser-is-current-reducer-seq-reducer parser)
      (parser-ignore parser :type 'final-parsed-as-shifted)
      (parser-ignore parser :type 'final-parsed-as-reduced))
  (parser-clear-ignore-final-parsed parser)
  'final-parsed-ignored)

(defmethod parser-ignore-final-parsed-as-reduced ((parser parser))
  (assert (not (parser-is-current-reducer-seq-reducer parser)))
  (assert (not (parser-is-current-shifter-active parser)))
  (with-slots (reducer-index-stack self-parsed-policy) parser
    (if reducer-index-stack
	(parser-set-reducer-index parser :type 'full-reduced)
	(setq self-parsed-policy 'not-allow-self-parsed))))

(defmethod parser-ignore-final-parsed-as-shifted ((parser parser))
  (assert (parser-is-current-reducer-seq-reducer parser))
  (assert (parser-is-current-shifter-active parser))
  (with-slots (reducer-index-stack shifter-stack) parser
    (assert shifter-stack)
    (let ((shifter (car shifter-stack)))
      (with-slots (reducer-index-stack-length) shifter
	(assert (equal reducer-index-stack-length (length reducer-index-stack)))
	(shifter-set-ignore-final-shifted shifter t)
	(shifter-activate shifter)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod parser-step-not-shifted-handler ((parser parser))
  (assert (parser-is-current-reducer-seq-reducer parser))
  (assert (parser-is-current-shifter-active parser))
  (assert (not (parser-has-token-reduced-or-shifted parser)))
  (with-slots (shifter-stack) parser
    (let ((shifter (pop shifter-stack)))
      (parser-set-reducer-index parser :type 'not-shifted))))

(defmethod parser-step-full-shifted-handler ((parser parser))
  (assert (parser-is-current-reducer-seq-reducer parser))
  (assert (parser-is-current-shifter-active parser))
  (assert (parser-has-token-reduced-or-shifted parser))
  (with-slots (token-reduced-or-shifted parser-streamer shifter-stack) parser
    (log-debug "parser-step-full-shifted-handler" token-reduced-or-shifted)
    (unread-element parser-streamer token-reduced-or-shifted)
    ;; clean
    (setq token-reduced-or-shifted nil)
    (shifter-deactivate (car shifter-stack)))
  (parser-set-reducer-index parser :type 'full-shifted))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod parser-step-not-reduced-handler ((parser parser))
  (assert (not (parser-is-current-reducer-seq-reducer parser)))
  (assert (not (parser-is-current-shifter-active parser)))
  (assert (not (parser-has-token-reduced-or-shifted parser)))
  (parser-set-reducer-index parser :type 'not-reduced))

(defmethod parser-step-full-reduced-handler ((parser parser))
  (assert (not (parser-is-current-reducer-seq-reducer parser)))
  (assert (not (parser-is-current-shifter-active parser)))
  (assert (parser-has-token-reduced-or-shifted parser))
  (with-slots (token-reduced-or-shifted parser-streamer token-type-final) parser
    (log-debug "parser-step-full-reduced-handler" token-reduced-or-shifted)

    (when (lexer:token-typep token-reduced-or-shifted 'function-call-argument-list)
      (log-debug "parser-step-full-reduced-handler 'function-call-argument-list " token-type-final))

    (unread-element parser-streamer token-reduced-or-shifted)
    ;; clean
    (setq token-reduced-or-shifted nil))
  (parser-set-reducer-index parser :type 'full-reduced))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod parser-step-final-token-checker ((parser parser))
  (assert (parser-has-token-reduced-or-shifted parser))
  (assert (not (parser-is-current-reducer-end parser)))
  (assert (not (parser-is-reducer-index-stack-empty parser)))
  (assert (parser-is-token-reduced-or-shifted-final-token parser))
  (let ((result-status))
    (with-slots (token-type-final final-token-checker-policy parser-streamer reducer-index-stack token-reduced-or-shifted) parser
      (log-debug "parser-step-final-token-checker " token-reduced-or-shifted)
      (ecase final-token-checker-policy
	('return-immediately (if (parser-is-current-reducer-seq-reducer parser)
				 (setq result-status 'full-parsed-as-shifted)
				 (setq result-status 'full-parsed-as-reduced)))
	('parse-until-eof (let ((try-last-token (read-element parser-streamer)))
			    (if try-last-token
				(progn
				  (unread-element parser-streamer try-last-token)
				  (if (parser-is-current-reducer-seq-reducer parser)
				      (setq result-status 'just-full-shifted)
				      (setq result-status 'just-full-reduced)))
				(if (parser-is-current-reducer-seq-reducer parser)
				    (setq result-status 'full-parsed-as-shifted)
				    (setq result-status 'full-parsed-as-reduced)))))))
    result-status))

(defmethod parser-step-check-token-reduced-or-shifted-handler ((parser parser))
  (assert (parser-has-token-reduced-or-shifted parser))
  (assert (not (parser-is-current-reducer-end parser)))
  (assert (not (parser-is-reducer-index-stack-empty parser)))
  (let ((result-status))
    (if (parser-is-token-reduced-or-shifted-final-token parser)
	(setq result-status (parser-step-final-token-checker parser))
	(if (parser-is-current-reducer-seq-reducer parser)
	    (setq result-status 'just-full-shifted)
	    (setq result-status 'just-full-reduced)))
    result-status))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod parser-step-check-not-parsed-handler ((parser parser))
  (assert (parser-is-reducer-index-stack-empty parser))
  (assert (not (parser-has-token-reduced-or-shifted parser)))
  (log-debug "parser-step-check-not-parsed-handler" parser)
  (with-slots (token-type-final token-reduced-or-shifted parser-streamer self-parsed-policy) parser
    (let ((last-token (read-element parser-streamer))
	  (result-status))
      (if (and (equal 'allow-self-parsed self-parsed-policy)
	       last-token
	       (lexer:token-typep last-token token-type-final))
	  (progn
	    (setq result-status 'self-parsed)
	    (setq token-reduced-or-shifted last-token)
	    (log-debug "self-parsed " parser-streamer)
	    ;; TODO
	    ;; (parser-streamer-undo parser-streamer :type 'parsed-token-streamer)
	    )
	  (progn
	    (when last-token
	      (unread-element parser-streamer last-token))
	    (with-slots (parser-streamer) parser
	      (parser-streamer-undo parser-streamer :type 'parsed-token-streamer))
	    (setq result-status 'not-parsed)))
      result-status)))

(defmethod parser-step-full-parsed-as-reduced-handler ((parser parser))
  (assert (not (parser-is-current-reducer-seq-reducer parser)))
  (assert (not (parser-is-current-shifter-active parser)))
  (assert (parser-has-token-reduced-or-shifted parser)))

(defmethod parser-step-full-parsed-as-shifted-handler ((parser parser))
  (assert (parser-is-current-reducer-seq-reducer parser))
  (assert (parser-is-current-shifter-active parser))
  (assert (parser-has-token-reduced-or-shifted parser)))

(defmethod parser-step-layer-not-reduced-handler ((parser parser))
  (assert (not (parser-is-reducer-index-stack-empty parser)))
  (assert (parser-is-current-reducer-end parser))
  (assert (not (parser-is-current-shifter-active parser)))
  (parser-ignore parser :type 'last-one-last-layer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod parser-step-return-next-token-as-final-token-handler ((parser parser))
  (with-slots (token-type-final parser-streamer token-reduced-or-shifted) parser
    (let ((next-token (read-element parser-streamer)))
      (assert next-token)
      (assert (lexer:token-typep next-token token-type-final))
      (setq token-reduced-or-shifted next-token)
      'next-token-is-final-token-type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod parser-step-check ((parser parser))
  (labels ((is-need-ignore-final-parsed (parser)
	     (parser-is-ignore-final-parsed parser))
	   (is-need-check-exit-as-not-parsed (parser)
	     (parser-is-reducer-index-stack-empty parser))
	   (is-need-check-layer-not-reduced (parser)
	     (parser-is-current-reducer-end parser))
	   (is-need-check-token-reduced-or-shifted (parser)
	     (parser-has-token-reduced-or-shifted parser))
	   (is-need-check-final-token (parser)
	     (parser-is-token-reduced-or-shifted-final-token parser))
	   ;; (is-need-return-next-token-as-final-token (parser)
	   ;;   (parser-is-next-token-final-token parser))
	   (is-need-check-create-shifter (parser)
	     (and (parser-is-current-reducer-seq-reducer parser)
		  (not (parser-is-current-shifter-active parser))))
	   (is-need-shift (parser)
	     (parser-is-current-shifter-active parser))
	   (is-need-reduce (parser)
	     (and (not (parser-is-current-shifter-active parser))
		  (not (parser-is-current-reducer-seq-reducer parser))
		  (not (parser-is-current-reducer-end parser)))))
    (cond
      ((is-need-ignore-final-parsed parser) 'need-ignore-final-parsed)
      ((is-need-check-exit-as-not-parsed parser) 'need-check-exit-as-not-parsed)
      ((is-need-check-token-reduced-or-shifted parser) 'need-check-token-reduced-or-shifted)
      ;; ((is-need-check-final-token parser) 'need-check-final-token)
      ((is-need-check-layer-not-reduced parser) 'need-check-layer-not-reduced)
      ;; ((is-need-return-next-token-as-final-token parser) 'need-return-next-token-as-final-token)
      ((is-need-check-create-shifter parser) 'need-check-create-shifter)
      ((is-need-shift parser) 'need-shift)
      ((is-need-reduce parser) 'need-reduce))))

(defmethod parser-step-dispatch ((parser parser) step-check-result)
  (ecase step-check-result
    ('need-ignore-final-parsed (parser-ignore parser :type 'final-parsed))
    ('need-check-exit-as-not-parsed (parser-step-check-not-parsed-handler parser))
    ('need-check-token-reduced-or-shifted (parser-step-check-token-reduced-or-shifted-handler parser))
    ;; ('need-check-final-token (parser-step-final-token-checker parser))
    ('need-check-layer-not-reduced (parser-step-layer-not-reduced-handler parser))
    ('need-return-next-token-as-final-token (parser-step-return-next-token-as-final-token-handler parser))
    ('need-check-create-shifter (parser-check-create-shifter parser))
    ('need-shift (parser-shift parser))
    ('need-reduce (parser-reduce parser))))

(defmethod parser-step-handle ((parser parser) step-dispatch-result)
  ;; debug
  (with-slots (parser-id parser-shifter-level parser-streamer self-parsed-policy
			 token-type-final token-reduced-or-shifted shifter-stack reducer-list reducer-index-stack) parser
    (let* ((reducer-index (when reducer-index-stack
			    (car reducer-index-stack)))
	   (reducer (when (and reducer-index
			       (not (equal (length reducer-list) reducer-index)))
		      (elt reducer-list reducer-index)))
	   (next-token (peek-element parser-streamer))
	   (shifter (when (parser-is-current-shifter-active parser)
		      (car shifter-stack)))
	   (info (format nil "parser[~A,~A@~A  ~A] dispatch(~A) reduced/shifted(~A) 
reducer(~A/~A@~A: ~A) 
shifter(~A) parser-streamer-next(~A)"
	   		 parser-id token-type-final parser-shifter-level self-parsed-policy
			 step-dispatch-result
	   		 token-reduced-or-shifted 
	   		 ;; reducer
	   		 reducer-index
	   		 (length reducer-list)
	   		 (length reducer-index-stack)
	   		 reducer
	   		 shifter
	   		 next-token))
	   ;; (info (format nil "hi"))
	   )
      (ecase step-dispatch-result
	;; handle (parser-ignore parser :type 'final-parsed))
	('final-parsed-ignored (log-debug-b info))
	;; handle parser-step-check-not-parsed-handler
	('not-parsed (log-debug-b info self-parsed-policy))
	('self-parsed (log-debug-b info self-parsed-policy))
	;; handle parser-step-check-token-reduced-or-shifted-handler
	('just-full-reduced (log-debug-b info))
	('just-full-shifted (log-debug-b info))
	('full-parsed-as-reduced (log-debug-b info))
	('full-parsed-as-shifted (log-debug-b info))
	;; handle parser-step-layer-not-reduced-handler
	('shifted-ignored (log-debug-b info))
	('reduced-ignored (log-debug-b info))
	('ignore-nothing nil)
	;; parser-step-return-next-token-as-final-token-handler
	('next-token-is-final-token-type nil)
	;; handle parser-create-shifter
	('shifter-created (log-debug-b info))
	('shifter-neednot-created nil)
	;; handle parser-shift
	('not-shifted nil)
	('full-shifted nil)
	;; handle parser-reduce
	('not-reduced nil)
	('full-reduced nil))))

  (let ((result-status 'in-process))
    (ecase step-dispatch-result
      ;; handle (parser-ignore parser :type 'final-parsed))
      ('final-parsed-ignored nil)
      ;; handle parser-step-check-not-parsed-handler
      ('not-parsed (setq result-status 'not-parsed))
      ('self-parsed (setq result-status 'full-parsed))
      ;; handle parser-step-check-token-reduced-or-shifted-handler
      ('just-full-reduced (parser-step-full-reduced-handler parser))
      ('just-full-shifted (parser-step-full-shifted-handler parser))
      ('full-parsed-as-reduced (progn
				 (parser-step-full-parsed-as-reduced-handler parser)
				 (setq result-status 'full-parsed)))
      ('full-parsed-as-shifted (progn
				 (parser-step-full-parsed-as-shifted-handler parser)
				 (setq result-status 'full-parsed)))
      ;; handle parser-step-layer-not-reduced-handler
      ('shifted-ignored nil)
      ('reduced-ignored nil)
      ('ignore-nothing nil)
      ;; parser-step-return-next-token-as-final-token-handler
      ('next-token-is-final-token-type nil)
      ;; handle parser-create-shifter
      ('shifter-created nil)
      ('shifter-neednot-created nil)
      ;; handle parser-shift
      ('not-shifted (parser-step-not-shifted-handler parser))
      ('full-shifted nil)
      ;; handle parser-reduce
      ('not-reduced (parser-step-not-reduced-handler parser))
      ('full-reduced nil))
    result-status))

(defmethod parser-step ((parser parser))
  (let ((parser-shifter-level (with-slots (parser-shifter-level) parser
				parser-shifter-level))
	(index-info (with-slots (reducer-index-stack reducer-list) parser
		      (format nil "(~A)~A/~A@~A" (+ (if reducer-index-stack
							(car reducer-index-stack)
							0)
						    (car '(1)))
			      (car reducer-index-stack)
			      (length reducer-list)
			      (length reducer-index-stack))))
	(step-check-result)
	(step-dispatch-result)
	(step-handle-result))
    (with-slots (parser-id token-type-final) parser
	(log-debug "parser-step >>> " parser-shifter-level parser-id token-type-final index-info))
    (setq step-check-result (parser-step-check parser))
    (log-debug "parser-step " step-check-result)
    (setq step-dispatch-result (parser-step-dispatch parser step-check-result))
    (log-debug "parser-step " step-dispatch-result)
    (setq step-handle-result (parser-step-handle parser step-dispatch-result))
    (log-debug "parser-step " step-handle-result)
    (log-debug "parser-step <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<")
    step-handle-result))

(defmethod parser-parse ((parser parser))
  (let ((result-status)
	(result-token))
    (loop
       do (ecase (parser-step parser)
	    ('in-process nil)
	    ('not-parsed (progn
			   (parser-undo parser :type 'parsed-token-streamer)
			   (setq result-status 'not-parsed)
			   (return)))
	    ('full-parsed (progn
			    (setq result-status 'full-parsed)
			    (with-slots (token-reduced-or-shifted) parser
			      (setq result-token token-reduced-or-shifted)
			      (setq token-reduced-or-shifted nil))
			    (return)))))
    (log-debug "parser-parse " result-status result-token)
    (parser-set-status parser result-status)
    (list result-status result-token)))
