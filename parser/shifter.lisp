(in-package :parser)

(defclass shifter ()
  ((shift-streamer :initform (make-instance 'utils:streamer))
   (shift-reducer :initarg :shift-reducer)
   (token-shifted :initform nil)
   (token-parsed :initform nil)
   (is-active :initarg :is-active :initform nil :accessor shifter-activep)
   (reducer-index :initarg :reducer-index)
   (reducer-index-stack-length :initarg :reducer-index-stack-length)
   (parent-parser :initarg :parent-parser)
   (parser-stack :initform nil)
   (ignore-final-shifted :initform nil)
   (shifter-status :initform nil)
   (self-parsed-policy :initform 'allow-self-parsed)
   ;; debug
   (shifter-id :initform (get-parser-shifter-id :type 'shifter))
   (parser-shifter-level :initarg :parser-shifter-level :initform 0)
   (last-status :initform nil)))

(set-pprint-dispatch 'shifter
		     #'(lambda (stream structure)
			 (with-slots (shift-streamer shift-reducer) structure
			   (funcall (formatter "~@<#<~;~W ~W ~2I~_~/pprint-fill/~;>~:>") stream 'shifter
				    shift-streamer shift-reducer))))

(defmethod initialize-instance :after ((shifter shifter) &rest initargs)
  (with-slots (shift-streamer parser-stack parent-parser shift-reducer) shifter
    ;; add the first token to shift-streamer directly
    (with-slots (token-type-list-reduced-from) shift-reducer
      (with-slots (parser-streamer) parent-parser
	(let ((first-token-of-seq-reducer (read-element parser-streamer)))
	  (assert (lexer:token-typep first-token-of-seq-reducer (car token-type-list-reduced-from)))
	  (add-tail-element shift-streamer first-token-of-seq-reducer))))
    (push (shifter-make-parser shifter) parser-stack)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod shifter-is-parser-stack-empty ((shifter shifter))
  (with-slots (parser-stack) shifter
    (null parser-stack)))

(defmethod shifter-is-shift-streamer-empty ((shifter shifter))
  (with-slots (shift-streamer) shifter
    (zerop (streamer-length shift-streamer))))

(defmethod shifter-current-parser-status-p ((shifter shifter) status)
  (assert (not (shifter-is-parser-stack-empty shifter)))
  (with-slots (parser-stack) shifter
    (with-slots (parser-status) (car parser-stack)
      (equal status parser-status))))

(defmethod shifter-has-token-shifted ((shifter shifter))
  (with-slots (token-shifted) shifter
    token-shifted))

(defmethod shifter-has-token-parsed ((shifter shifter))
  (with-slots (token-parsed) shifter
    token-parsed))

(defmethod shifter-activate ((shifter shifter))
  (with-slots (is-active) shifter
    (setq is-active t)))

(defmethod shifter-deactivate ((shifter shifter))
  (with-slots (is-active) shifter
    (setq is-active nil)))

(defmethod shifter-is-ignore-final-shifted ((shifter shifter))
  (with-slots (ignore-final-shifted) shifter
    ignore-final-shifted))

(defmethod shifter-set-ignore-final-shifted ((shifter shifter) status)
  (with-slots (ignore-final-shifted) shifter
    (setq ignore-final-shifted status)))

(defmethod shifter-is-shift-streamer-filled ((shifter shifter))
  (with-slots (shift-streamer shift-reducer) shifter
    (with-slots (token-type-list-reduced-from) shift-reducer
      (= (streamer-length shift-streamer)
	 (length token-type-list-reduced-from)))))

(defmethod shifter-is-shift-streamer-not-filled ((shifter shifter))
  (with-slots (shift-streamer shift-reducer) shifter
    (with-slots (token-type-list-reduced-from) shift-reducer
      (< (streamer-length shift-streamer)
	 (length token-type-list-reduced-from)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod shifter-create-shifter-parser ((shifter shifter))
  (assert (not (shifter-has-token-shifted shifter)))
  (assert (not (shifter-is-ignore-final-shifted shifter)))
  (assert (not (shifter-is-parser-stack-empty shifter)))
  (assert (not (shifter-is-shift-streamer-filled shifter)))
  (assert (not (shifter-current-parser-status-p shifter nil)))
  (assert (shifter-activep shifter))
  (let ((new-parser (shifter-make-parser shifter)))
    (with-slots (parser-stack) shifter
      (push new-parser parser-stack)))
  'parser-created)

(defmethod shifter-make-parser ((shifter shifter))
  (assert (not (shifter-is-shift-streamer-filled shifter)))
  (with-slots (shift-streamer shift-reducer parent-parser parser-shifter-level self-parsed-policy) shifter
    (with-slots (token-type-list-reduced-from) shift-reducer
      (with-slots (parser-streamer reducer-list final-token-checker-policy) parent-parser
	(let ((shifted-token-list (peek-element-all shift-streamer))
	      (token-type-final (elt token-type-list-reduced-from (streamer-length shift-streamer)))
	      (new-parser-streamer (copy-class parser-streamer))
	      (new-parser))
	  (assert (< (streamer-length shift-streamer) (length token-type-list-reduced-from)))
	  (assert (every #'utils:notnull (mapcar #'lexer:token-typep shifted-token-list token-type-list-reduced-from)))
	  (with-slots (token-streamer-read-token-stack) new-parser-streamer
	    (setq token-streamer-read-token-stack nil))
	  (setq new-parser (make-instance 'parser
					  :parser-streamer new-parser-streamer
					  :reducer-list reducer-list
					  :token-type-final token-type-final
					  ;; :final-token-checker-policy final-token-checker-policy
					  :final-token-checker-policy 'return-immediately
					  :self-parsed-policy self-parsed-policy
					  :parser-shifter-level (+ 1 parser-shifter-level)
					  :reducer-index-stack '(0)))
	  (with-slots (reducer-index-stack) new-parser
	    (setq reducer-index-stack (list (- 1 1))))
	  (when (null self-parsed-policy)
	    (log-debug-b "shifter-make-parser with " self-parsed-policy (with-slots (parser-id) new-parser
									  parser-id)))
	  (log-debug "shifter-make-parser " shifter new-parser (+ (car '(0)) (car '(1)))
		     (with-slots (reducer-index-stack) new-parser
		       (+ (car reducer-index-stack) (car '(1)))))
	  new-parser)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod shifter-reducer-reduce ((shifter shifter))
  (assert (not (shifter-has-token-shifted shifter)))
  (assert (not (shifter-is-ignore-final-shifted shifter)))
  (assert (not (shifter-is-parser-stack-empty shifter)))
  (assert (shifter-activep shifter))
  (assert (shifter-is-shift-streamer-filled shifter))
  (with-slots (shift-streamer shift-reducer token-shifted) shifter
    (destructuring-bind (status token-reduced) (reducer-reduce shift-reducer shift-streamer)
      (assert (equal status 'full-reduced))
      (setq token-shifted token-reduced)
      'full-shifted)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod shifter-ignore ((shifter shifter) &key type)
  (ecase type
    ('all (shifter-ignore-all shifter))
    ('all-except-first-token (shifter-ignore-all-except-first-token shifter))
    ('final-shifted (shifter-ignore-final-shifted shifter))))

(defmethod shifter-ignore-all ((shifter shifter))
  (assert (not (shifter-has-token-shifted shifter)))
  (assert (not (shifter-is-ignore-final-shifted shifter)))
  (assert (shifter-is-parser-stack-empty shifter))
  (assert (shifter-activep shifter))
  (with-slots (shift-streamer parent-parser) shifter
    (with-slots (parser-streamer) parent-parser
      (let ((token-list (read-element-all shift-streamer)))
	(mapcar #'(lambda (token) (unread-element-token parser-streamer token :type 'to-all-orignal))
		(reverse token-list)))))
  'all-ignored)

(defmethod shifter-ignore-all-except-first-token ((shifter shifter))
  (assert (not (shifter-has-token-shifted shifter)))
  (assert (not (shifter-is-ignore-final-shifted shifter)))
  (assert (shifter-is-parser-stack-empty shifter))
  (assert (shifter-activep shifter))
  (with-slots (shift-streamer parent-parser) shifter
    (with-slots (parser-streamer) parent-parser
      (let ((first-token)
	    (token-list (read-element-all shift-streamer)))
	(setq first-token (car token-list))
	(assert first-token)
	(setq token-list (cdr token-list))
	(mapcar #'(lambda (token) (unread-element-token parser-streamer token :type 'to-all-orignal))
		(reverse token-list))
	;; TODO
	;; unread-element to shifter-streamer ? or (assert (equal 1 (streamer-length shift-streamer))) in
	;; shifter-not-shifted-handler should fail
	(unread-element parser-streamer first-token))))
  'all-except-first-token-ignored)

#+nil
(defmethod shifter-ignore-final-shifted ((shifter shifter))
  ;; (assert (not (shifter-activep shifter)))
  (assert (not (shifter-is-parser-stack-empty shifter)))
  (assert (shifter-is-shift-streamer-empty shifter))
  (let ((token-shifted))
    (with-slots (shift-streamer is-active parent-parser parser-stack) shifter
      (with-slots (shifter-stack reducer-index-stack parser-streamer) parent-parser
	(assert (equal shifter (car shifter-stack)))
	(let (;; (token-shifted (read-element parser-streamer))
	      (last-parser (car parser-stack)))
	  (setq token-shifted (read-element parser-streamer))
	  ;; (log-debug "shifter-ignore-final-shifted " token-shifted (slot-value token-shifted 'token-type))
	  (log-debug "shifter-ignore-final-shifted " token-shifted (lexer::class-token-list-reduced-from token-shifted))
	  (assert (equal 'lexer:token (type-of token-shifted)))
	  (with-slots (token-buf token-type ) token-shifted
	    (list token-buf token-type ))
	  (with-slots (token-list-reduced-from) token-shifted
	    ;; put butlast token-list to shift-streamer
	    (unread-element-seq shift-streamer (butlast token-list-reduced-from))
	    ;; put last token to last parser and set ignore-final-parsed
	    (with-slots (parser-streamer) last-parser
	      (unread-element parser-streamer (car (last token-list-reduced-from))))
	    (parser-set-ignore-final-parsed last-parser))))
      'final-shifted-ignored)))

(defmethod shifter-ignore-final-shifted ((shifter shifter))
  ;; (assert (not (shifter-activep shifter)))
  (assert (not (shifter-is-parser-stack-empty shifter)))
  ;; (assert (shifter-is-shift-streamer-empty shifter))
  (let ((token-shifted))
    (with-slots (shift-streamer is-active parent-parser parser-stack self-parsed-policy) shifter
      (with-slots (shifter-stack reducer-index-stack parser-streamer) parent-parser
	(assert (equal shifter (car shifter-stack)))
	(let (;; (token-shifted (read-element parser-streamer))
	      (last-parser (car parser-stack)))
	  (setq token-shifted (read-element parser-streamer))
	  ;; (log-debug "shifter-ignore-final-shifted " token-shifted (slot-value token-shifted 'token-type))
	  (log-debug "shifter-ignore-final-shifted " token-shifted (lexer::class-token-list-reduced-from token-shifted))
	  (assert (equal 'lexer:token (type-of token-shifted)))
	  ;; put butlast token-list to shift-streamer
	  (unread-element-seq shift-streamer (butlast (lexer::class-token-list-reduced-from token-shifted)))
	  ;; put last token to last parser and set ignore-final-parsed
	  (with-slots (parser-streamer) last-parser
	    (unread-element parser-streamer (car (last (lexer::class-token-list-reduced-from token-shifted)))))
	  (setq self-parsed-policy 'not-allow-self-parsed)
	  (parser-set-ignore-final-parsed last-parser)
	  (parser-set-status last-parser nil)))
      (shifter-set-ignore-final-shifted shifter nil)
      'final-shifted-ignored)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod shifter-shift-parse ((shifter shifter))
  (assert (not (shifter-is-parser-stack-empty shifter)))
  (assert (not (shifter-has-token-parsed shifter)))
  (assert (shifter-current-parser-status-p shifter nil))
  (assert (shifter-is-shift-streamer-not-filled shifter))
  (with-slots (parser-stack token-parsed) shifter
    (log-debug "shifter-shift-parse" (with-slots (reducer-index-stack)
					 (car parser-stack)
				       reducer-index-stack)
	       (+ (car '(0)) (car '(1))))
    ;; (break)
    (destructuring-bind (status token) (parser-parse (car parser-stack))
      (ecase status
	('not-parsed nil)
	('full-parsed (setq token-parsed token)))
      status)))

(defmethod shifter-full-parsed-handler ((shifter shifter))
  (assert (not (shifter-has-token-shifted shifter)))
  (assert (shifter-has-token-parsed shifter))
  (assert (shifter-is-shift-streamer-not-filled shifter))
  (with-slots (shift-streamer token-parsed self-parsed-policy) shifter
    (add-tail-element shift-streamer token-parsed)
    (setq token-parsed nil)
    (setq self-parsed-policy 'allow-self-parsed)
    'nothing))

(defmethod shifter-not-parsed-handler ((shifter shifter))
  (assert (not (shifter-has-token-parsed shifter)))
  (assert (not (shifter-has-token-shifted shifter)))
  (assert (shifter-is-shift-streamer-not-filled shifter))
  (with-slots (shift-streamer parser-stack) shifter
    (log-debug "shifter-not-parsed-handler " shifter (car parser-stack) (second parser-stack)
	       (length parser-stack))
    (pop parser-stack)
    (if parser-stack
	(let ((last-token (read-tail-element shift-streamer))
	      (parser (car parser-stack)))
	  ;; (assert (equal (streamer-length shift-streamer) (- (length parser-stack) 1)))
	  (when (not (< 0 (streamer-length shift-streamer)))
	    (log-debug-b "shifter-not-parsed-handler" (length parser-stack) (car parser-stack)))
	  (assert (< 0 (streamer-length shift-streamer)))
	  (with-slots (parser-streamer) parser
	    (unread-element parser-streamer last-token))
	  (parser-set-ignore-final-parsed parser)
	  (parser-set-status parser nil)
	  'set-ignore-final-parsed)
	'parser-stack-empty)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod shifter-not-shifted-handler ((shifter shifter))
  (assert (not (shifter-has-token-parsed shifter)))
  (assert (not (shifter-has-token-shifted shifter)))
  (assert (shifter-is-parser-stack-empty shifter))
  ;; when creating shifter, the first token is exactly the first token of seq-reudcer
  ;; and is put to shift-streamer, so put it back
  (with-slots (shift-streamer parent-parser) shifter
    (assert (equal 1 (streamer-length shift-streamer)))
    (let ((first-token (read-element shift-streamer)))
      (with-slots (parser-streamer) parent-parser
	(unread-element parser-streamer first-token)))))

(defmethod shifter-full-shifted-handler ((shifter shifter))
  (assert (not (shifter-has-token-parsed shifter)))
  (assert (not (shifter-is-parser-stack-empty shifter)))
  (assert (shifter-is-shift-streamer-empty shifter))
  (assert (shifter-has-token-shifted shifter))
  ;; (with-slots (token-shifted parent-parser) shifter
  ;;   (with-slots (token-reduced-or-shifted parser-streamer) parent-parser
  ;;     (assert (null token-reduced-or-shifted))
  ;;     (setq token-reduced-or-shifted token-shifted)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod shifter-step-check ((shifter shifter))
  (labels ((is-need-ignore-final-shifted (shifter)
	     (shifter-is-ignore-final-shifted shifter))
	   (is-need-check-exit-as-not-shifted (shifter)
	     (shifter-is-parser-stack-empty shifter))
	   (is-need-check-final-token (shifter)
	     (shifter-is-shift-streamer-filled shifter))
	   (is-need-shifter-shift-parse (shifter)
	     (shifter-current-parser-status-p shifter nil))
	   (is-need-create-shifter-parser (shifter)
	     (shifter-is-shift-streamer-not-filled shifter)))
    (cond
      ((is-need-ignore-final-shifted shifter) 'need-ignore-final-shifted)
      ((is-need-check-exit-as-not-shifted shifter) 'need-check-exit-as-not-shifted)
      ((is-need-check-final-token shifter) 'need-check-final-token)
      ((is-need-shifter-shift-parse shifter) 'need-shifter-shift-parse)
      ((is-need-create-shifter-parser shifter) 'need-create-shifter-parser))))

(defmethod shifter-step-dispatch ((shifter shifter) step-check-result)
  (ecase step-check-result
    ('need-ignore-final-shifted (shifter-ignore shifter :type 'final-shifted))
    ('need-check-exit-as-not-shifted (shifter-ignore shifter :type 'all-except-first-token))
    ('need-check-final-token (shifter-reducer-reduce shifter))
    ('need-shifter-shift-parse (shifter-shift-parse shifter))
    ('need-create-shifter-parser (shifter-create-shifter-parser shifter))))

(defmethod shifter-step-handle ((shifter shifter) step-dispatch-result)
  (let ((result-status 'in-process))
    (ecase step-dispatch-result
      ;; (shifter-ignore shifter :type 'final-shifted)
      ('final-shifted-ignored nil)
      ;; (shifter-ignore shifter :type 'all-except-first-token)
      ('all-except-first-token-ignored (setq result-status 'not-shifted))
      ;; (shifter-reducer-reduce shifter)
      ('full-shifted (setq result-status 'full-shifted))
      ;; (shifter-shift-parse shifter)
      ('not-parsed (ecase (shifter-not-parsed-handler shifter)
		     ('set-ignore-final-parsed nil)
		     ('parser-stack-empty (setq result-status 'not-shifted))))
      ('full-parsed (shifter-full-parsed-handler shifter))
      ;; (shifter-create-shifter-parser shifter)
      ('parser-created nil))
    result-status))

(defmethod shifter-step ((shifter shifter))
  (let ((parser-shifter-level (with-slots (parser-shifter-level) shifter
				parser-shifter-level))
	(step-check-result)
	(step-dispatch-result)
	(step-handle-result))
    (with-slots (shifter-id shift-reducer shift-streamer) shifter
      (log-debug "shifter-step >>>" parser-shifter-level shifter-id shift-reducer shift-streamer))
    (setq step-check-result (shifter-step-check shifter))
    (log-debug "shifter-step " step-check-result )
    (setq step-dispatch-result (shifter-step-dispatch shifter step-check-result))
    (log-debug "shifter-step " step-dispatch-result)
    (setq step-handle-result (shifter-step-handle shifter step-dispatch-result))
    (log-debug "shifter-step "  step-handle-result)
    (log-debug "shifter-step <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<")
    step-handle-result))

(defmethod shifter-shift ((shifter shifter))
  (let ((result-status)
	(result-token))
    (log-debug "shifter-shift enter")
    (loop
       do (ecase (shifter-step shifter)
	    ('in-process nil)
	    ('not-shifted (progn
			    (shifter-not-shifted-handler shifter)
			    (setq result-status 'not-shifted)
			    (return)))
	    ('full-shifted (progn
			     (shifter-full-shifted-handler shifter)
			     (setq result-status 'full-shifted)
			     (with-slots (token-shifted) shifter
			       (setq result-token token-shifted)
			       (setq token-shifted nil))
			     (return)))))
    (log-debug "shifter-shift " result-status)
    (with-slots (shifter-status) shifter
      (setq shifter-status result-status))
    (list result-status result-token)))
