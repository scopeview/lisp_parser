(in-package :parser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; simple caculation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; expr := (number | add-expr | p-expr)
;;; add-expr := expr tok-add expr
;;; p-expr := tok-lp expr tok-rp
;;; number := tok-number
(let* ((tok-number-1 (make-instance 'lexer:token :token-type 'tok-number :token-buf "1"))
       (tok-number-2 (make-instance 'lexer:token :token-type 'tok-number :token-buf "2"))
       (tok-number-3 (make-instance 'lexer:token :token-type 'tok-number :token-buf "3"))
       (tok-number-4 (make-instance 'lexer:token :token-type 'tok-number :token-buf "4"))
       (tok-add (make-instance 'lexer:token :token-type 'tok-add :token-buf "+"))
       (tok-lp (make-instance 'lexer:token :token-type 'tok-lp :token-buf "("))
       (tok-rp (make-instance 'lexer:token :token-type 'tok-rp :token-buf ")"))
       ;; 1+(2+3)+4
       (token-list-1 (list tok-number-1 tok-add
			   tok-lp tok-number-2 tok-add tok-number-3 tok-rp
			   tok-add tok-number-4 ))
       ;; 1+(2+3)+(3+4)+1
       (token-list-2 (list tok-number-1 tok-add
			   tok-lp tok-number-2 tok-add tok-number-3 tok-rp
			   tok-add
			   tok-lp tok-number-3 tok-add tok-number-4 tok-rp
			   tok-add
			   tok-number-1 ))
       ;; 1+((2+3)+(3+4))+1
       (token-list-3 (list tok-number-1 tok-add
			   tok-lp tok-lp tok-number-2 tok-add tok-number-3 tok-rp
			   tok-add
			   tok-lp tok-number-3 tok-add tok-number-4 tok-rp tok-rp
			   tok-add
			   tok-number-1 ))
       ;; (1+2+3+4+(((1+2)))+((3)+(4)))
       (token-list-4 (list tok-lp
			   tok-number-1 tok-add
			   tok-number-2 tok-add
			   tok-number-3 tok-add
			   tok-number-4 tok-add
			   tok-lp tok-lp tok-lp tok-number-1 tok-add tok-number-2 tok-rp tok-rp tok-rp
			   tok-add
			   tok-lp tok-lp tok-number-3 tok-rp tok-add tok-lp tok-number-4 tok-rp tok-rp tok-rp ))
       ;; 
       (token-streamer (make-instance 'utils:streamer :source token-list-4))
       (parser-streamer (make-instance 'parser-streamer :token-streamer token-streamer))
       ;; 
       (single-reducer-number (make-instance 'single-reducer
					     :token-type-reduced 'number
					     :token-type-reduced-from 'tok-number))
       (seq-reducer-p-expr (make-instance 'seq-reducer
					  :token-type-reduced 'p-expr
					  :token-type-list-reduced-from (list 'tok-lp 'expr 'tok-rp)))
       (seq-reducer-add-expr (make-instance 'seq-reducer
					    :token-type-reduced 'add-expr
					    :token-type-list-reduced-from (list 'expr 'tok-add 'expr)))
       (any-reducer-expr (make-instance 'any-reducer
					:token-type-reduced 'expr
					:token-type-list-reduced-from (list 'number 'add-expr 'p-expr)))
       (reducer-list (list single-reducer-number
			   seq-reducer-p-expr
			   seq-reducer-add-expr
			   any-reducer-expr))
       (parser (make-instance 'parser
			      :parser-streamer parser-streamer
			      :reducer-list reducer-list
			      :token-type-final 'expr)))
  (with-slots (reducer-index-stack) parser
    (setq reducer-index-stack (list (- 1 1))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; 
  (destructuring-bind (status token-parsed) (parser-parse parser)
    (lisp-unit2:assert-equal 'parser::full-parsed status)
    ;; (lisp-unit2:assert-true (lexer:token-typep token-parsed 'expr))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; recursive rule
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let* ((tok-number-1 (make-instance 'lexer:token :token-type 'tok-number :token-buf "1"))
       (tok-number-2 (make-instance 'lexer:token :token-type 'tok-number :token-buf "2"))
       (tok-number-3 (make-instance 'lexer:token :token-type 'tok-number :token-buf "3"))
       (tok-comma (make-instance 'lexer:token :token-type 'tok-comma :token-buf ","))
       ;; 1,2,3
       (token-list (list tok-number-1 tok-comma
			 tok-number-2 tok-comma
			 tok-number-3))
       ;; 
       (token-streamer (make-instance 'utils:streamer :source token-list))
       (parser-streamer (make-instance 'parser-streamer :token-streamer token-streamer))
       ;; 
       (list (make-instance 'any-reducer
			    :token-type-reduced 'list
			    :token-type-list-reduced-from (list 'single
								'multiple)))
       (single (make-instance 'single-reducer
			      :token-type-reduced 'single
			      :token-type-reduced-from 'tok-number))

       (multiple (make-instance 'seq-reducer
				:token-type-reduced 'multiple
				:token-type-list-reduced-from (list 'list
								    'tok-comma
								    'single)))
       (reducer-list (list list
			   single
			   multiple))
       (parser (make-instance 'parser
			      :parser-streamer parser-streamer
			      :reducer-list reducer-list
			      :token-type-final 'list)))
  (with-slots (reducer-index-stack) parser
    (setq reducer-index-stack (list (- 1 1))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; 
  (destructuring-bind (status token-parsed) (parser-parse parser)
    (lisp-unit2:assert-equal 'parser::full-parsed status)
    ;; (lisp-unit2:assert-true (lexer:token-typep token-parsed 'expr))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; two recursive rules with the same pattern
;;; - function-definition-list
;;; - function-call-argument-list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let* ((tok-keyword-int (make-instance 'lexer:token :token-type 'keyword-int :token-buf "int"))
       (tok-keyword-char (make-instance 'lexer:token :token-type 'keyword-char :token-buf "char"))
       (tok-keyword-double (make-instance 'lexer:token :token-type 'keyword-double :token-buf "double"))
       (tok-keyword-return (make-instance 'lexer:token :token-type 'keyword-return :token-buf "return"))
       (tok-string-fun (make-instance 'lexer:token :token-type 'string :token-buf "fun"))
       (tok-string-argc (make-instance 'lexer:token :token-type 'string :token-buf "argc"))
       (tok-string-i (make-instance 'lexer:token :token-type 'string :token-buf "i"))
       (tok-string-j (make-instance 'lexer:token :token-type 'string :token-buf "j"))
       (tok-string-0 (make-instance 'lexer:token :token-type 'string :token-buf "0"))
       (tok-string-1 (make-instance 'lexer:token :token-type 'string :token-buf "1"))
       (tok-string-printf (make-instance 'lexer:token :token-type 'string :token-buf "printf"))
       (tok-string-printf-formatter (make-instance 'lexer:token :token-type 'string :token-buf "\"%d\n\""))
       (tok-string-printf-rest (make-instance 'lexer:token :token-type 'string :token-buf "i"))
       (tok-lp (make-instance 'lexer:token :token-type 'tok-lp :token-buf "("))
       (tok-rp (make-instance 'lexer:token :token-type 'tok-rp :token-buf ")"))
       (tok-lb (make-instance 'lexer:token :token-type 'tok-lb :token-buf "{"))
       (tok-rb (make-instance 'lexer:token :token-type 'tok-rb :token-buf "}"))
       (tok-comma (make-instance 'lexer:token :token-type 'tok-comma :token-buf ","))
       (tok-semicomma (make-instance 'lexer:token :token-type 'tok-semicomma :token-buf ";"))
       (tok-equal (make-instance 'lexer:token :token-type 'tok-equal :token-buf "="))
       ;; 	printf("%d\n", j);
       ;;  -->
       ;; 	("%d\n", j);
       (token-list (list tok-lp tok-string-printf-formatter tok-comma tok-string-j tok-rp))

       ;; 
       (token-streamer (make-instance 'utils:streamer :source token-list))
       (parser-streamer (make-instance 'parser-streamer :token-streamer token-streamer))
       ;;;;;;;;;;;;;;;;
       ;;
       ;;;;;;;;;;;;;;;;
       (function-definition (make-instance 'seq-reducer
					   :token-type-reduced 'function-definition
					   :token-type-list-reduced-from (list 'type
									       'function-name
									       'function-parameter-brace-list
									       'function-brace-body)))
       (function-parameter-brace-list (make-instance 'seq-reducer
						     :token-type-reduced 'function-parameter-brace-list
						     :token-type-list-reduced-from (list 'tok-lp
											 'function-parameter-list
											 'tok-rp)))
       (function-parameter-list (make-instance 'any-reducer
					       :token-type-reduced 'function-parameter-list
					       :token-type-list-reduced-from (list 'function-parameter-single
										   'function-parameter-multiple)))
       (function-parameter-single (make-instance 'single-reducer
						 :token-type-reduced 'function-parameter-single
						 :token-type-reduced-from 'function-parameter-declaration))
       (function-parameter-multiple (make-instance 'seq-reducer
						   :token-type-reduced 'function-parameter-multiple
						   :token-type-list-reduced-from (list 'function-parameter-list
										       'tok-comma
										       'function-parameter-single)))
       (function-name (make-instance 'single-reducer
				     :token-type-reduced 'function-name
				     :token-type-reduced-from 'string))
       ;; 
       (function-parameter-declaration (make-instance 'any-reducer
						      :token-type-reduced 'function-parameter-declaration
						      :token-type-list-reduced-from (list 'int-declaration)))
       (int-declaration (make-instance 'seq-reducer
				       :token-type-reduced 'int-declaration
				       :token-type-list-reduced-from (list 'keyword-int
									   'var)))
       ;;
       (function-brace-body (make-instance 'seq-reducer
					   :token-type-reduced 'function-brace-body
					   :token-type-list-reduced-from (list 'tok-lb
									       'stmt-list
									       'tok-rb)))
       ;;
       (stmt-list (make-instance 'any-reducer
				 :token-type-reduced 'stmt-list
				 :token-type-list-reduced-from (list 'stmt-single
								     'stmt-multiple)))
       (stmt-single (make-instance 'any-reducer
				   :token-type-reduced 'stmt-single
				   :token-type-list-reduced-from (list 'var-definition
								       'function-call
								       'return-stmt)))
       (stmt-multiple (make-instance 'seq-reducer
				     :token-type-reduced 'stmt-multiple
				     :token-type-list-reduced-from (list 'stmt-list
									 'stmt-single)))
       ;;
       (var-definition (make-instance 'any-reducer
				      :token-type-reduced 'var-definition
				      :token-type-list-reduced-from (list 'double-definition)))
       (double-definition (make-instance 'seq-reducer
					 :token-type-reduced 'double-definition
					 :token-type-list-reduced-from (list 'keyword-double
									     'var
									     'tok-equal
									     'val
									     'tok-semicomma)))
       ;;
       (function-call (make-instance 'seq-reducer
				     :token-type-reduced 'function-call
				     :token-type-list-reduced-from (list 'function-name
									 'function-call-argument-brace-list
									 'tok-semicomma)))
       (function-call-argument-brace-list (make-instance 'seq-reducer
							 :token-type-reduced 'function-call-argument-brace-list
							 :token-type-list-reduced-from (list 'tok-lp
											     'function-call-argument-list
											     'tok-rp)))
       (function-call-argument-list (make-instance 'any-reducer
						   :token-type-reduced 'function-call-argument-list
						   :token-type-list-reduced-from (list 'function-call-argument-single
										       'function-call-argument-multiple)))
       (function-call-argument-single (make-instance 'single-reducer
						     :token-type-reduced 'function-call-argument-single
						     :token-type-reduced-from 'var-or-val))
       (function-call-argument-multiple (make-instance 'seq-reducer
						       :token-type-reduced 'function-call-argument-multiple
						       :token-type-list-reduced-from (list 'function-call-argument-list
											   'tok-comma
											   'function-call-argument-single)))
       ;;
       (return-stmt (make-instance 'seq-reducer
				   :token-type-reduced 'return-stmt
				   :token-type-list-reduced-from (list 'keyword-return
								       'var-or-val
								       'tok-semicomma)))
       ;;
       (var-or-val (make-instance 'any-reducer
				  :token-type-reduced 'var-or-val
				  :token-type-list-reduced-from (list 'var
								      'val)))
       (var (make-instance 'single-reducer
			   :token-type-reduced 'var
			   :token-type-reduced-from 'string))
       (val (make-instance 'single-reducer
			   :token-type-reduced 'val
			   :token-type-reduced-from 'string))
       ;;
       (type (make-instance 'any-reducer
			    :token-type-reduced 'type
			    :token-type-list-reduced-from (list 'keyword-int
								'keyword-char
								'keyword-double)))
       (reducer-list (list
		      ;;
		      function-parameter-brace-list


		      function-call-argument-brace-list
		      function-call-argument-list
		      function-call-argument-single
		      function-call-argument-multiple
		      var val type
		      var-or-val
		      var-definition double-definition

		      ;; 
		      ;; function-parameter-brace-list
		      ))
       (parser (make-instance 'parser
			      :parser-streamer parser-streamer
			      :reducer-list reducer-list
			      :token-type-final 'function-call-argument-brace-list)))
  (with-slots (reducer-index-stack) parser
    (setq reducer-index-stack (list (- 1 1))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; 
  (destructuring-bind (status token-parsed) (parser-parse parser)
    (lisp-unit2:assert-equal 'parser::full-parsed status)
    ;; (lisp-unit2:assert-true (lexer:token-typep token-parsed 'expr))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; function-call
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let* ((tok-keyword-int (make-instance 'lexer:token :token-type 'keyword-int :token-buf "int"))
       (tok-keyword-char (make-instance 'lexer:token :token-type 'keyword-char :token-buf "char"))
       (tok-keyword-double (make-instance 'lexer:token :token-type 'keyword-double :token-buf "double"))
       (tok-keyword-return (make-instance 'lexer:token :token-type 'keyword-return :token-buf "return"))
       (tok-string-fun (make-instance 'lexer:token :token-type 'string :token-buf "fun"))
       (tok-string-argc (make-instance 'lexer:token :token-type 'string :token-buf "argc"))
       (tok-string-i (make-instance 'lexer:token :token-type 'string :token-buf "i"))
       (tok-string-j (make-instance 'lexer:token :token-type 'string :token-buf "j"))
       (tok-string-0 (make-instance 'lexer:token :token-type 'string :token-buf "0"))
       (tok-string-1 (make-instance 'lexer:token :token-type 'string :token-buf "1"))
       (tok-string-printf (make-instance 'lexer:token :token-type 'string :token-buf "printf"))
       (tok-string-printf-formatter (make-instance 'lexer:token :token-type 'string :token-buf "\"%d\n\""))
       (tok-string-printf-rest (make-instance 'lexer:token :token-type 'string :token-buf "i"))
       (tok-lp (make-instance 'lexer:token :token-type 'tok-lp :token-buf "("))
       (tok-rp (make-instance 'lexer:token :token-type 'tok-rp :token-buf ")"))
       (tok-lb (make-instance 'lexer:token :token-type 'tok-lb :token-buf "{"))
       (tok-rb (make-instance 'lexer:token :token-type 'tok-rb :token-buf "}"))
       (tok-comma (make-instance 'lexer:token :token-type 'tok-comma :token-buf ","))
       (tok-semicomma (make-instance 'lexer:token :token-type 'tok-semicomma :token-buf ";"))
       (tok-equal (make-instance 'lexer:token :token-type 'tok-equal :token-buf "="))
       ;; 	printf("%d\n", j);
       (token-list (list tok-string-printf tok-lp tok-string-printf-formatter tok-comma tok-string-j tok-rp
       			 tok-semicomma))
       ;; 
       (token-streamer (make-instance 'utils:streamer :source token-list))
       (parser-streamer (make-instance 'parser-streamer :token-streamer token-streamer))
       ;;;;;;;;;;;;;;;;
       ;;
       ;; function-definition := seq of type function-name function-parameter-brace-list function-brace-body
       ;; function-parameter-brace-list := seq of ( function-parameter-list  )
       ;; function-parameter-list := any of function-parameter-single function-parameter-multiple
       ;; function-parameter-single := function-parameter-declaration
       ;; function-parameter-multiple := seq of function-parameter-list , function-parameter-single
       ;; function-name := any string

       ;; function-parameter-declaration := any of int-declaration
       ;; int-declaration := seq of keyword-int var

       ;; function-brace-body := seq of { stmt-list }

       ;; stmt-list := any of stmt-single stmt-multiple
       ;; stmt-single = any of var-definition function-call return-stmt
       ;; stmt-multiple := seq of stmt-list stmt-single

       ;; var-definition := any of int-definition
       ;; int-definition := seq of keyword-int var = val ;

       ;; function-call := seq of function-name function-call-argument-brace-list ;
       ;; function-call-argument-brace-list := seq of ( function-call-argument-list )
       ;; function-call-argument-list := any of function-call-argument-single function-call-argument-multiple
       ;; function-call-argument-single := var-or-val
       ;; function-call-argument-multiple := seq of function-call-argument-list , function-call-argument-single

       ;; return-stmt := seq of keyword-return var-or-val ;

       ;; var-or-val := any of var val
       ;; var := any string
       ;; val := any string

       ;; type := any of (keyword-int, keyword-char)
       ;;;;;;;;;;;;;;;;
       (function-definition (make-instance 'seq-reducer
					   :token-type-reduced 'function-definition
					   :token-type-list-reduced-from (list 'type
									       'function-name
									       'function-parameter-brace-list
									       'function-brace-body)))
       (function-parameter-brace-list (make-instance 'seq-reducer
						     :token-type-reduced 'function-parameter-brace-list
						     :token-type-list-reduced-from (list 'tok-lp
											 'function-parameter-list
											 'tok-rp)))
       (function-parameter-list (make-instance 'any-reducer
					       :token-type-reduced 'function-parameter-list
					       :token-type-list-reduced-from (list 'function-parameter-single
										   'function-parameter-multiple)))
       (function-parameter-single (make-instance 'single-reducer
						 :token-type-reduced 'function-parameter-single
						 :token-type-reduced-from 'function-parameter-declaration))
       (function-parameter-multiple (make-instance 'seq-reducer
						   :token-type-reduced 'function-parameter-multiple
						   :token-type-list-reduced-from (list 'function-parameter-list
										       'tok-comma
										       'function-parameter-single)))
       (function-name (make-instance 'single-reducer
				     :token-type-reduced 'function-name
				     :token-type-reduced-from 'string))
       ;; 
       (function-parameter-declaration (make-instance 'any-reducer
						      :token-type-reduced 'function-parameter-declaration
						      :token-type-list-reduced-from (list 'int-declaration)))
       (int-declaration (make-instance 'seq-reducer
				       :token-type-reduced 'int-declaration
				       :token-type-list-reduced-from (list 'keyword-int
									   'var)))
       ;;
       (function-brace-body (make-instance 'seq-reducer
					   :token-type-reduced 'function-brace-body
					   :token-type-list-reduced-from (list 'tok-lb
									       'stmt-list
									       'tok-rb)))
       ;;
       (stmt-list (make-instance 'any-reducer
				 :token-type-reduced 'stmt-list
				 :token-type-list-reduced-from (list 'stmt-single
								     'stmt-multiple)))
       (stmt-single (make-instance 'any-reducer
				   :token-type-reduced 'stmt-single
				   :token-type-list-reduced-from (list 'var-definition
								       'function-call
								       'return-stmt)))
       (stmt-multiple (make-instance 'seq-reducer
				     :token-type-reduced 'stmt-multiple
				     :token-type-list-reduced-from (list 'stmt-list
									 'stmt-single)))
       ;;
       (var-definition (make-instance 'any-reducer
				      :token-type-reduced 'var-definition
				      :token-type-list-reduced-from (list 'double-definition)))
       (double-definition (make-instance 'seq-reducer
					 :token-type-reduced 'double-definition
					 :token-type-list-reduced-from (list 'keyword-double
									     'var
									     'tok-equal
									     'val
									     'tok-semicomma)))
       ;;
       (function-call (make-instance 'seq-reducer
				     :token-type-reduced 'function-call
				     :token-type-list-reduced-from (list 'function-name
									 'function-call-argument-brace-list
									 'tok-semicomma)))
       (function-call-argument-brace-list (make-instance 'seq-reducer
							 :token-type-reduced 'function-call-argument-brace-list
							 :token-type-list-reduced-from (list 'tok-lp
											     'function-call-argument-list
											     'tok-rp)))
       (function-call-argument-list (make-instance 'any-reducer
						   :token-type-reduced 'function-call-argument-list
						   :token-type-list-reduced-from (list 'function-call-argument-single
										       'function-call-argument-multiple)))
       (function-call-argument-single (make-instance 'single-reducer
						     :token-type-reduced 'function-call-argument-single
						     :token-type-reduced-from 'var-or-val))
       (function-call-argument-multiple (make-instance 'seq-reducer
						       :token-type-reduced 'function-call-argument-multiple
						       :token-type-list-reduced-from (list 'function-call-argument-list
											   'tok-comma
											   'function-call-argument-single)))
       ;;
       (return-stmt (make-instance 'seq-reducer
				   :token-type-reduced 'return-stmt
				   :token-type-list-reduced-from (list 'keyword-return
								       'var-or-val
								       'tok-semicomma)))
       ;;
       (var-or-val (make-instance 'any-reducer
				  :token-type-reduced 'var-or-val
				  :token-type-list-reduced-from (list 'var
								      'val)))
       (var (make-instance 'single-reducer
			   :token-type-reduced 'var
			   :token-type-reduced-from 'string))
       (val (make-instance 'single-reducer
			   :token-type-reduced 'val
			   :token-type-reduced-from 'string))
       ;;
       (type (make-instance 'any-reducer
			    :token-type-reduced 'type
			    :token-type-list-reduced-from (list 'keyword-int
								'keyword-char
								'keyword-double)))
       (reducer-list (list
		      function-parameter-single
		      stmt-single
		      function-name
		      function-call-argument-single
		      var val type

		      function-definition function-parameter-brace-list function-parameter-list
		      function-parameter-multiple
		      function-parameter-declaration int-declaration function-brace-body
		      stmt-list		 stmt-multiple
		      var-definition double-definition

		      function-call function-call-argument-brace-list function-call-argument-list
		      function-call-argument-multiple

		      return-stmt var-or-val))
       (parser (make-instance 'parser
			      :parser-streamer parser-streamer
			      :reducer-list reducer-list
			      :token-type-final 'function-call)))
  (with-slots (reducer-index-stack) parser
    (setq reducer-index-stack (list (- 1 1))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; 
  (destructuring-bind (status token-parsed) (parser-parse parser)
    (lisp-unit2:assert-equal 'parser::full-parsed status)
    ;; (lisp-unit2:assert-true (lexer:token-typep token-parsed 'expr))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; simple c language function definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let* ((tok-keyword-int (make-instance 'lexer:token :token-type 'keyword-int :token-buf "int"))
       (tok-keyword-char (make-instance 'lexer:token :token-type 'keyword-char :token-buf "char"))
       (tok-keyword-double (make-instance 'lexer:token :token-type 'keyword-double :token-buf "double"))
       (tok-keyword-return (make-instance 'lexer:token :token-type 'keyword-return :token-buf "return"))
       (tok-string-fun (make-instance 'lexer:token :token-type 'string :token-buf "fun"))
       (tok-string-argc (make-instance 'lexer:token :token-type 'string :token-buf "argc"))
       (tok-string-i (make-instance 'lexer:token :token-type 'string :token-buf "i"))
       (tok-string-j (make-instance 'lexer:token :token-type 'string :token-buf "j"))
       (tok-string-0 (make-instance 'lexer:token :token-type 'string :token-buf "0"))
       (tok-string-1 (make-instance 'lexer:token :token-type 'string :token-buf "1"))
       (tok-string-add (make-instance 'lexer:token :token-type 'string :token-buf "add"))
       (tok-lp (make-instance 'lexer:token :token-type 'tok-lp :token-buf "("))
       (tok-rp (make-instance 'lexer:token :token-type 'tok-rp :token-buf ")"))
       (tok-lb (make-instance 'lexer:token :token-type 'tok-lb :token-buf "{"))
       (tok-rb (make-instance 'lexer:token :token-type 'tok-rb :token-buf "}"))
       (tok-comma (make-instance 'lexer:token :token-type 'tok-comma :token-buf ","))
       (tok-semicomma (make-instance 'lexer:token :token-type 'tok-semicomma :token-buf ";"))
       (tok-equal (make-instance 'lexer:token :token-type 'tok-equal :token-buf "="))
       ;; char fun(int argc)
       ;; {
       ;; 	double i = 1;
       ;; 	add(i, j);
       ;; 	return 0;
       ;; }
       (token-list (list tok-keyword-char tok-string-fun tok-lp tok-keyword-int tok-string-argc tok-rp
			 tok-lb
			 tok-keyword-double tok-string-i tok-equal tok-string-1 tok-semicomma
			 tok-string-add tok-lp tok-string-i tok-comma tok-string-j tok-rp tok-semicomma
			 tok-keyword-return tok-string-0 tok-semicomma
			 tok-rb))
       ;; 
       (token-streamer (make-instance 'utils:streamer :source token-list))
       (parser-streamer (make-instance 'parser-streamer :token-streamer token-streamer))
       ;;;;;;;;;;;;;;;;
       ;;
       ;; function-definition := seq of type function-name function-parameter-brace-list function-brace-body
       ;; function-parameter-brace-list := seq of ( function-parameter-list  )
       ;; function-parameter-list := any of function-parameter-single function-parameter-multiple
       ;; function-parameter-single := function-parameter-declaration
       ;; function-parameter-multiple := seq of function-parameter-list , function-parameter-single
       ;; function-name := any string

       ;; function-parameter-declaration := any of int-declaration
       ;; int-declaration := seq of keyword-int var

       ;; function-brace-body := seq of { stmt-list }

       ;; stmt-list := any of stmt-single stmt-multiple
       ;; stmt-single = any of var-definition function-call return-stmt
       ;; stmt-multiple := seq of stmt-list stmt-single

       ;; var-definition := any of int-definition
       ;; int-definition := seq of keyword-int var = val ;

       ;; function-call := seq of function-name function-call-argument-brace-list ;
       ;; function-call-argument-brace-list := seq of ( function-call-argument-list )
       ;; function-call-argument-list := any of function-call-argument-single function-call-argument-multiple
       ;; function-call-argument-single := var-or-val
       ;; function-call-argument-multiple := seq of function-call-argument-list , function-call-argument-single

       ;; return-stmt := seq of keyword-return var-or-val ;

       ;; var-or-val := any of var val
       ;; var := any string
       ;; val := any string

       ;; type := any of (keyword-int, keyword-char)
       ;;;;;;;;;;;;;;;;
       (function-definition (make-instance 'seq-reducer
					   :token-type-reduced 'function-definition
					   :token-type-list-reduced-from (list 'type
									       'function-name
									       'function-parameter-brace-list
									       'function-brace-body)))
       (function-parameter-brace-list (make-instance 'seq-reducer
						     :token-type-reduced 'function-parameter-brace-list
						     :token-type-list-reduced-from (list 'tok-lp
											 'function-parameter-list
											 'tok-rp)))
       (function-parameter-list (make-instance 'any-reducer
					       :token-type-reduced 'function-parameter-list
					       :token-type-list-reduced-from (list 'function-parameter-single
										   'function-parameter-multiple)))
       (function-parameter-single (make-instance 'single-reducer
						 :token-type-reduced 'function-parameter-single
						 :token-type-reduced-from 'function-parameter-declaration))
       (function-parameter-multiple (make-instance 'seq-reducer
						   :token-type-reduced 'function-parameter-multiple
						   :token-type-list-reduced-from (list 'function-parameter-list
										       'tok-comma
										       'function-parameter-single)))
       (function-name (make-instance 'single-reducer
				     :token-type-reduced 'function-name
				     :token-type-reduced-from 'string))
       ;; 
       (function-parameter-declaration (make-instance 'any-reducer
						      :token-type-reduced 'function-parameter-declaration
						      :token-type-list-reduced-from (list 'int-declaration)))
       (int-declaration (make-instance 'seq-reducer
				       :token-type-reduced 'int-declaration
				       :token-type-list-reduced-from (list 'keyword-int
									   'var)))
       ;;
       (function-brace-body (make-instance 'seq-reducer
					   :token-type-reduced 'function-brace-body
					   :token-type-list-reduced-from (list 'tok-lb
									       'stmt-list
									       'tok-rb)))
       ;;
       (stmt-list (make-instance 'any-reducer
				 :token-type-reduced 'stmt-list
				 :token-type-list-reduced-from (list 'stmt-single
								     'stmt-multiple)))
       (stmt-single (make-instance 'any-reducer
				   :token-type-reduced 'stmt-single
				   :token-type-list-reduced-from (list 'var-definition
								       'function-call
								       'return-stmt)))
       (stmt-multiple (make-instance 'seq-reducer
				     :token-type-reduced 'stmt-multiple
				     :token-type-list-reduced-from (list 'stmt-list
									 'stmt-single)))
       ;;
       (var-definition (make-instance 'any-reducer
				      :token-type-reduced 'var-definition
				      :token-type-list-reduced-from (list 'double-definition)))
       (double-definition (make-instance 'seq-reducer
					 :token-type-reduced 'double-definition
					 :token-type-list-reduced-from (list 'keyword-double
									     'var
									     'tok-equal
									     'val
									     'tok-semicomma)))
       ;;
       (function-call (make-instance 'seq-reducer
				     :token-type-reduced 'function-call
				     :token-type-list-reduced-from (list 'function-name
									 'function-call-argument-brace-list
									 'tok-semicomma)))
       (function-call-argument-brace-list (make-instance 'seq-reducer
							 :token-type-reduced 'function-call-argument-brace-list
							 :token-type-list-reduced-from (list 'tok-lp
											     'function-call-argument-list
											     'tok-rp)))
       (function-call-argument-list (make-instance 'any-reducer
						   :token-type-reduced 'function-call-argument-list
						   :token-type-list-reduced-from (list 'function-call-argument-single
										       'function-call-argument-multiple)))
       (function-call-argument-single (make-instance 'single-reducer
						     :token-type-reduced 'function-call-argument-single
						     :token-type-reduced-from 'var-or-val))
       (function-call-argument-multiple (make-instance 'seq-reducer
						       :token-type-reduced 'function-call-argument-multiple
						       :token-type-list-reduced-from (list 'function-call-argument-list
											   'tok-comma
											   'function-call-argument-single)))
       ;;
       (return-stmt (make-instance 'seq-reducer
				   :token-type-reduced 'return-stmt
				   :token-type-list-reduced-from (list 'keyword-return
								       'var-or-val
								       'tok-semicomma)))
       ;;
       (var-or-val (make-instance 'any-reducer
				  :token-type-reduced 'var-or-val
				  :token-type-list-reduced-from (list 'var
								      'val)))
       (var (make-instance 'single-reducer
			   :token-type-reduced 'var
			   :token-type-reduced-from 'string))
       (val (make-instance 'single-reducer
			   :token-type-reduced 'val
			   :token-type-reduced-from 'string))
       ;;
       (type (make-instance 'any-reducer
			    :token-type-reduced 'type
			    :token-type-list-reduced-from (list 'keyword-int
								'keyword-char
								'keyword-double)))
       (reducer-list (list
		      function-parameter-single
		      stmt-single
		      function-name
		      function-call-argument-single
		      var val type

		      function-definition function-parameter-brace-list function-parameter-list
		      function-parameter-multiple
		      function-parameter-declaration int-declaration function-brace-body
		      stmt-list		 stmt-multiple
		      var-definition double-definition
		      function-call function-call-argument-brace-list function-call-argument-list
		      function-call-argument-multiple
		      return-stmt var-or-val))
       (parser (make-instance 'parser
			      :parser-streamer parser-streamer
			      :reducer-list reducer-list
			      :token-type-final 'function-definition)))
  (with-slots (reducer-index-stack) parser
    (setq reducer-index-stack (list (- 1 1))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; 
  (destructuring-bind (status token-parsed) (parser-parse parser)
    (lisp-unit2:assert-equal 'parser::full-parsed status)
    ;; (lisp-unit2:assert-true (lexer:token-typep token-parsed 'expr))
    (print token-parsed)
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; simple c language function definition (with lexer)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let* ((input-string "char fun(int argc)
       {
       	double i = 1;
       	add(i, j);
       	return 0;
       }")
       (c-code (make-instance 'lexer::filter-group :filter-group-name 'c-code))
       (lexer)
       ;; 
       (lexer-streamer)
       (parser-streamer)
       ;;;;;;;;;;;;;;;;
       ;; production
       ;; type := any of (keyword-int, keyword-char)
       ;;;;;;;;;;;;;;;;
       (function-definition (make-instance 'seq-reducer
					   :token-type-reduced 'function-definition
					   :token-type-list-reduced-from (list 'type
									       'function-name
									       'function-parameter-brace-list
									       'function-brace-body)))
       (function-parameter-brace-list (make-instance 'seq-reducer
						     :token-type-reduced 'function-parameter-brace-list
						     :token-type-list-reduced-from (list 'tok-lp
											 'function-parameter-list
											 'tok-rp)))
       (function-parameter-list (make-instance 'any-reducer
					       :token-type-reduced 'function-parameter-list
					       :token-type-list-reduced-from (list 'function-parameter-single
										   'function-parameter-multiple)))
       (function-parameter-single (make-instance 'single-reducer
						 :token-type-reduced 'function-parameter-single
						 :token-type-reduced-from 'function-parameter-declaration))
       (function-parameter-multiple (make-instance 'seq-reducer
						   :token-type-reduced 'function-parameter-multiple
						   :token-type-list-reduced-from (list 'function-parameter-list
										       'tok-comma
										       'function-parameter-single)))
       (function-name (make-instance 'single-reducer
				     :token-type-reduced 'function-name
				     :token-type-reduced-from 'string))
       ;; 
       (function-parameter-declaration (make-instance 'any-reducer
						      :token-type-reduced 'function-parameter-declaration
						      :token-type-list-reduced-from (list 'int-declaration)))
       (int-declaration (make-instance 'seq-reducer
				       :token-type-reduced 'int-declaration
				       :token-type-list-reduced-from (list 'keyword-int
									   'var)))
       ;;
       (function-brace-body (make-instance 'seq-reducer
					   :token-type-reduced 'function-brace-body
					   :token-type-list-reduced-from (list 'tok-lb
									       'stmt-list
									       'tok-rb)))
       ;;
       (stmt-list (make-instance 'any-reducer
				 :token-type-reduced 'stmt-list
				 :token-type-list-reduced-from (list 'stmt-single
								     'stmt-multiple)))
       (stmt-single (make-instance 'any-reducer
				   :token-type-reduced 'stmt-single
				   :token-type-list-reduced-from (list 'var-definition
								       'function-call
								       'return-stmt)))
       (stmt-multiple (make-instance 'seq-reducer
				     :token-type-reduced 'stmt-multiple
				     :token-type-list-reduced-from (list 'stmt-list
									 'stmt-single)))
       ;;
       (var-definition (make-instance 'any-reducer
				      :token-type-reduced 'var-definition
				      :token-type-list-reduced-from (list 'double-definition)))
       (double-definition (make-instance 'seq-reducer
					 :token-type-reduced 'double-definition
					 :token-type-list-reduced-from (list 'keyword-double
									     'var
									     'tok-equal
									     'val
									     'tok-semicomma)))
       ;;
       (function-call (make-instance 'seq-reducer
				     :token-type-reduced 'function-call
				     :token-type-list-reduced-from (list 'function-name
									 'function-call-argument-brace-list
									 'tok-semicomma)))
       (function-call-argument-brace-list (make-instance 'seq-reducer
							 :token-type-reduced 'function-call-argument-brace-list
							 :token-type-list-reduced-from (list 'tok-lp
											     'function-call-argument-list
											     'tok-rp)))
       (function-call-argument-list (make-instance 'any-reducer
						   :token-type-reduced 'function-call-argument-list
						   :token-type-list-reduced-from (list 'function-call-argument-single
										       'function-call-argument-multiple)))
       (function-call-argument-single (make-instance 'single-reducer
						     :token-type-reduced 'function-call-argument-single
						     :token-type-reduced-from 'var-or-val))
       (function-call-argument-multiple (make-instance 'seq-reducer
						       :token-type-reduced 'function-call-argument-multiple
						       :token-type-list-reduced-from (list 'function-call-argument-list
											   'tok-comma
											   'function-call-argument-single)))
       ;;
       (return-stmt (make-instance 'seq-reducer
				   :token-type-reduced 'return-stmt
				   :token-type-list-reduced-from (list 'keyword-return
								       'var-or-val
								       'tok-semicomma)))
       ;;
       (var-or-val (make-instance 'any-reducer
				  :token-type-reduced 'var-or-val
				  :token-type-list-reduced-from (list 'var
								      'val)))
       (var (make-instance 'single-reducer
			   :token-type-reduced 'var
			   :token-type-reduced-from 'string))
       (val (make-instance 'single-reducer
			   :token-type-reduced 'val
			   :token-type-reduced-from 'string))
       ;;
       (type (make-instance 'any-reducer
			    :token-type-reduced 'type
			    :token-type-list-reduced-from (list 'keyword-int
								'keyword-char
								'keyword-double)))
       (reducer-list (list
		      function-parameter-single
		      stmt-single
		      function-name
		      function-call-argument-single
		      var val type

		      function-definition function-parameter-brace-list function-parameter-list
		      function-parameter-multiple
		      function-parameter-declaration int-declaration function-brace-body
		      stmt-list		 stmt-multiple
		      var-definition double-definition
		      function-call function-call-argument-brace-list function-call-argument-list
		      function-call-argument-multiple
		      return-stmt var-or-val))
       (parser))
  ;;;;;;;;;;;;;;;;
  ;; lexer
  ;;;;;;;;;;;;;;;;
  (lexer:add-filter c-code (lexer:token-filter :string "char" :token-type 'keyword-char))
  (lexer:add-filter c-code (lexer:token-filter :string "int" :token-type 'keyword-int))
  (lexer:add-filter c-code (lexer:token-filter :string "double" :token-type 'keyword-double))
  (lexer:add-filter c-code (lexer:token-filter :string "return" :token-type 'keyword-return))
  (lexer:add-filter c-code (lexer:token-filter :string "=" :token-type 'tok-equal))
  (lexer:add-filter c-code (lexer:token-filter :string "," :token-type 'tok-comma))
  (lexer:add-filter c-code (lexer:token-filter :string ";" :token-type 'tok-semicomma))
  (lexer:add-filter c-code (lexer:token-filter :string "(" :token-type 'tok-lp))
  (lexer:add-filter c-code (lexer:token-filter :string ")" :token-type 'tok-rp))
  (lexer:add-filter c-code (lexer:token-filter :string "{" :token-type 'tok-lb))
  (lexer:add-filter c-code (lexer:token-filter :string "}" :token-type 'tok-rb))
  (lexer:add-filter c-code (lexer:token-filter :many-any-char (list #\Space #\Tab #\Return #\Linefeed #\Newline #\Page)))
  (lexer:add-filter c-code (lexer:token-filter :function (utils:many
							  (utils:make-filter (utils:make-matcher :single-char-test #'alphanumericp)))
					       :token-type 'string))
  (lexer:add-filter c-code (lexer:token-filter :others t))

  ;; 
  (with-input-from-string (string-stream input-string)
    (setq lexer (make-instance 'lexer:lexer
			       :stream (make-instance 'utils:cstreamer :source string-stream)
			       :filter-group-list (list c-code)
			       :filter-group-state-stack (list 'c-code)))

    #+nil
    (loop
       with token
       do (progn
	    (setq token (lexer::lexer-read-token lexer))
	    (if token
		(print token)
		(return))))

    ;;;;;;;;;;;;;;;;
    ;; parser
    ;;;;;;;;;;;;;;;;
    (setq lexer-streamer (make-instance 'lexer:lexer-streamer :lexer lexer))
    (setq parser-streamer (make-instance 'parser-streamer :token-streamer lexer-streamer))
    (setq parser (make-instance 'parser
    			      :parser-streamer parser-streamer
    			      :reducer-list reducer-list
    			      :token-type-final 'function-definition))
    (with-slots (reducer-index-stack) parser
      (setq reducer-index-stack (list (- 1 1))))
    (destructuring-bind (status token-parsed) (parser-parse parser)
      (lisp-unit2:assert-equal 'parser::full-parsed status)
      ;; (lisp-unit2:assert-true (lexer:token-typep token-parsed 'expr))
      (print token-parsed)
      ))
  ))
