(in-package :parser-test)

;;; expr := (term | add-expr | subtract-expr)
;;; add-expr := expr tok-add expr
;;; subtract-expr := expr tok-subtract expr
;;; term := (tok-number | mul-term | div-term | paranthesis-expr)
;;; mul-term := term tok-star term
;;; div-term := term tok-div term
;;; paranethesis-expr := tok-l-p expr tok-r-p
;;; number := tok-number
;;;
;;;
;;;
;;; expr := (number | add-expr | sub-expr | p-expr)
;;; add-expr := expr tok-add expr
;;; sub-expr := expr tok-sub expr
;;; p-expr := tok-lp expr tok-rp
;;; number := tok-number
;;;
;;; 1 + ( 2 - 3 ) + 4
;;; tok1 (:token-type 'term)
;;; tok2 (
;;;
;;;
;;;
;;; expr   := (expr-1 | expr-2 | expr3)
;;; expr-1 := expr tok-add term
;;; expr-2 := expr tok-sub term
;;; expr-3 := term
;;; term   := (term-1 | term-2)
;;; term-1 := term tok-mul factor
;;; term-2 := factor
;;; factor := (factor-1 factor-2)
;;; factor-1 := number
;;; factor-2 := tok-lp expr tok-rp

(lisp-unit2:define-test parser-test::parser--complex-calculator
    (:tags '(parser-test::tag-parser))
  )

