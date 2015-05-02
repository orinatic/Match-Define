;;;; File:  load.scm -- Loader for pattern matching system

; Pattern matcher:

(load "~/Documents/6.945/Match-Define/ghelper")
(load "~/Documents/6.945/Match-Define/matcher")


; Term rewriting / pattern-directed invocation system:

(define (rule-memoize f) f) ; A stub put in place in case you want to
                            ; play with memoization in the term
                            ; rewriter

(load "~/Documents/6.945/Match-Define/utils")
(load "~/Documents/6.945/Match-Define/rule-implementation")
(load "~/Documents/6.945/Match-Define/rules")

(load "~/Documents/6.945/Match-Define/pattern-operator")
