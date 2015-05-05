(load "~/Documents/6.945/Match-Define/ghelper")
(load "~/Documents/6.945/Match-Define/matcher")


; Term rewriting / pattern-directed invocation system:

(define (rule-memoize f) f) ; A stub put in place in case you want to
                            ; play with memoization in the term
                            ; rewriter

(load "~/Documents/6.945/Match-Define/utils")
(load "~/Documents/6.945/Match-Define/pattern-operator")

(set! user-initial-environment (make-top-level-environment))
(environment-define user-initial-environment 
                    'generic-evaluation-environment
                    (extend-top-level-environment user-initial-environment))
(define generic-evaluation-environment 
  (access generic-evaluation-environment user-initial-environment))

(load "~/Documents/6.945/Match-Define/sub-interpreter/utils" user-initial-environment)
(load "~/Documents/6.945/Match-Define/sub-interpreter/ghelper" user-initial-environment)
(load "~/Documents/6.945/Match-Define/sub-interpreter/matcher" user-initial-environment)
(load "~/Documents/6.945/Match-Define/sub-interpreter/syntax" user-initial-environment)
(load "~/Documents/6.945/Match-Define/sub-interpreter/rtdata" user-initial-environment)

(load "~/Documents/6.945/Match-Define/sub-interpreter/interp" generic-evaluation-environment)
(load "~/Documents/6.945/Match-Define/sub-interpreter/repl" generic-evaluation-environment)
;;; This allows nonstrict definitions.
;;(load "general-procedures" generic-evaluation-environment)
;;(load "kons" generic-evaluation-environment)

(ge generic-evaluation-environment)

;(load "~/Documents/6.945/Match-Define/sub-interpreter/match-repl.scm")
