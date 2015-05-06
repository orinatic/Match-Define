;;;; File:  load.scm -- Loader for matching system

; Pattern matcher:

(load "ghelper")
(load "matcher")


; Term rewriting / pattern-directed invocation system:

(define (rule-memoize f) f) ; A stub put in place in case you want to
                            ; play with memoization in the term
                            ; rewriter

(load "utils")
;(load "rule-implementation")
;(load "rules")

(load "pattern-operator")


(set! user-initial-environment (make-top-level-environment))
(environment-define user-initial-environment 
                    'generic-evaluation-environment
                    (extend-top-level-environment user-initial-environment))
(define generic-evaluation-environment 
  (access generic-evaluation-environment user-initial-environment))

(load "previous-attempts/sub-interpreter/utils" user-initial-environment)
(load "previous-attempts/sub-interpreter/ghelper" user-initial-environment)
(load "previous-attempts/sub-interpreter/matcher" user-initial-environment)
(load "previous-attempts/sub-interpreter/syntax" user-initial-environment)
(load "previous-attempts/sub-interpreter/rtdata" user-initial-environment)

(load "previous-attempts/sub-interpreter/interp" generic-evaluation-environment)
(load "previous-attempts/sub-interpreter/repl" generic-evaluation-environment)
;;; This allows nonstrict definitions.
;;(load "general-procedures" generic-evaluation-environment)
;;(load "kons" generic-evaluation-environment)

(ge generic-evaluation-environment)
