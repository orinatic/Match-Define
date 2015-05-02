;;;Known Bugs

;When we evaluate match in a macro, we don't have access to the
;environment, so we don't catch other existing bindings in the
;environment.  So sometimes our matches fail when they shouldn't.  We
;should ask Professor Sussman about this on Monday.  This may be
;avoidable for let, but will be much trickier (if possible?) for
;define.  
