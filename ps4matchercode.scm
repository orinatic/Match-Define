;;Matcher code from Pset 4 that we need to add to the generic matcher code.  

;;Taken from Eli's pset 4, although if we can find a version that
;;doesn't have the pletrec hack, that'd be good.  

(define (result-receiver dict n-eaten) `(success ,dict ,n-eaten))

(define (match:choice . match-combinators)
  (define (choice-match data dictionary succeed)
    (and (pair? data)
	 (let lp ((matcher match-combinators))
	   (if (pair? matcher)
	       (let ((match ((car matcher) data dictionary succeed)))
		 (if match
		     match
		     (lp (cdr matcher))))
	       #f))))
  choice-match)

(define (match:choice? pattern)
 (and (pair? pattern)
      (eq? (car pattern) '?:choice)))

(defhandler match:->combinators
  (lambda (pattern) 
    (apply match:choice (map match:->combinators pattern)))
  match:choice?)

;;Pletrec.  If we can, we need to get rid of this global hash table.  
(define named-patterns (make-strong-eqv-hash-table))

(define (match:pletrec? pattern)
 (and (pair? pattern)
      (eq? (car pattern) '?:pletrec)))

(define (match:ref? pattern)
  (and (pair? pattern)
       (eq? (car pattern) '?:ref)))

(defhandler match:->combinators
  (lambda (pattern)
    (for-each (lambda (def)
		(hash-table/put! 
		 named-patterns 
		 (car def) 
		 (match:->combinators (cadr def))))
	      (cadr pattern))
    (match:->combinators (caddr pattern)))
  match:pletrec?)

(defhandler match:->combinators
  (lambda (pattern)
    (match:ref 
     (match:variable-name pattern) 
     named-patterns))
  match:ref?)

(define (match:ref reference env-dict)
  (define (ref-match data dictionary succeed)
    (and (pair? data)
	 ((hash-table/get env-dict reference -1)
	  data dictionary succeed)))
  ref-match)

