(define-syntax match-case
  (sc-macro-transformer
   (lambda (exp env)
     (let* ((key (close-syntax (cadr exp) env))
	    (clauses (cddr exp)))
       `(cond 
	 ,@(map (lambda (clause)
		  (let ((pattern (car clause))
			(body (cadr clause)))
		    `(((matcher ,pattern) ,key) 
		      (match-let ,key ,pattern ,body))))
		clauses))))))
