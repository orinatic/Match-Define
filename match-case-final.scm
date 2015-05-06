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
		clauses)
	 (else 'no-match))))))


(match-case '(3 100 buffalos beefalo)
            (`(harold (? a) (? b ,number?) (?? c)) 
	     (pp `(harold is ,a ,b ,c)))
            (((?:choice a (? c)) 100 (?:choice buffalos buffaloes) 
	      (?? b))(pp b))) 
