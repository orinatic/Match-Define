(define *d* '((*empty* 0)))

(define (remove-duplicates l)
  (cond ((null? l)
         '())
        ((member (car l) (cdr l))
         (remove-duplicates (cdr l)))
        (else
         (cons (car l) (remove-duplicates (cdr l))))))

(define (find-variables todo vars)
  (if (null? todo)
      (remove-duplicates vars)
      (let ((token (car todo)))
	(cond 
	 ((match:element? token) 
	  (find-variables (cdr todo) (cons (cadr token) vars)))
	 ((match:segment? token) 
	  (find-variables (cdr todo) (cons (cadr token) vars)))
	 ((pair? token) 
	  (find-variables (cdr todo) 
			  (find-variables token vars)))
	  (else (find-variables (cdr todo) vars))))))

(find-variables `((? a) ((?? b) (? a ,string?) ((? c) (? d)))) '())

(define (match:lookup dict symbol)
  (cadr (assq symbol dict)))

(define (match:lookup? exp)
  (tagged-list? exp 'match:lookup))

(define-syntax match-let
  (sc-macro-transformer
   (lambda (exp env)
     (let* ((key (close-syntax (cadr exp) env))
	    (pattern (caddr exp))
	    (vars (find-variables pattern '()))
	    (body (cdddr exp)))
       `(fluid-let ((*d* (append ((matcher ,pattern) ,key) *d*)))
	  ((lambda ,vars
	     ,@(map (lambda (statement) 
		      (make-syntactic-closure env vars statement))
		    body)) 
	   ,@(map (lambda (var) 
		    `(match:lookup *d* ',var)) vars))))))))

(match-let '(5 6 7 8) '((? a) (?? d))
	   (cons a d))

(match-let '(1 2 3 4) '((? a) (? b) (? c) 4)
	   (match-let '(5 6) '((? a) (? d))
		      (pp b)
		      (+ a b c d)))

((lambda (a b)
   ((lambda (c d)
      (+ a b c d)) 1 2)) 3 4)

(let ((token `(bin-arith ,+ 2 4)))
  (match-let token `(bin-arith (? op) (? a1 ,number?) (? a2 ,number?))
	     (pp (op a1 a2))))

(define (parse-token token)
  (match-case token
	      ('(bin-arith (? op) (? a1 ,number?) (? a2 ,number?)) 
	       (op a1 a2))
	      ('(un-arith (? op) (? a, number?)) (op a))
	      ('(?? a) (pp a))))

(define (process-clauses todo done)
  (if (null? todo)
      (reverse done)
      (let ((pattern (caar todo))
	    (body (cadar todo)))
	(process-clauses 
	 (cdr todo) 
	 (cons (cons pattern 
		     (process-body body 
		     '() 
		     (find-variables pattern '())))
	       done)))))   

(process-clauses
 '(((bin-arith (? op) (? a1 ,number?) (? a2 ,number?)) 
   (op a1 a2))
  ((un-arith (? op) (? a, number?)) (op a))
  ((?? a) (pp a)))
 '())

(define-syntax match-case
  (sc-macro-transformer
   (lambda (exp env)
     (let ((key (close-syntax (cadr exp) env))
	   (clauses (cddr env)))


       `(let case-iter ((todo ,clauses))
	  (let* ((clause (car todo))
		 (pattern (car clause))
		 (body (,@handle-clause-body (cadr clause)))
       

)))))))


