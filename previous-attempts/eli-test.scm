(define *d* '((*empty* 0)))

(define (remove-duplicates l)
  (cond ((null? l)
         '())
        ((member (car l) (cdr l))
         (remove-duplicates (cdr l)))
        (else
         (cons (car l) (remove-duplicates (cdr l))))))

(define (find-variables input)
  (let find-iter ((todo (list input)) (vars '()))
    (if (null? todo)
	(remove-duplicates vars)
	(let ((token (car todo)))
	  (cond 
	   ((match:element? token) 
	    (find-iter (cdr todo) (cons (cadr token) vars)))
	   ((match:segment? token) 
	    (find-iter (cdr todo) (cons (cadr token) vars)))
	   ((pair? token) 
	    (find-iter (cdr todo) 
			    (find-iter token vars)))
	   (else (find-iter (cdr todo) vars)))))))

(find-variables '(?? a))
(find-variables `((? a) ((?? b) (? a ,string?) ((? c) (? d)))))

(define (match:lookup dict symbol)
  (cadr (assq symbol dict)))

(define (match:lookup? exp)
  (tagged-list? exp 'match:lookup))

(define-syntax match-let
  (sc-macro-transformer
   (lambda (exp env)
     (let* ((key (close-syntax (cadr exp) env))
	    (pattern (caddr exp))
	    (vars (find-variables pattern))
	    (body (cdddr exp)))
       (pp pattern)
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

(define (process-clauses todo done)
  (if (null? todo)
      (cons 'quote (list (reverse done)))
      (let* ((clause (car todo))
	     (pattern (car clause))
	     (body (cadr clause))
	     (vars (find-variables pattern)))
	(pp clause)
	(pp pattern)
	(pp body)
	(process-clauses 
	 (cdr todo) 
	 (cons (cons pattern 
		     `( (lambda ,vars ,body) 
			  ,@(map (lambda (var)
				   `(match:lookup *d* ',var))
				 vars)))
	       done)))))   
(pp (process-clauses
 '(((2 3 (? a)) (+ 2 3 a))
   ((1 2 3) (+ 1 2 3)))
'()))


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

(match-case '(1 2 3)
	    (`(2 3 (? a ,number?)) (+ 2 3 a))
	    (`(1 2 3) (+ 1 2 3)))
(match-case 'fooboo
	    (`(2 3 (? a ,number?)) (+ 2 3 a))
	    (`(1 2 3) (+ 1 2 3)))

(define (parse-token token)
  (match-case token
	      (`(bin-arith (? op) (? a1 ,number?) (? a2 ,number?)) 
	       (op a1 a2))
	      (`(un-arith (? op) (? a, number?)) (op a))
	      ('(?? a) (pp a))))

(parse-token `(bin-arith ,+ 1 4))
