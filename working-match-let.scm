(define *dictionary* '((*empty* 0)))

(match-let key '((? a) (? b))
	   (+ a b))

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

(define (process-body todo done vars)
  (if (null? todo)
      (reverse done)
      (let ((token (car todo)))
	(cond 
	 ((memq token vars) 
	  (process-body (cdr todo) (cons `(ref *d* ',token) done) vars))
	 ((pair? token)
	  (process-body (cdr todo) 
			(cons (process-body token '() vars) done) 
			vars))
	 (else 
	  (process-body (cdr todo) (cons token done) vars))))))

(process-body '((- (+ c d)  
		   2 
		   (* a c) 
		   (sqrt (* f b))) 
		(pp a))
	      '()
	      '(a b c d))

(define (ref dict symbol)
  (cadr (assq symbol dict)))


(define-syntax match-let
  (sc-macro-transformer
   (lambda (exp env)
     (let* ((key (close-syntax (cadr exp) env))
	   (pattern (caddr exp))
	   (vars (find-variables pattern '()))
	   (body (process-body (cdddr exp) '() vars)))
       `(let ((*d* (append ((matcher ,pattern) ,key) *d*)))
	  (begin ,@(map (lambda (statement) statement)
			body)))))))

(match-let '(1 2 3 4) '((? a) (? b) (? c) 4)
	   (+ a b c))
      
(let ((token '(bin-arith + 2 4)))
  (match-let token `(bin-arith (? op) (? a1 ,number?) (? a2 ,number?))
	     (pp (list op a1 a2))))