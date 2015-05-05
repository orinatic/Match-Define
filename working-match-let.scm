;;Our final nice version

(define *d* '())

;;removes duplicates out of a list
(define (remove-duplicates l)
  (cond ((null? l)
         '())
        ((member (car l) (cdr l))
         (remove-duplicates (cdr l)))
        (else
         (cons (car l) (remove-duplicates (cdr l))))))

;;given a match statement, it pulls variables out of the match statement
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
;(d c b a)

;;This puts in the calls to ref as required.  
;;Replaces the variables in vars in the code with the calls to ref
;;that look up their values in the dictionary.  
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
; ((- (+ (ref *d* (quote c)) (ref *d* (quote d))) 2 (* (ref *d* (quote a)) (ref *d* (quote c))) (sqrt (* f (ref *d* (quote b))))) (pp (ref *d* (quote a))))

;;gets the value of symbol from the dictionary
(define (ref dict symbol)
  (cadr (assq symbol dict)))

;;match-let is a macro which "lets" the variables matched in the match
;;statement be the values that they match for the duration of the let
;;statement.  
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
;6

(let ((token '(bin-arith + 2 4)))
  (match-let token `(bin-arith (? op) (? a1 ,number?) (? a2 ,number?))
	     (pp (list op a1 a2))))
;(+ 2 4) 

