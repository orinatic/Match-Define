;;The code for let.  
#|(define-syntax m-let
  (syntax-rules ()
    ((_ (vars) body ...)
     (let ((letvars (map car vars))
	   (letexprs (map cadr vars)))
       
|#

;;dummy test
'((x 1) (y 2))

(let ((x 1) (y 2))
  (pp x)
  (pp y))

(define testlist ((x 1) (y 2)))

#|
(let (((caar testlist) (cadar testlist))
      ((caadr testlist) (cdadr testlist)))
  (pp "here"))
|#

(define-syntax m-let
  (sc-macro-transformer
   (lambda (exp env)
     (let* ((values (cdr exp))
	    (vars (map car values))
	    (exprs (map cadr values)))
      (pp vars)
      (pp exprs)
       (pp
       `(begin
	   ,@(map (lambda(v e)
		    (let ((w (make-syntactic-closure env '() v)))
		      `(define ,v ,e)))
		 vars exprs)))))))

(m-let '((a 1) (b 1) (c 1)))
