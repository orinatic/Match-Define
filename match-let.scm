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

(define-syntax dict-let
  (sc-macro-transformer
   (lambda (exp env)
     (let* ((dict (cdr exp)))
       (pp
	`(begin
	   ,@(let (
		   ,@((map (lambda(entry)
			     (let ((val 
			      (make-syntactic-closure env '() (cadr entry))))
			       `(,(car entry) ,val))))
		  dict)))))))))


(define d 4)
(dict-let (a d) (b cos) (c 3))
