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

(define testlist '((x 1) (y 2)))


(let (((caar testlist) (cadar testlist))
      ((caadr testlist) (cdadr testlist)))
  (pp "here"))
