(define *dictionary* '((*empty* 0)))

(define-syntax match-dict
  (sc-macro-transformer
   (lambda (exp env)
     (let ((dict (cadr exp))
	   (body (caddr exp)))
       (pp `(let ((*dictionary* (append ,dict *dictionary*)))
	  (pp ,dict)
	  (pp *dictionary*)
	  ,body))))))

(define-syntax ref-t
  (sc-macro-transformer
   (lambda (exp env)
     (pp `(begin (pp *dictionary*)
	     (assq ,(cadr exp) *dictionary*))))))

(define (ref symbol)
  (pp *dict*)
  (assq symbol *dictionary*))

(match-dict '((a 1) (b 2)) (+ (ref 'a) (ref 'b)))

(let ((*dictionary* (append '((a 1) (b 2)) *dictionary*)))
  (pp '((a 1) (b 2)))
  (pp *dictionary*)
  (assq 'a *dictionary*)
  (ref-t 'a))
