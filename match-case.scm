;(match-case to-match
;	    (('bin-op (? op) (? a1) (? a2)) 
;	     <do stuff>)
;	    (('un-op (? op) (? a))
;	     <do other stuff>)
;	    (else 
;	     <do other other stuff>))
; if a body returns false, we will have fall-through?

(define (match-case key . clauses)
  (let case-iter ((todo clauses))
    (let* ((clause (car clauses))
	   (pred (car clause))
	   (body (cadr clause))
	   (match (*run-match* pred key)))
      (if match
	  (dict-let match body)
	  (case-iter (cdr clauses))))))

(match-case 'to-match
	    '((bin-op (? op) (? a1) (? a2)) 
	     ()
	    '((un-op (? op) (? a))
	     (do-other-stuff))
	    '(else 
	     (do-more-stuff)))
		       
(or #t hello) ;-> #t

(if #t
    #t
    hello)

(define-syntax match-let
  (sc-macro-transformer
   (lambda (exp env)
     (let* ((body (cddr exp))
	    (dict (assign-iter (cadr exp) '())))
       `(let ( ,@(map (lambda(entry)
			`(,(car entry) ,(cadr entry)))
		      dict))
	   (begin ,@(map (lambda (statement)
		    statement)
		  body))) 
	   ))))

(define-syntax dict-let
  (sc-macro-transformer
   (lambda (exp env)
     (let ((dict (cadr exp))
	    (body (caddr exp)))
	`(let (
	       ,@(map (lambda(entry)
			       `(,(car entry) ,(cadr entry)))
		  dict))
	   ,body)))));)

(define (dict-let dict body)
  (let ((keys (dict->keys dict))
	(vals (dict->vals dict)))
    (lambda 

(dict->lists '((a 1) (b 2)))

(define (dict->keys dict)
  (map (lambda (elt) (car elt)) dict))

(define (dict->vals dict)
	(map (lambda (elt) (cadr elt)) dict))  

(define (dict-let dict body)
  (lambda ()
    (define (empty a) 'nothing)
    (let ((our-env (procedure-environment empty)))
      (pp (environment-bindings our-env))
      (let dict-define-loop ((todo dict))
	(if (null? todo)
	    'done
	    (let ((var (caar todo))
		  (val (cadar todo)))
	      (environment-define our-env var val)
	      (dict-define-loop (cdr todo)))))
      (pp (environment-bindings our-env))
      (eval body our-env))))

(define-syntax dict-let
  (sc-macro-transformer
   (lambda (exp env)
     (let ((dict (cadr exp))
	   (body (caddr exp)))
       `(lambda ()
	 (define (empty a) 'nothing)
	 (let ((our-env (procedure-environment empty)))
	   (pp (environment-bindings our-env))
	   (let dict-define-loop ((todo ,dict))
	     (if (null? todo)
		 'done
		 (let ((var (caar todo))
		       (val (cadar todo)))
		   (environment-define our-env var val)
		   (dict-define-loop (cdr todo)))))
	   (pp (environment-bindings our-env))
	   (eval ,body our-env)))))))

((dict-let '((g 5)) g))

