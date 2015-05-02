;;The code for let.  

(define-syntax dict-let
  (sc-macro-transformer
   (lambda (exp env)
     (let ((dict (cadr exp))
	    (body (caddr exp)))
      ;(pp dict)
     ; (pp
	`(let (
	       ,@(map (lambda(entry)
;			     (let ((val 
;			      (make-syntactic-closure env '() (cadr entry))))
			       `(,(car entry) ,(cadr entry)))
		  dict))
	   ,body)))));)

(define d 4)
(dict-let ((a d) (b cos) (c 3)) (+ c a))
;7 
;Success!!

#|
;For when we get a generic version working
(define testAList '((a d) (b cos) (c 3)))
(dict-let testAList (+ c a))
|#
(let ((((? x) (? y)) (1 2))
      (c 4))
  stuff)

 
(match-let ((((? y) (? x)) ((1 2)))
	     (d 5))
	   (+ d x y))

(match-let ((((? y)) ((1 2)))
	     );(d 5))
	   (+ x y))

(match-let ((((? y)) ((4 2)))
	     (d 5))
	   (pp d)
	   (pp x)
	   (pp y)
	   d)

(define (*run-match* vars pattern)
  ((match:->combinators vars) pattern '() (lambda (d n) d)))

(define (assign-iter todo done)
  (if (null? todo)
      (filter pair? done)
      (let* ((assign (car todo))
	     (vars (car assign))
	     (vals (cadr assign)))
	(if (identifier? vars)
	    (assign-iter (cdr todo) (append done (list assign)))
	    (assign-iter 
	     (cdr todo)
	     (append done (or (*run-match* vars vals)
			      (begin (warn 'match-failed-in-let) 
				     '()))))))))


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

(match-let ((((? y) (? x)) ((1 2)))
	     (d 5))
	   (+ d x y)) ;-> 8

(match-let ((((? y) (? x)) ((1 2))))
	   (+ x y)) ;-> 3

(match-let ((((? r)) ((4 2)))
	     (d 5))
	   d)
;-> Warning: match-failed-in-let
;5

(define-syntax dict-let*
  (sc-macro-transformer
   (lambda (exp env)
     (let ((dict (cadr exp))
	    (body (caddr exp)))
      ;(pp
	`(let* (
	       ,@(map (lambda(entry)
			    ; (let ((val 
			    ;  (make-syntactic-closure env '() (cadr entry))))
			       `(,(car entry) ,(cadr entry)));)
		  dict))
	   ,body)))));)

(dict-let* ((a 4) (b cos) (c a)) (+ c a))
;8 
;Success!

;No match-let* because currently it will be useless


(define-syntax match-let*
  (sc-macro-transformer
   (lambda (exp env)
     (let* ((body (cddr exp))
	    (dict (assign-iter (cadr exp) '())))
       (pp dict)
       `(let* ( ,@(map (lambda(entry)
			`(,(car entry) ,(cadr entry)))
		      dict))
	   (begin ,@(map (lambda (statement)
		    statement)
		  body))) 
	   ))))

(match-let* ((((? y) (? x)) ((1 2)))
	     (d (+ y x)))
	    (+ d x y)) ;-> 6

(match-let* ((((? y) (? x)) ((1 2)))
	    (((? c) (? d)) (((+ 3 x) y))))
	   (+ c d x y)) ;-> 9


(define-syntax match-letrec
  (sc-macro-transformer
   (lambda (exp env)
     (let* ((body (cddr exp))
	    (dict (assign-iter (cadr exp) '())))
       `(letrec ( ,@(map (lambda(entry)
			`(,(car entry) ,(cadr entry)))
		      dict))
	   (begin ,@(map (lambda (statement)
		    statement)
		  body))) 
	   ))))
