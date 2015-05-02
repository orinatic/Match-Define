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

(define-syntax match-let
  (sc-macro-transformer
   (lambda (exp env)
     (let* ((assigns (cadr exp))
	    (body (cddr exp))
	    (dict 
	     (filter
	      pair?
	      (let assign-iter ((todo assigns) (done '()))
		(if (null? todo)
		    done
		    (let* ((assign (car todo))
			   (vars (car assign))
			   (vals (cadr assign)))
		      (if (identifier? vars)
			  (assign-iter 
			   (cdr todo) 
			   (append done (list assign)))
			  (assign-iter
			   (cdr todo)
			   (append done (or (*run-match* vars vals)
					    (begin 
					      (warn 'match-failed-in-let)
					      '())))))))))))
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
     (let* ((assigns (cadr exp))
	    (body (cddr exp))
	    (dict 
	     (filter
	      pair?
	      (let assign-iter ((todo assigns) (done '()))
		(if (null? todo)
		    done
		    (let* ((assign (car todo))
			   (vars (car assign))
			   (vals (cadr assign)))
		      (if (identifier? vars)
			  (assign-iter 
			   (cdr todo) 
			   (append done (list assign)))
			  (assign-iter
			   (cdr todo)
			   (append done (or (*run-match* vars vals)
					    (begin 
					      (warn 'match-failed-in-let)
					      '())))))))))))
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

(match-let ((? y) (? x)) ((1 2)) (+ x y))
;3 
;Success!
(match-let ((? y) (? x)) ((1)) (+ x y))
;failed-match 
;Success!
