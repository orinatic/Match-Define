;;the code that actually runs the matcher
(define (*run-match* vars pattern)
  ((match:->combinators vars) pattern '() (lambda (d n) d)))

;;If we decide to go with the macro system
;;system, we're contemplating overwriting the actual definitions of
;;let, let*, define, etc with these match versions (as the match
;;versions are backwards compatible).  
;; Our eval-environment system isn't fully backwards-compatible yet
;; (the expressions in the let statement need to be made into an
;; alist.  If they're used with a match statement as well, then the
;; caller needs to append the expressions with the output of the call
;; to *run-match*, as in 
;; 
;;((dict-let (append (*run-match* '((? y) (? x)) '((1 2))) '((d 5)))
;;	   (+ d x y 400)))
;;

;;Please see the final report for a full API for the macros and
;;eval-environment system, along with the API for our repl version.  

;Our define macro
(define-syntax match-define
  (sc-macro-transformer
   (lambda (exp env)
     (let* ((vars (cadr exp))
	    (vals (caddr exp))
	    (dict (*run-match* vars vals)))
       (if dict
	   ;(pp
	    `(begin
	       ,@(map (lambda(entry)
			(let ((val 
			       (close-syntax (cadr entry) env)))
			  `(define ,(car entry) ,val)))
		      dict))
	    `(pp 'failed-match))))));)

;A helper assign function
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
;Our let macro
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

;Our let* macro
(define-syntax match-let*
  (sc-macro-transformer
   (lambda (exp env)
     (let* ((body (cddr exp))
	    (dict (assign-iter (cadr exp) '())))
       `(let* ( ,@(map (lambda(entry)
			`(,(car entry) ,(cadr entry)))
		      dict))
	   (begin ,@(map (lambda (statement)
		    statement)
		  body)))))))

;Our letrec macro
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

;The macro for named-let.  
(define-syntax match-named-let
  (sc-macro-transformer
   (lambda (exp env)
     (let* ((name (cadr exp))
	    (body (cdddr exp))
	    (dict (assign-iter (caddr exp) '())))
       `(let ,name ( ,@(map (lambda(entry)
			`(,(car entry) ,(cadr entry)))
		      dict))
	   (begin ,@(map (lambda (statement)
		    statement)
		  body)))))))

;;;; Our dict-let macro, which attempts to use the environment-eval
;;;; trick.  
;;;; Does not allow the body to use variables that are defined
;;;; externally. We're working on a fix for this.  
(define-syntax dict-let
  (sc-macro-transformer
   (lambda (exp env)
     (let ((dict (cadr exp))
	   (body (caddr exp)))
       `(lambda ()
	  (define (empty a) 'nothing)
	  (let ((our-env (procedure-environment empty)))
	    (let dict-define-loop ((todo ,dict))
	      (if (null? todo)
		  'done
		  (let ((var (caar todo))
			(val (cadar todo)))
		    (environment-define our-env var val)
		    (dict-define-loop (cdr todo)))))
	    (eval ,body our-env)))))))



;;;; Match-define tests
(match-define ((? y) (? x)) ((1 2)))
;x->2
;y->1
(match-define ((? y) (? x)) ((1)))
;failed-match

;;;; Match-let tests
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
(match-let ((((? y)) ((1 2))))
	   (+ x y))
;Warning: match-failed-in-let
3

(match-let ((((? y)) ((4 2)))
	     (d 5))
	   d)
;Warning:  match-failed-in-let
;5
;

;;;;Match-let* tests
(match-let* ((((? y) (? x)) ((1 2)))
	     (d (+ y x)))
	    (+ d x y)) ;-> 6

(match-let* ((((? y) (? x)) ((1 2)))
	    (((? c) (? d)) (((+ 3 x) y))))
	   (+ c d x y)) ;-> 9

;;;; Match-letrec tests

;;Letrec test cases!

;This test case taken from the MIT Scheme Documentation
(match-letrec ((even?
          (lambda (n)
            (if (zero? n)
                #t
                (odd? (- n 1)))))
         (odd?
          (lambda (n)
            (if (zero? n)
                #f
                (even? (- n 1))))))
  (even? 88))

;#t Success!

(match-letrec ((((? y) (? x)) ((1 2)))
	     (d 5))
	   (+ d x y)) ;-> 8

(match-letrec ((((? y) (? x)) ((1 2))))
	   (+ x y)) ;-> 3

(match-letrec ((((? r)) ((4 2)))
	     (d 5))
	   d)
;Warning: match-failed-in-let
;Value: 5

;;checking letrec
(match-letrec ((even?
          (lambda (n)
            (if (zero? n)
                (begin 
		  (pp y)
		  #t)
		(odd? (- n 1)))))
	       (((? y) (? x)) ((1 2)))
	       (odd?
		(lambda (n)
		  (if (zero? n)
		      (begin
			(pp x)
			#f)
		      (even? (- n 1))))))
  (even? 88))
;1
;Value: #t
;Success!

;checking if letrec works.  We letrec even? and odd? (which call each
;other) and then letrec four? and five? (which call each other) and
;then call four? 
(match-letrec ((even?
          (lambda (n)
            (if (zero? n)
                (begin 
		  (pp y)
		  #t)
		(odd? (- n 1)))))
	       (((? y) (? x)) ((1 2)))
	       (odd?
		(lambda (n)
		  (if (zero? n)
		      (begin
			(pp x)
			#f)
		      (even? (- n 1))))))
	      (if (odd? x) ;this should be false, as x should be 2
		  (even? y)
		  (match-letrec ((four?
				  (lambda (n)
				    (if (zero? (- n 4))
					  n
					  (five? (- n 1)))))
				 (five?
				  (lambda (n)
				    (if (zero? (- n 5))
					n
					(even? (- n 1))))))
				(four? 500)))) 

;2
;Value: #t
;Success!
;yes, four? and five? are dumb functions, but we can do nested
;match-letrecs and it works, which is what we were trying to test!  

;;;;; Match-named-let testing

;Borrowed from the MIT Scheme documentation
;This aims to test if named let actually works
(match-named-let loop
     ((numbers '(3 -2 1 6 -5))
      (nonneg '())
      (neg '()))
  (cond ((null? numbers)
         (list nonneg neg))
        ((>= (car numbers) 0)
         (loop (cdr numbers)
               (cons (car numbers) nonneg)
               neg))
        (else
         (loop (cdr numbers)
               nonneg
               (cons (car numbers) neg)))))
; ((6 1 3) (-5 -2))
;Success!

;Another test for named-let
(match-named-let godeeper
      ((cat 'yayyoufinished)
      (noodles '(1 2 3 4 5 6)))
     (if (null? noodles)
	 cat
	 (godeeper cat (cdr noodles))))
;yayyoufinished
;Success!
