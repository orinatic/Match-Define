;;The code for let.  

#|
(define (dict-let dict body)
  (let ((vars (map car dict))
	(vals (map cadr dict)))
    (pp vars)
    (pp vals)))
(define-syntax dict-let
  (sc-macro-transformer
   (lambda (exp env)
     (pp exp)
     (let ((dict (cadr exp))
	    (body (caddr exp)))
      ;(pp dict)
      (pp
	`(let (
	       ,@(map (lambda(entry)
;			     (let ((val 
;			      (make-syntactic-closure env '() (cadr entry))))
			       `(,(car entry) ,(cadr entry)))
		  dict))
	   ,body))))))

(define d 4)
(define dictachu '((a d) (b cos) (c 3)))
(dict-let dictachu (pp `c))
(dict-let ((a d) (b cos) (c 3)) (+ c a))
;7 
;Success!!

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
;;*run-match* can be defined to be anything, with the following
;;restrictoins:
;; Must take a list of vars a pattern to match against
;; Must return an alist of (var val) if the match succeeds
;; Must return false if the match fails

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

(match-let ((((? y)) ((1 2)))
	     );(d 5))
	   (+ x y))
;Warning: match-failed-in-let
;Unbound variable: y
;and then an error

(match-let ((((? y)) ((4 2)))
	     (d 5))
	   (pp d)
	   (pp x)
	   (pp y)
	   d)
;Warning:  match-failed-in-let
;5
;Unbound variable: x
;and then an error

#|
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

|#

(define-syntax match-let*
  (sc-macro-transformer
   (lambda (exp env)
     (let* ((body (cddr exp))
	    (dict (assign-iter (cadr exp) '())))
;       (pp dict)
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

;;Test cases

;Borrowed from the MIT Scheme documentation
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

(match-named-let godeeper
      ((cat 'yayyoufinished)
      (noodles '(1 2 3 4 5 6)))
     (if (null? noodles)
	 cat
	 (godeeper cat (cdr noodles))))
;yayyoufinished
;Success!

(define (dict-let*new dict body)
  (let ((vars (map car dict))
	(vals (map cadr dict)))
    (pp vars)
    (pp vals)))
(define halloDict '((a 1) (b 2)))
(dict-let*new halloDict (pp a))

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
		  body)))))))

