(load "~/Documents/6.945/Match-Define/load.scm")
(define-syntax test-define
  (syntax-rules ()
    ((_ name val)
     ;(let ((dict ((match->combinators names) (list vals) '() 
     ;(lambda (d n) d))))
       (define name val))))

(define-syntax show-vars
  (sc-macro-transformer
   (lambda (exp env)
     (let ((vars (cdr exp)))
       `(begin
	  (display
	   (list
	    ,@(map (lambda(v)
		     (let ((w (make-syntactic-closure env '() v)))
		       `(list ',w ,w)))
		   vars)))
	  (newline))))))
(let ((i 1) (j 3) (k 7))
  (show-vars i j k))


(define-syntax list-define
  (sc-macro-transformer
   (lambda (exp env)
     (let ((vars (cdr exp)))
	`(begin
	   ,@(map (lambda(v)
		    (let ((w (make-syntactic-closure env '() v)))
		      `(define ,v 1)))
		 vars))))))

(list-define a b c)

(define (succeed-fn d n) `(succeed ,d)))

(define dict ((match:->combinators '((? y) (? x))) '((1 2)) '()
	     (lambda (d n) d)))


(test-define bar 1)


(let ((t (caar dict)))
  (define t 1)
  x)
