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
;a -> 1
;b -> 1
;c -> 1

(define-syntax dict-define
  (sc-macro-transformer
   (lambda (exp env)
     (let* ((dict (cdr exp)))
       ;(pp
	`(begin
	   ,@(map (lambda(entry)
		(let ((val 
		       (make-syntactic-closure env '() (cadr entry))))
		  `(define ,(car entry) ,val)))
		  dict))))));)
(define d 4)
(dict-define (a d) (b cos) (c 3))
;a -> 4
;b -> #[compiled-procedure 83 ("arith" #xd7) #x1a #xa5962a]
;c -> 3

(define-syntax match-define
  (sc-macro-transformer
   (lambda (exp env)
     (let* ((dict (cdr exp)))
       (pp
	`(begin
	   ,@(map (lambda(entry)
		(let ((val 
		       (make-syntactic-closure env '() (cadr entry))))
		  `(define ,(car entry) ,val)))
		  dict)))))))

(dict-define (a 1) (b 2) (c 3))

(define (succeed-fn d n) `(succeed ,d)))

(define dict ((match:->combinators '((? y) (? x))) '((1 2)) '()
	     (lambda (d n) d)))


(test-define bar 1)


(let ((t (caar dict)))
  (define t 1)
  x)
