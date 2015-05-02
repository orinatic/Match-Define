(load "~/Documents/6.945/Match-Define/load.scm")

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
     (let* ((dict (cadr exp)))
       (pp env)
       (pp dict)
       ;(pp (eval dict (the-environment)))
       (pp
	`(begin
	   ,@(map (lambda(entry)
		(let ((val 
		       (close-syntax (cadr entry) env)))
		  `(define ,(car entry) ,(cadr entry))))
		   dict)))))))
(define d 4)
(define t_dict '((a d) (b cos) (c 3)))
(dict-define ((a d) (b cos) (c 3)))
(dict-define t_dict) ;-> breaks :(
;a -> 4
;b -> #[compiled-procedure 83 ("arith" #xd7) #x1a #xa5962a]
;c -> 3
(eval 'd (the-environment))

(define-syntax match-define
  (sc-macro-transformer
   (lambda (exp env)
     (let* ((match (cadr exp))
	    (vals (caddr exp))
	    (dict ((match:->combinators match)
				       vals 
				       '() 
				       (lambda (d n) d))))
       (if dict
	   ;(pp
	    `(begin
	       ,@(map (lambda(entry)
			(let ((val 
			       (close-syntax (cadr entry) env)))
			  `(define ,(car entry) ,val)))
		      dict))
	    `(pp 'failed-match)
	    )))));)

(match-define ((? y) (? x)) ((1 2)))
;x->2
;y->1
(match-define ((? y) (? x)) ((1)))
;failed-match

