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

(define-syntax match-let
  (sc-macro-transformer
   (lambda (exp env)
     (let* ((match (cadr exp))
	    (vals (caddr exp))
	    (body (cadddr exp))
	    (dict ((match:->combinators match)
				       vals 
				       '() 
				       (lambda (d n) d))))
       (if dict
	   `(let (
		  ,@(map (lambda(entry)
			;   (let ((val 
			;	  (make-syntactic-closure env '() (cadr entry))))
			     `(,(car entry) ,(cadr entry)))
			 dict))
	   ,body)
	   (pp 'failed-match))))))

(match-let ((? y) (? x)) ((1 2)) (+ x y))
;3 
;Success!
(match-let ((? y) (? x)) ((1)) (+ x y))
;failed-match 
;Success!

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
