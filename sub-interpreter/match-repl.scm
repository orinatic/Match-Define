;;; Dict evaluating repl test

(load "~/Documents/6.945/Match-Define/eli-load.scm")
(load "load")

(define (let-dict? exp) (tagged-list? exp 'let-dict))
(define (define-dict? exp) (tagged-list? exp 'define-dict))
;;(define (let-dict*? exp) (tagged-list exp 'let-dict*))
;;(define (letrec-dict? exp) (tagged-list exp 'letrec-dict))



(define (let-dict dict body)
  (let ((names (map car dict))
	(values (map (lambda (elt)
		       (if (list? (cadr elt))
			   (cons 'list (cadr elt))
			   (cadr elt)))
		     dict)))
    (cons (list 'LAMBDA names body) values)))


(defhandler eval
  (lambda (exp env)
    (let ((dict (eval (cadr exp) env)))
      (eval (let-dict dict
		      (sequence->begin (cddr exp)))
	    env)))
  let-dict?)

(defhandler eval
  (lambda (exp env)
    (let  ((dict (eval (cadr exp) env)))
      (for-each 
       (lambda (elt)
	 (define-variable! (car elt) (cadr elt) env))
       dict)))
  define-dict?)

(init)
(define d '((a 2) (b 3)))
(define l '(q w e r t y))

(let-dict d (+ a b))
(let-dict '((a 2) (b 3)) (+ a b))
(let-dict ((lambda () '((a 2) (b 3)))) (+ a b))
(let-dict '((x l) (xs (2 3 4 5))) (begin (pp x) (pp xs)))


(define-dict ((lambda () '((a 3) (b 7)))))
(define-dict '((c 4) (g (1 2))))

(let-dict ((matcher '((? a) (? b))) '(1 2)) (+ a b))

(define (match-let? exp) (tagged-list? exp 'match-let))
(define (match-define? exp) (tagged-list? exp 'match-define))

(defhandler eval
  (lambda (exp env)
    (let* ((pattern (eval (cadr exp) env))
	   (input (eval (caddr exp) env))
	   (body (sequence->begin (cdddr exp)))
	   (dict ((matcher pattern) input)))
      (if dict 
	  (eval (let-dict dict body) env)
	  'match-failed)))
  match-let?)

(init)
(match-let '((? a) (? b)) '(1 2) (+ a b))
(let ((vars '(seq (? x) (?? xs)))
      (vals '(seq 1 2 3 4 5)))
 (match-let vars vals (cons x xs)))

(defhandler eval
  (lambda (exp env)
    (pp (cadr exp))
    (pp (caddr exp))
    (let* ((pattern (eval (cadr exp) env))
	   (input (eval (caddr exp) env))
	   (dict ((matcher pattern) input)))
      (pp dict)
      (if dict 
	  (for-each 
	   (lambda (elt)
	     (define-variable! (car elt) (cadr elt) env))
	   dict)
    'match-failed)))
match-define?)

(init)
(match-define '(? x) 1)
(match-define '((? x) ((? y) (? z))) '(p (-3 4)))
