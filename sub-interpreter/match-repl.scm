;;; Dict evaluating repl test

(load "load")

(define (let-dict? exp) (tagged-list? exp 'let-dict))
(define (define-dict? exp) (tagged-list? exp 'define-dict))
;;(define (let-dict*? exp) (tagged-list exp 'let-dict*))
;;(define (letrec-dict? exp) (tagged-list exp 'letrec-dict))

(define (let-dict dict body)
  (let ((names (map car dict))
	(values (map cadr dict)))
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
    (let ((dict (eval (cadr exp) env)))
      (for-each 
       (lambda (elt)
	 (define-variable! (car elt)
	   (cadr elt)
	   env))
       dict)))
  define-dict?)

(let-dict ((lambda () (list (list 'a 2) (list 'b 3)))) (+ a b))

(define-dict ((lambda () (list (list 'a 3) (list 'b 7)))))
