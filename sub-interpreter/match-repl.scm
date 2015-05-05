;;; Dict evaluating repl test
;(load "load")

(define (let-dict? exp) (tagged-list? exp 'let-dict))
(define (define-dict? exp) (tagged-list? exp 'define-dict))
(define (match-let? exp) (tagged-list? exp 'match-let))
(define (match-define? exp) (tagged-list? exp 'match-define))
(define (match-case? exp) (tagged-list? exp 'match-case))

(define (let-dict dict body)
  (let ((names (map car dict))
	(values (map (lambda (elt)
		       (pp (cadr elt))
		       (if (list? (cadr elt))
			   (cons 'quote (list (cadr elt)))
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

(defhandler eval
  (lambda (exp env)
    (define token (eval (cadr exp) env))
    (let case-iter ((todo (cddr exp)))
      (let* ((clause (car todo))
	     (pred (car clause))
	     (body (cadr clause))
	     (match ((matcher pred) token)))
	(cond 
	 (match (eval (let-dict match body) env))
	 ((null? (cdr todo)) 'no-match-found)
	 (else (case-iter (cdr todo)))))))
  match-case?)

;;;; Testing 
#|
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


(init)
(match-let '((? a) (? b)) '(1 2) (+ a b))
(let ((vars '(seq (? x) (?? xs)))
      (vals '(seq 1 2 3 4 5)))
 (match-let vars vals (cons x xs)))
#|
#|

(init)
(let ((vars '(seq (? x) (?? xs)))
      (vals '(seq 1 2 3 4 5)))
 (match-let vars vals (cons x xs)))
(define (parse-token token)
  (match-case token
	      ((bin-op (? op) (? a1) (? a2)) (op a1 a2))
	      ((un-op (? op) (? a)) (op a))
	      ((?? a) (pp a))))

(parse-token '(bin-op + 1 3))
(parse-token '(un-op - 4))
(parse-token '(+ 1 2))
(parse-token '(goto 0x3453))
(parse-token '(2 3 4 5))


(init)
(match-define '(? x) 1)
(match-define '((? x) (?? xs)) '(1 2 3 4 5))
(define m (list cos sin))
(match-define '(?? xs) (list m '2 '3 '4))
(match-define '((? x) ((? y) (? z))) '(p (-3 4)))



;;;; Segment Variable Testing

((matcher '((? x) (?? a))) '(1 2 3 4 5))
((matcher '(?? a)) '(2 3 4 5))
(quote ((2 3 4 5)))
(quote 1)
(quote a)
|#
