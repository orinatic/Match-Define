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
    (let* ((pattern (eval (cadr exp) env))
	   (input (eval (caddr exp) env))
	   (dict ((matcher pattern) input)))
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

;;;Let-dict and define-dict testing

(init)
(define d '((a 2) (b 3)))
(define l '(q w e r t y))

(let-dict d (+ a b)) ;5
(let-dict '((a 2) (b 3)) (+ a b)) ;5
(let-dict ((lambda () '((a 2) (b 3)))) (+ a b)) ;5
(let-dict '((x l) (xs (2 3 4 5))) (begin (pp x) (pp xs)))
;(q w e r t y)
;(2 3 4 5)
;#!unspecific


(define-dict ((lambda () '((a 3) (b 7)))))
(define-dict '((c 4) (g (1 2))))

(let-dict ((matcher '((? a) (? b))) '(1 2)) (+ a b)) ;3

;Match-let testing

(init)
(match-let '((? a) (? b)) '(1 2) (+ a b)) ;3
(let ((vars '(seq (? x) (?? xs)))
      (vals '(seq 1 2 3 4 5)))
 (match-let vars vals (cons x xs)))
;(1 2 3 4 5)


(init)
(let ((vars '(seq (? x) (?? xs)))
      (vals '(seq 1 2 3 4 5)))
 (match-let vars vals (cons x xs)))
;(1 2 3 4 5)

;;;Match-case testing

(define (parse-token token)
  (match-case token
	      ((bin-op (? op) (? a1) (? a2)) (op a1 a2))
	      ((un-op (? op) (? a)) (op a))
	      ((?? a) (pp a))))

(parse-token '(bin-op + 1 3)) ;4
(parse-token '(un-op - 4)) ; -4
(parse-token '(+ 1 2)) ;((+ 1 2)) #!unspecific
(parse-token '(goto 0x3453))
;((goto 0x3453))
;#!unspecific
(parse-token '(2 3 4 5))
;((2 3 4 5))
;#!unspecific

;;;Match-define testing

(init)
(match-define '(? x) 1)
(match-define '((? x) (?? xs)) '(1 2 3 4 5))
(define m (list cos sin))
(match-define '(?? xs) (list m '2 '3 '4))
(match-define '((? x) ((? y) (? z))) '(p (-3 4)))

;;;; Segment Variable Testing

((matcher '((? x) (?? a))) '(1 2 3 4 5))
;((a (2 3 4 5)) (x 1))
((matcher '(?? a)) '(2 3 4 5))
;((a ((2 3 4 5))))
(quote ((2 3 4 5)))
(quote 1)
(quote a)

;;;Testing multi-line body 

(init)
(let ((vars '(seq (? x) (?? xs)))
      (vals '(seq 1 2 3 4 5)))
 (match-let vars vals (begin (cons x xs) (pp 'herewego) 
			     (pp 'lotsoflines)
			     (pp x) (pp xs))))
;herewego
;lotsoflines
;1
;(2 3 4 5)
;#!unspecific
;works fine with a begin statement.  But can it work without the
;begin?!?!

(init)
(let ((vars '(seq (? x) (?? xs)))
      (vals '(seq 1 2 3 4 5)))
 (match-let vars vals (pp (cons x xs)) (pp 'herewego) (pp x) (pp xs)))

;(1 2 3 4 5)
;herewego
;1
;(2 3 4 5)
;#!unspecific
;Yes we can!  Awesome.

;;;Testing predicates


((matcher '(a ((? b) 2 3) (? b) c))
 '(a (1 2 3) 1 c))
;Value: ((b 1))

((matcher '((? b)))
 '(4))
;Value: ((b 4))

((matcher '((? b ,number?)))
 '(4))
;Value: ((b 4))



(define (parse-token token)
  (match-case token
	      ((bool-lit (? a boolean?)) (pp a))
	      ((stringy (? a string?)) (pp a))
	      ((?? a) (pp a) (pp 'extra))))

(parse-token '(goto 0x3453))
(parse-token '(bool-lit #t))
(parse-token '(stringy "Macros are hard"))

;;Creating an interesting demo (maybe?)

(define (parse-token token)
  (match-case token
	      ((bin-op (? op) (? a1) (? a2)) (op a1 a2))
	      ((un-op (? op) (? a)) (op a))
	      
	      ((?? a) (pp a))))

|#
