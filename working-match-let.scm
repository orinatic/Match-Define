;;Our final nice version

;The dictionary of bindings
(define *d* '())

;;removes duplicates out of a list
(define (remove-duplicates l)
  (cond ((null? l)
         '())
        ((member (car l) (cdr l))
         (remove-duplicates (cdr l)))
        (else
         (cons (car l) (remove-duplicates (cdr l))))))

;;given a match statement, it pulls variables out of the match statement
(define (find-variables todo vars)
  (if (null? todo)
      (remove-duplicates vars)
      (let ((token (car todo)))
	(cond 
	 ((match:element? token) 
	  (find-variables (cdr todo) (cons (cadr token) vars)))
	 ((match:segment? token) 
	  (find-variables (cdr todo) (cons (cadr token) vars)))
	 ((pair? token) 
	  (find-variables (cdr todo) 
			  (find-variables token vars)))
	  (else (find-variables (cdr todo) vars))))))

(find-variables `((? a) ((?? b) (? a ,string?) ((? c) (? d)))) '())
;(d c b a)

;;This puts in the calls to ref as required.  
;;Replaces the variables in vars in the code with the calls to ref
;;that look up their values in the dictionary.  
(define (process-body todo done vars)
  (if (null? todo)
      (reverse done)
      (let ((token (car todo)))
	(cond 
	 ((memq token vars) 
	  (process-body (cdr todo) 
			(cons `(match:lookup *d* ',token) done) 
			vars))
	 ((boolean/and (pair? token) 
		       (not (match:element? token))
		       (not (match:segment? token))
		       (not (match:lookup? token)))
	  (process-body (cdr todo) 
			(cons (process-body token '() vars) done) 
			vars))
	 (else 
	  (process-body (cdr todo) (cons token done) vars))))))

(process-body '((- (+ c d)  
		   2 
		   (* a c) 
		   (sqrt (* f b))) 
		(pp a))
	      '()
	      '(a b c d))
; ((- (+ (ref *d* (quote c)) (ref *d* (quote d))) 2 (* (ref *d* (quote a)) (ref *d* (quote c))) (sqrt (* f (ref *d* (quote b))))) (pp (ref *d* (quote a))))


;;gets the value of symbol from the dictionary
(define (match:lookup dict symbol)
  (cadr (assq symbol dict)))

;predicate for match_lookup
(define (match:lookup? exp)
  (tagged-list? exp 'match:lookup))

;;match-let is a macro which "lets" the variables matched in the match
;;statement be the values that they match for the duration of the let
;;statement.  
(define-syntax match-let
  (sc-macro-transformer
   (lambda (exp env)
     (let* ((key (close-syntax (cadr exp) env))
	   (pattern (caddr exp))
	   (vars (find-variables pattern '()))
	   (body (process-body (cdddr exp) '() vars)))
       `(begin
	  (fluid-let ((*d* (append ((matcher ,pattern) ,key) *d*)))
	    (begin ,@(map (lambda (statement) statement)
			  body))))))))

;Match-let testing

(match-let '(5 6) '((? a) (? d))
	      (+ a d))
;11

(match-let '(1 2 3 4) '((? a) (? b) (? c) 4)
	      (match-let '(5 6) '((? a) (? d))
			       (+ a b c d)))
;16

(let ((token '(bin-arith + 2 4)))
  (match-let token `(bin-arith (? op) (? a1 ,number?) (? a2 ,number?))
	          (pp (list op a1 a2))))

;(+ 2 4) ;Unspecified return value

(match-let '(1 2) '((? a) (? b)) (+ a b)) 
;3

(let ((vals '(seq 1 2 3 4 5)))
 (match-let vals '(seq (? x) (?? xs)) (cons x xs)))
;(1 2 3 4 5)

(let ((vals '(seq 1 2 3 4 5)))
 (match-let vals '(seq (? x) (?? xs)) (pp `(,x and ,xs have been set))
	    (cons x xs)))
; (1 and (2 3 4 5) have been set)
;Value 20: (1 2 3 4 5)


;;;Match-case testing

(define (parse-token token)
  (match-case token
	      ((bin-op (? op) (? a1) (? a2)) (op a1 a2))
	      ((un-op (? op) (? a)) (op a))
	      ((?? a) (pp a) (pp "no match"))))

(parse-token '(bin-op + 1 3))
(parse-token '(un-op - 4))
(parse-token '(+ 1 2))
(parse-token '(goto 0x3453))
(parse-token '(2 3 4 5))

;;;; Segment Variable Testing

((matcher '((? x) (?? a))) '(1 2 3 4 5))
;((a (2 3 4 5)) (x 1))
((matcher '(?? a)) '(2 3 4 5))
;((a ((2 3 4 5))))
(quote ((2 3 4 5)))
;((2 3 4 5))
(quote 1)
;1
(quote a)
;a

;;;Testing multi-line body 

(let ((vals '(seq 1 2 3 4 5)))
 (match-let vals '(seq (? x) (?? xs)) (begin (cons x xs) (pp 'herewego) 
			     (pp 'lotsoflines)
			     (pp x) (pp xs))))
;herewego
;lotsoflines
;1
;(2 3 4 5)
;;Unspecified return value


(let ((vals '(seq 1 2 3 4 5)))
 (match-let vals '(seq (? x) (?? xs)) (pp (cons x xs)) (pp 'herewego) (pp x) (pp xs)))
;(1 2 3 4 5)
;herewego
;1
;(2 3 4 5)
;;Unspecified return value
;;Yay!   Works without the begin statement.  

;;;Testing predicates

((matcher '(a ((? b) 2 3) (? b) c))
 '(a (1 2 3) 1 c))
;((b 1))

((matcher '((? b)))
 '(4))
;((b 4))

((matcher `((? b ,number?)))
 '(4))
;((b 4))


(define (parse-token token)
  (match-case token
	      ((bool-lit `(? a ,boolean?)) (pp a))
	      ((stringy `(? a ,string?)) (pp a))
	      ((?? a) (pp a) (pp 'extra))))

(parse-token '(goto 0x3453))
(parse-token '(bool-lit #t))
(parse-token '(stringy "Macros are hard"))

;;Very simple demo

(define (parse-token token)
  (match-case token
	      ((bin-op (? op) (? a1) (? a2)) (op a1 a2))
	      ((un-op (? op) (? a)) (op a))
	      ((bool (? val ,boolean?)) val)
	      ((stringy (? a ,string?)) (pp a))
	      ((num (? a ,number?)) (pp `(number is ,a)))
	      ((?? a) (pp a))))


(parse-token '(goto 0x3453))
(parse-token '(bool #t))
(parse-token '(stringy "Macros are hard"))
(parse-token '(bin-op (+ 4 5)))
(parse-token '(un-op (- 4)))
(parse-token '(num 4))

