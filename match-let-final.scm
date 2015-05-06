(define *d* '((*empty* 0)))

(define (remove-duplicates l)
  (cond ((null? l)
         '())
        ((member (car l) (cdr l))
         (remove-duplicates (cdr l)))
        (else
         (cons (car l) (remove-duplicates (cdr l))))))

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
; (d c b a)

(define (match:lookup dict symbol)
  (cadr (assq symbol dict)))

(define (match:lookup? exp)
  (tagged-list? exp 'match:lookup))

(define-syntax match-let
  (sc-macro-transformer
   (lambda (exp env)
     (let* ((key (close-syntax (cadr exp) env))
	    (pattern (caddr exp))
	    (vars (find-variables pattern '()))
	    (body (cdddr exp)))
       `(fluid-let ((*d* (append ((matcher ,pattern) ,key) *d*)))
	  ((lambda ,vars
	     ,@(map (lambda (statement) 
		      (make-syntactic-closure env vars statement))
		    body)) 
	   ,@(map (lambda (var) 
		    `(match:lookup *d* ',var)) vars))))))))
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

((matcher `((? x ,number?) (?? xs))) '(1 2 3 4 5))
;((xs (2 3 4 5)) (x 1))

(let ((vals '(1 2 3 4 5)))
 (match-let vals `((? x ,number?) (?? xs)) (cons x xs)))
;(1 2 3 4 5)

((matcher `((? x ,number?) (?? xs ,number?))) '(1 2 3 4 5))
;((xs (2 3 4 5)) (x 1))

(let ((vals '(1 2 3 4 5)))
 (match-let vals `((? x ,number?) (?? xs ,number?)) (cons x xs)))
;(1 2 3 4 5)

;Segment variables don't work with predicates. This is undefined
;behavior and is weird but okay
((matcher `((? x ,number?) (?? xs ,number?))) '(1 #f #f #f #f))
;((xs (#f #f #f #f)) (x 1))

;same as above
(let ((vals '(1 2 3 4 #t)))
 (match-let vals `((? x ,number?) (?? xs ,number?)) (cons x xs)))
;(1 2 3 4 #t)


(let ((vals '(1 #t 3 4 5)))
 (match-let vals `((? x ,boolean?) (?? xs)) (cons x xs)))
;errors, because match failed.  

((matcher `((? x ,boolean?) (?? xs))) '(1 #t 3 4 5))
;#f
