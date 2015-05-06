(define-syntax match-case
  (sc-macro-transformer
   (lambda (exp env)
     (let* ((key (close-syntax (cadr exp) env))
	    (clauses (cddr exp)))
       `(cond 
	 ,@(map (lambda (clause)
		  (let ((pattern (car clause))
			(body (cadr clause)))
		    `(((matcher ,pattern) ,key) 
		      (match-let ,key ,pattern ,body))))
		clauses)
	 (else 'no-match))))))


(define (parse-token token)
  (match-case token
	      (`(bool-lit (? a ,boolean?)) (pp `(bool is ,a)))
	      (`(stringy (? a ,string?)) (pp `(string is ,a)))
	      ('(?? a) (pp `(extra is ,a)))))

(parse-token '(goto 0x3453)) ;(extra is ((goto 0x3453)))
(parse-token '(bool-lit #t)) ;(bool is #t)
(parse-token '(stringy "Macros are hard")) ;(string is "Macros are hard")

;;Very simple demo

(define (parse-token token)
  (match-case token
	      ('(bin-op (? op) (? a1) (? a2)) (op a1 a2))
	      ('(un-op (? op) (? a)) (op a))
	      (`(bool (? val ,boolean?)) val)
	      (`(stringy (? a ,string?)) (pp `(string is ,a)))
	      (`(num (? a ,number?)) (pp `(number is ,a)))
	      ('(?? a) (pp `(extra ,a)))))


(parse-token '(goto 0x3453)) ;(extra ((goto 0x3453)))
(parse-token '(bool #t)) ;#t
(parse-token '(stringy "Macros are hard")) ;(string is "Macros are hard")
(parse-token `(bin-op ,+ 4 5)) ;9
(parse-token `(un-op ,- 4)) ;-4
(parse-token '(num 4)) ;(number is 4)


;;;Match-case testing
(match-case '(harold 4 5 3333 33 3333)
	    (`(harold (? a) (? b ,number?) (?? c)) (pp `(harold is ,a ,b ,c)))
	    (`(cora (? a ,number?) (? b) (?? c)) (pp "cora"))
	    (((?? c) chesterworth (? d)) (pp "chesterworth")))
;(harold is 4 5 (3333 33 3333))

(match-case '(3 100 buffalos beefalo)
	    (`(harold (? a) (? b ,number?) (?? c)) (pp `(harold is ,a
								,b
								,c)))
	    ('((?:choice a (? c)) 100 (?:choice buffalos buffaloes) (??
								    b))
	     (pp b)))
;(beefalo)

(match-case '(1 2 3)
           ('(2 3 (? a)) (+ 2 3 a))
           ('(1 2 3) (+ 1 2 3)))
;6

(match-case '(2 3 557)
           ('(2 3 (? a)) (+ 2 3 a))
           ('(1 2 3) (+ 1 2 3)))
;562

(match-case '(1 #t 3)
           ('(2 3 (? a)) (+ 2 3 a))
           ('(1 2 3) (+ 1 2 3))
	   (`(1 (? a ,boolean?) 3) a))
;#t

(define (parse-token token)
  (match-case token
	      ('(bin-op (? op) (? a1) (? a2)) (op a1 a2))
	      ('(un-op (? op) (? a)) (op a))
	      ('(?? a) (pp "no match"))))

(parse-token `(bin-op ,+ 1 3)) ;4
(parse-token `(un-op ,- 4)) ;-4
(parse-token `(,+ 1 2)) ;"no match"
(parse-token '(goto 0x3453)) ;"no match"
(parse-token '(2 3 4 5)) ;"no match"

((matcher
  '(?:pletrec ((panda (?:choice pudding bear)))
	      (?:ref panda)))
'((bear) (bear)))

((matcher '(? a)) 444)
;((a 444))
