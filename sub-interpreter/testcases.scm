;;;; Test Cases for the new non-REPL version

;Match-let testing

(match-let '((? a) (? b)) '(1 2) (+ a b)) 

(let ((vars '(seq (? x) (?? xs)))
      (vals '(seq 1 2 3 4 5)))
 (match-let vars vals (cons x xs)))


(let ((vars '(seq (? x) (?? xs)))
      (vals '(seq 1 2 3 4 5)))
 (match-let vars vals (cons x xs) (pp `(,x and ,xs have been set))))


;;;Match-case testing

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

;;;Match-define testing

(match-define '(? x) 1)
(match-define '((? x) (?? xs)) '(1 2 3 4 5))
(match-define '(?? xs) (list (list cos sin) '2 '3 '4))
(match-define '((? x) ((? y) (? z))) '(p (-3 4)))

;;;; Segment Variable Testing

((matcher '((? x) (?? a))) '(1 2 3 4 5))
((matcher '(?? a)) '(2 3 4 5))
(quote ((2 3 4 5)))
(quote 1)
(quote a)

;;;Testing multi-line body 

(let ((vars '(seq (? x) (?? xs)))
      (vals '(seq 1 2 3 4 5)))
 (match-let vars vals (begin (cons x xs) (pp 'herewego) 
			     (pp 'lotsoflines)
			     (pp x) (pp xs))))

(let ((vars '(seq (? x) (?? xs)))
      (vals '(seq 1 2 3 4 5)))
 (match-let vars vals (pp (cons x xs)) (pp 'herewego) (pp x) (pp xs)))

;;;Testing predicates

((matcher '(a ((? b) 2 3) (? b) c))
 '(a (1 2 3) 1 c))

((matcher '((? b)))
 '(4))

((matcher `((? b ,number?)))
 '(4))

((matcher `((? b ,number?)))
 '(4))

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

