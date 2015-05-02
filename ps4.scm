;;;; Eli Davis
;;;; pset 4
;;;; Collaborated with Jenny Ramseyer and Tom Boning

(load "~/Documents/6.945/p4/code/load.scm")
;;;; Problem 4-1
;;; If we look at the bottom of match:segment, we see the following
;;; code:
;;(let lp ((i 0))
;;  (if (<= i n)
;;      (or (succeed (match:bind variable
;;			       (list-head data i)
;;			       dictionary)
;;		   i)
;;	  (lp (+ i 1)))
;;      #f))
;;; The succeed function we've passed in is:
;;(define (print-all-results dict n-eaten)
;;  (pp ‘(success ,dict ,n-eaten))
;;  #f)
;;; Thus, when we get to call lp, we will call print-all-results,
;;; which will print the dict we tried, but then return false. Thus,
;;; we will try again, and keep trying until i = n, at which point we
;;; have run out of things to try and will stop.


;;;; Problem 4-2

(define x-matcher (match:eqv 'x))
(define (result-receiver dict n-eaten) `(success ,dict ,n-eaten))

(define (match:choice . match-combinators)
  (define (choice-match data dictionary succeed)
    (and (pair? data)
	 (let lp ((matcher match-combinators))
	   (if (pair? matcher)
	       (let ((match ((car matcher) data dictionary succeed)))
		 (if match
		     match
		     (lp (cdr matcher))))
	       #f))))
  choice-match)

(define (match:choice? pattern)
 (and (pair? pattern)
      (eq? (car pattern) '?:choice)))

(defhandler match:->combinators
  (lambda (pattern) 
    (apply match:choice (map match:->combinators pattern)))
  match:choice?)

(define both-matcher (match:choice (match:eqv 'x) (match:eqv 'y)))
(both-matcher '(x) '() result-receiver) ;-> (success () 1)
(both-matcher '(y) '() result-receiver) ;-> (success () 1)
(both-matcher '(z) '() result-receiver) ;-> #f

(define (succeed-fn d n) `(succeed ,d)))

((match:->combinators '(?:choice a b (? x) c)) '(z) '()
 (lambda (d n) `(succeed ,d)))
;Value: (succeed ((x z)))

((match:->combinators
 `((? y) (?:choice a b (? x ,string?) (? y ,symbol?) c))) '((z z)) '()
 (lambda (d n) `(succeed ,d)))
;Value: (succeed ((y z)))

((match:->combinators `(?:choice b (? x ,symbol?))) '(b) '()
 (lambda (x n) (pp `(succeed ,x)) #f))
;Value: #f

((matcher '(?:choice (a a (?? x) b (?? x) c) (a a (?? x) (?? x) c)))
	  '(a a y y y y c)) ; -> ((x (y y)))


((matcher '(?:choice (a a (?? x) (? d) (?? x) c) (a a (?? x) (?? x) c)))
	  '(a a y y b y y c)) ; -> ((d b) (x (y y)))

((matcher '(?:choice (a a (?? x) (? d) (?? x) c) (a a (?? x) (?? x) c)))
	  '(a a y y b y y c)) ; -> ((d b) (x (y y)))

;;;; Problem 4-3
;;; I really don't like using this global table, but I'm not sure how
;;; to use a more local one without rewriting everything so it's under
;;; one definition
(define named-patterns (make-strong-eqv-hash-table))

(define (match:pletrec? pattern)
 (and (pair? pattern)
      (eq? (car pattern) '?:pletrec)))

(define (match:ref? pattern)
  (and (pair? pattern)
       (eq? (car pattern) '?:ref)))

(defhandler match:->combinators
  (lambda (pattern)
    (for-each (lambda (def)
		(hash-table/put! 
		 named-patterns 
		 (car def) 
		 (match:->combinators (cadr def))))
	      (cadr pattern))
    (match:->combinators (caddr pattern)))
  match:pletrec?)

(defhandler match:->combinators
  (lambda (pattern)
    (match:ref 
     (match:variable-name pattern) 
     named-patterns))
  match:ref?)

(define (match:ref reference env-dict)
  (define (ref-match data dictionary succeed)
    (and (pair? data)
	 ((hash-table/get env-dict reference -1)
	  data dictionary succeed)))
  ref-match)

((match:->combinators '(?:pletrec ((foo baz))
				  (?:ref foo)))
 '(baz)
 '()
 succeed-fn) ;-> (succeed ())


(define one-star 
  (match:->combinators 
   '(?:pletrec ((one-star (?:choice * (1 (?:ref one-star)))))
	       (?:ref one-star))))
(one-star '((1  *)) '() succeed-fn) ;-> (succeed ())
(one-star '((1 (1  *))) '() succeed-fn) ;-> (succeed ())

((match:->combinators
 '(?:pletrec ((odd-even-etc (?:choice () (1 (?:ref even-odd-etc))))
	      (even-odd-etc (?:choice () (2 (?:ref odd-even-etc)))))
	     (?:ref odd-even-etc)))
 '((1 (2 (1 ()))))
 '()
 succeed-fn) ;-> (succeed ())

(define a-b-swap
  (match:->combinators
   '(?:pletrec ((a-b-etc (?:choice () ((?? a) (?:ref even-odd-etc))))
		(b-a-etc (?:choice () ((?? b) (?:ref odd-even-etc)))))
	       (?:ref a-b-etc))))
(a-b-swap '((1 (2 (1 ())))) '() succeed-fn) 
					;-> (succeed ((b (2)) (a (1))))
(a-b-swap '((%% ($$ (%% ())))) '() succeed-fn) 
					;-> (succeed ((b ($$)) (a (%%))))
(a-b-swap '((1 (2 (3 ())))) '() succeed-fn) 
					;-> #f

;;;; Problem 4-4
;;; If it wasn't there, we would keep applying the commutitive law
;;; back and forth, in an infinite loop

;;;; Problem 4-5
;;; The numerical simplifications do not need to deal with as many
;;; segment variables, because all the 0s and 1s will have been pushed
;;; to the front. Thus, we can have the rule (rule `(* 0 (?? x)) 0)
;;; instead of (rule `(* (?? y) 0 (?? x)) 0)

;;;; Problem 4-6
;;; Yes, although it needs some amount of code modification. You need
;;; a sorted? predicate, and need to be able to apply predicates to
;;; segment variables. Then you just have a rule which matches an
;;; unsorted segment variable, and runs merge sort or quick sort or
;;; something else faster than bubble sort on it.
;;; The sorted? predicate should be set to true when a segment is
;;; sorted, and written in a table somewhere for easy access. All
;;; algibraic transformations should preserve sort order (I believe
;;; they already do this). This, each segment will only need to be
;;; sorted once, and the whole sort time will be O(n*log(n))


;;;; Problem 4-7
(define algebra-3
  (rule-simplifier
   (list
    ;; Sums
    (rule `(+ (? a)) a)
    (rule `(+ (?? a) (+ (?? b)) (?? c))
	  `(+ ,@a ,@b ,@c))
    (rule `(+ (?? a) (? y) (? x) (?? b))
	  (and (expr<? x y)
	       `(+ ,@a ,x ,y ,@b)))
    ;; Products
    (rule `(* (? a)) a)
    (rule `(* (?? a) (* (?? b)) (?? c))
	  `(* ,@a ,@b ,@c))
    (rule `(* (?? a) (? y) (? x) (?? b))
	  (and (expr<? x y)
	       `(* ,@a ,x ,y ,@b)))
    ;; Distributive law
    (rule `(* (?? a) (+ (?? b)) (?? c))
	  `(+ ,@(map (lambda (x) `(* ,@a ,x ,@c)) b)))
    ;; Numerical simplifications below
    (rule `(+ 0 (?? x)) `(+ ,@x))
    (rule `(+ (? x ,number?) (? y ,number?) (?? z))
	  `(+ ,(+ x y) ,@z))
    (rule `(* 0 (?? x)) 0)
    (rule `(* 1 (?? x)) `(* ,@x))
    (rule `(* (? x ,number?) (? y ,number?) (?? z))
	  `(* ,(* x y) ,@z))
    ;; Like grouping
    ; (+ ... x ... x ... x) OR (+ ... (* x y) ... (* x y) ...)
    (rule `(+ (?? a) (? x) (?? b) (? x) (?? c))
	  `(+ ,@a ,@b (* 2 ,x) ,@c))
    
    ; (+ ... x ... (* k x) ...) 
    (rule `(+ (?? a) (? x) (?? b) (* (? k ,number?) (? x)) (?? c))
	  `(+ ,@a ,@b (* ,(+ k 1) ,x) ,@c))

    ; (+ ... (* x y) ... (* k x y) ...)
    (rule `(+ (?? a) 
	      (* (? x) (?? y)) 
	      (?? b) 
	      (* (? k, number?) (? x) (?? y))
	      (?? c))
	  `(+ ,@a ,@b (* ,(+ k 1) ,x ,@y) ,@c))

    ; (+ ... (* k x y) ... (* l x y) ...)
    (rule `(+ (?? a) 
	      (* (? k, number?) (? x) (?? y)) 
	      (?? b) 
	      (* (? l, number?) (? x) (?? y))
	      (?? c))
	  `(+ ,@a ,@b (* ,(+ k l) ,x ,@y) ,@c))
    )))

(algebra-3 '(+ (* 3 (+ x 1)) -3)) ;-> (* 3 x)
(algebra-3 '(+ x x)) ;-> (* 2 x)
(algebra-3 '(+ (* a b c) (* a b c))) ;-> (* 2 a b c)
(algebra-3 '(+ x 9 (* 3 x))) ;-> (+ f (* 3 a b))
(algebra-3 '(+ (* a b) f (* 2 a b))) ;-> (+ f (*3 a b))
(algebra-3 '(+ c (* 4 a b) f (* 2 a b) d a)) ;->  (+ a c d f (* 6 a b))
(algebra-3 '(+ (* 4 x) (* 3 x))) ;-> (* 7 x)
(algebra-3
 `(+ y (* x -2 w) (* x 4 y) (* w x) z (* 5 z) (* x w) (* x y 3)))
;Value: (+ y (* 6 z) (* 7 x y))
