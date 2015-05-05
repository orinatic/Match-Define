;;; Tom Boning
;;; Problem Set 5
;;; 6.905 Adventures in Advanced Symbolic Programming

(load "load.scm")

;;; Problem 5.1
(defhandler apply
  (lambda (procedure operands calling-environment)
    ;; Create a new vector of results by applying each function to the
    ;; operands serially.
    (let lp ((procedures (vector->list procedure))
	     (results '()))
      (if (null? procedures)
	  (list->vector results)
	  (let ((proc-result 
		 (apply (car procedures)
			operands calling-environment))
		(rest (cdr procedures)))
	    (lp rest (append results (list proc-result)))))))
  vector?)

;eval> ((vector cos sin) 0.6)
;#(.8253356149096783 .5646424733950354)

;;; Problem 5.2
;;; If we try to pass procedures defined in our interpreter to the
;;; underlying procedures that take procedural arguements, it does not
;;; work as they do not have the proper arguement passing that is
;;; expected. Specifically, the environment is not passed in the
;;; underlying scheme, so when it tries to call the lambda that was
;;; defined in our interpreter, it does not work, as the lambda needs
;;; that environment. 

;;; To fix this, we would need to check each time we hit an procedure
;;; that is defined in the underlying scheme, we could redefine it
;;; within our scheme.

;;; Problem 5.3

(define ALLOW-SELF-EVALUATING-SYMBOLS #f)

(define (default-apply procedure operands calling-environment)
  (if ALLOW-SELF-EVALUATING-SYMBOLS
      (cons procedure operands)
      (error "Unknown procedure type" procedure)))

(load "interp-2" (the-environment))


(defhandler eval 
  (lambda (exp env)
    (if (or (lookup-variable-value? exp env)
	    (not ALLOW-SELF-EVALUATING-SYMBOLS))
	(lookup-variable-value exp env)
	exp))
  variable?)

(define (lookup-variable-value? var env)
  (let plp ((env env))
    (if (eq? env the-empty-environment)
	(environment-bound? 
	 generic-evaluation-environment
	 var)
	(let scan
	    ((vars (vector-ref env 0))
	     (vals (vector-ref env 1)))
	  (cond ((null? vars) (plp (vector-ref env 2)))
		((eq? var (car vars)) #t)
		(else (scan (cdr vars) (cdr vals))))))))

(define +
  (make-generic-operator 
   2 'eval 
   (environment-lookup generic-evaluation-environment '+)))

(defhandler +
  (lambda (a b)
    (list '+ a b))
  symbol? any?)

(defhandler +
  (lambda (a b)
    (list '+ a b))
  any? symbol?)

(define -
  (make-generic-operator
   2 'eval
   (environment-lookup generic-evaluation-environment '-)))

(defhandler -
  (lambda (a b)
    (list '- a b))
  symbol? any?)

(defhandler -
  (lambda (a b)
    (list '- a b))
  any? symbol?) 

(define *
  (make-generic-operator
   2 'eval
   (environment-lookup generic-evaluation-environment '*)))

(defhandler *
  (lambda (a b)
    (list '* a b))
  symbol? any?)

(defhandler *
  (lambda (a b)
    (list '* a b))
  any? symbol?) 

(define /
  (make-generic-operator
   2 'eval
   (environment-lookup generic-evaluation-environment '/)))

(defhandler /
  (lambda (a b)
    (list '/ a b))
  symbol? any?)

(defhandler /
  (lambda (a b)
    (list '/ a b))
  any? symbol?) 

(define ALLOW-SELF-EVALUATING-SYMBOLS #f)
#|
eval> a
a

eval> (f)
(f)

eval> (+ 1 2)
3

eval> (* (+ 4 5) (+ a b))
(* 9 (+ a b))

|#
;;; Problem 5.4
;;; a) KONS is almost sufficient for ceating streams, but runs into
;;; issues because other forms don't have lazy evaluation, and so it
;;; may evaluate sooner than it should. In particular, trying to have
;;; streams reference eachother in circular manners does not work with
;;; just a KONS form.

;;; (load "general-procedures" generic-evaluation-environment)
;;; (load "kons" generic-evaluation-environment)

(define (add-streams s1 s2)
  (kons (+ (car s1) (car s2))
	(add-streams (cdr s1) (cdr s2))))

(define (ref-stream stream n)
  (if (= n 0)
      (car stream)
      (ref-stream (cdr stream) (- n 1))))

(define (map-stream proc items)
  (kons (proc (car items))
	(map-stream proc (cdr items))))

(define (scale-stream items factor)
  (map-stream (lambda (x) (* x factor))
	      items))

(define (integral (integrand lazy) initial-value dt)
  (define int
    (kons initial-value
	  (add-streams (scale-stream integrand dt)
		       int)))
  int)

(define (solve f y0 dt)
  (define y (integral dy y0 dt))
  (define dy (map-stream f y))
  y)

(ref-stream (solve (lambda (x) x) 1 0.001) 1000)
;;;eval> (ref-stream (solve (lambda (x) x) 1 0.001) 1000)
;;;;Unbound variable: dy

;;; b) We cannot define the y and dy in terms of eachother as they are
;;; in strict notation in the definitions. However, if we declare the
;;; dy operand as lazy in intergral, we avoid evaluating the undefined
;;; name until it is defined. In general, we should declare every
;;; stream as lazy in function definitions.

;;; 5.5 a) The advantage to not memoizing comes when we need to take
;;; advantage of random behavior. If we memoize randomized streams,
;;; then we are stuck with the value we get out. Additionally, it is
;;; possible that the computation depends on state outside the kons
;;; structure, such as global variables (such as a time based counter)
;;; that could change behavior. If we memoize, this would be
;;; detrimental.

;;; 5.5 b) This doesn't work because cons is strict so it immediately
;;; forces the evaluation of a and d.

;;; 5.5 c) If we use KONS instead of CONS, we cannot detect infinite
;;; streams. Infinite streams are really bad if code is not designed
;;; to handle them, as basically they loop forever. For instance,
;;; trying to take the average of an infinite stream would not
;;; terminate.

;;; 5.6 
;;; For my extension, I've created a way to produce lists of results
;;; of a function easily. Though you can do this without an eval apply
;;; extension, this makes it easy. For instance, this extension allows
;;; us to get lists of random integers easily, which we could feed
;;; into other functions.

(defhandler apply
  (lambda (procedure operands calling-environment)
    ;; Check for each function that the number of operands is correct
    ;; Create a new vector of results by applying each function to the
    ;; operands serially.
    (let ((proc (car operands))
	  (ops (cdr operands)))
      (let lp ((iteration procedure)
	       (results '()))
      (if (= 0 iteration)
	  results
          (let ((proc-result
                 (apply (eval proc calling-environment)
		  ops
		  calling-environment)))
            (lp (- iteration 1) 
		(append results (list proc-result))))))))
  integer?)

(define gen-counter
  (lambda ()
    (let ((n 0))
      (lambda () (set! n (+ n 1)) n))))

#|
eval> (100 random 100)
(83 93 93 3 21 91 77 23 17 65 ...)

eval> (let ((g (gen-counter)))
	(100 g))
(1 2 3 4 5 6 7 8 9 10 ...)

eval> (let ((g (gen-counter)))
(100 (vector g g g)))
(#(1 2 3) #(4 5 6) #(7 8 9) #(10 11 12) #(13 14 15) #(16 17 18) 
 #(19 20 21) #(22 23 24) #(25 26 27) #(28 29 30) ...) 

eval> ((+ 2 3) + 2 3) 
(5 5 5 5 5)
|#
