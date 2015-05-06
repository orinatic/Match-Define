;;;; Matcher based on match combinators, CPH/GJS style.
;;;     Idea is in Hewitt's PhD thesis (1969).

(declare (usual-integrations))

;;; There are match procedures that can be applied to data items.  A
;;; match procedure either accepts or rejects the data it is applied
;;; to.  Match procedures can be combined to apply to compound data
;;; items.

;;; A match procedure takes a list containing a data item, a
;;; dictionary, and a success continuation.  The dictionary
;;; accumulates the assignments of match variables to values found in
;;; the data.  The success continuation takes two arguments: the new
;;; dictionary, and the number of items absorbed from the list by the
;;; match.  If a match procedure fails it returns #f.

;;; Primitive match procedures:

(define (match:pattern-env? pattern-env)
  (and (pair? pattern-env)
       (eq? (car pattern-env) 'pattern-env))) 

(define (match:eqv pattern-constant pattern-env)
  (define (eqv-match data dictionary succeed)
    (and (pair? data)
   (eqv? (car data) pattern-constant)
   (succeed dictionary 1)))
  eqv-match)

(define (match:element variable restrictions pattern-env)
  (define (ok? datum)
    (every (lambda (restriction)
       (restriction datum))
     restrictions))
  (define (element-match data dictionary succeed)
    (and (pair? data)
   (ok? (car data))
   (let ((vcell (match:lookup variable dictionary pattern-env)))
     (if vcell
         (and (equal? (match:value vcell pattern-env) (car data))
        (succeed dictionary 1))
         (succeed (match:bind variable
            (car data)
            dictionary
            pattern-env)
      1)))))
  element-match)


;;; Support for the dictionary.

(define (match:bind variable data-object dictionary pattern-env)
  (cons (list variable data-object) dictionary))

(define (match:lookup variable dictionary pattern-env)
  (assq variable dictionary))

(define (match:value vcell pattern-env)
  (cadr vcell))

(define (match:segment variable pattern-env)
  (define (segment-match data dictionary succeed)
    (and (list? data)
   (let ((vcell (match:lookup variable dictionary pattern-env)))
     (if vcell
         (let lp ((data data)
      (pattern (match:value vcell pattern-env))
      (n 0))
     (cond ((pair? pattern)
      (if (and (pair? data)
         (equal? (car data) (car pattern)))
          (lp (cdr data) (cdr pattern) (+ n 1))
          #f))
           ((not (null? pattern)) #f)
           (else (succeed dictionary n))))
         (let ((n (length data)))
     (let lp ((i 0))
       (if (<= i n)
           (or (succeed (match:bind variable
            (list-head data i)
            dictionary
            pattern-env)
            i)
         (lp (+ i 1)))
           #f)))))))
  segment-match)

(define (match:list pattern-env . match-combinators)
  (define (list-match data dictionary succeed)
    (and (pair? data)
	 (let lp ((lst (car data))
		  (matchers match-combinators)
		  (dictionary dictionary))
	   (cond ((pair? matchers)
		  ((car matchers)
		   lst
		   dictionary
		   (lambda (new-dictionary n)
		     (if (> n (length lst))
			 (error "Matcher ate too much."
				n))
		     (lp (list-tail lst n)
			 (cdr matchers)
			 new-dictionary))))
		 ((pair? lst) #f)
		 ((null? lst)
		  (succeed dictionary 1))
		 (else #f)))))
  list-match)

;;; Syntax of matching is determined here.

(define (match:element? pattern)
  (and (pair? pattern)
       (eq? (car pattern) '?)))

(define (match:segment? pattern)
  (and (pair? pattern)
       (eq? (car pattern) '??)))

(define (match:variable-name pattern pattern-env) (cadr pattern))
(define (match:restrictions pattern pattern-env) (cddr pattern))

(define (match:list? pattern)
  (and (list? pattern)
       (or (null? pattern)
     (not (memq (car pattern) '(? ??))))))

(define match:->combinator
  (make-generic-operator 2 'eqv match:eqv))

(define (match:->combinators pattern)
  (match:->combinator pattern (make-pattern-env '())))

(defhandler match:->combinator
  (lambda (pattern pattern-env)
    (match:element
     (match:variable-name pattern pattern-env)
     (match:restrictions pattern pattern-env)
     pattern-env))
  match:element?
  match:pattern-env?)

(defhandler match:->combinator
  (lambda (pattern pattern-env) 
    (match:segment (match:variable-name pattern pattern-env) pattern-env))
  match:segment?
  match:pattern-env?)

(defhandler match:->combinator
  (lambda (pattern pattern-env)
    (apply match:list 
	   pattern-env 
	   (map (lambda (p)
		  (match:->combinator p pattern-env))
		  pattern)))
  match:list?
  match:pattern-env?)

(define (matcher pattern)
  (let ((match-combinator (match:->combinators pattern)))
    (lambda (datum)
      (match-combinator (list datum)
      '()
      (lambda (dictionary n)
        (and (= n 1)
             dictionary))))))

(define (match:choice? pattern)
  (and (pair? pattern)
       (eq? (car pattern) '?:choice)))

(define (match:choice pattern-env . patterns)
  (define (choice-match data dictionary succeed)
    (and (pair? data)
	 (let lp ((matchers patterns))
	   (if (pair? matchers)
	       (let ((match 
		      ((car matchers)
		       data
		       dictionary
		       succeed)))
		 (if match
		     match
		     (lp (cdr matchers))))
	       #f))))
  choice-match)

(defhandler match:->combinator
  (lambda (pattern pattern-env) 
    (apply match:choice 
	   pattern-env
	   (map (lambda (p) 
		  (match:->combinator p pattern-env))
		pattern)))
  match:choice?
  match:pattern-env?)

(define (match:pletrec? pattern)
  (and (pair? pattern)
       (eq? (car pattern) '?:pletrec)))

(define (match:ref? pattern)
  (and (pair? pattern)
       (eq? (car pattern) '?:ref)))

(define (make-pattern-env parent)
  (list 'pattern-env (make-strong-eqv-hash-table) parent))

(define (match:pletrec defs pattern-env)
  (define (make-pletrec-def assoc)
    (let* ((name (car assoc))
	   (comp-pattern (match:->combinator (cadr assoc)
					     pattern-env)))
     ; (pp (list 'comp-pattern comp-pattern))
      (hash-table/put! (cadr pattern-env) name comp-pattern)))
 ; (pp (list 'match:pletrec defs pattern-env))
  (for-each make-pletrec-def defs))

(defhandler match:->combinator
  (lambda (pattern pattern-env)
    (let ((new-env (make-pattern-env pattern-env)))
      (match:pletrec (cadr pattern) new-env)
      (match:->combinator (caddr pattern) new-env)))
  match:pletrec?
  match:pattern-env?)

(define (get-pattern pattern-env key)
  (let ((value (hash-table/get (cadr pattern-env) key #f)))
    (if value
	value
	(if (null? (caddr pattern-env))
	    (lambda (!#rest args) #f)
	    (get-pattern (caddr pattern-env) key)))))

(define (match:ref name pattern-env)
  (define (ref-match data dictionary succeed)
    ((get-pattern pattern-env name)
     data
     dictionary
     succeed))
  ref-match)

(defhandler match:->combinator
  (lambda (pattern pattern-env)
    (match:ref
     (match:variable-name pattern pattern-env)
     pattern-env))
  match:ref?
  match:pattern-env?)

#|
(define (report-success dict n)
  (assert (= n 1))
  `(succeed ,dict))

((match:->combinators '(?:choice a b (? x) c))
 '(z)
 '()
 (lambda (d n) `(succeed ,d)))
;Value 54: (succeed ((x z))) 

((match:->combinators '(a ((? b) 2 3) 1 c))
 '((a (1 2 3) 1 c))
 '()
  report-success)

;Value: (succeed ((b 1)))

((match:->combinators '(a ((? b) 2 3) (? b) c))
 '((a (1 2 3) 2 c))
 '()
  report-success)
;Value: #f

((match:->combinators '(a ((? b) 2 3) (? b) c))
 '((a (1 2 3) 1 c))
 '()
  report-success)
;Value: (succeed ((b 1)))


((match:->combinators '(a (?? x) (?? y) (?? x) c))
 '((a b b b b b b c))
 '()
 (lambda (dict n)
   (assert (= n 1))
   (pp `(succeed ,dict))
   #f))
(succeed ((y (b b b b b b)) (x ())))
(succeed ((y (b b b b)) (x (b))))
(succeed ((y (b b)) (x (b b))))
(succeed ((y ()) (x (b b b))))
;Value: #f

((matcher '(a ((? b) 2 3) (? b) c))
 '(a (1 2 3) 1 c))
;Value: ((b 1))
|#

;;; Nice pattern inspection procedure that will be used by the
;;; pattern-directed invocation system.
#|
(define (match:pattern-names pattern pattern-env)
  (let loop ((pattern pattern) (names '()))
    (cond ((or (match:element? pattern pattern-env)
               (match:segment? pattern pattern-env))
           (let ((name
      (match:variable-name pattern)))
             (if (memq name names)
                 names
                 (cons name names))))
          ((list? pattern)
           (let elt-loop
         ((elts pattern) (names names))
             (if (pair? elts)
                 (elt-loop (cdr elts)
         (loop (car elts) names))
                 names)))
          (else names))))
|#
