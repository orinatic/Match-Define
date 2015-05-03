(define apply
  (make-generic-operator 3 'apply default-apply))

(defhandler apply
  (lambda (procedure operands calling-environment)
    (define (evaluate-list operands)
      (cond ((null? operands) '())
	        ((null? (rest-operands operands))
		      (list (eval (first-operand operands)
				   calling-environment)))
		    (else
		          (cons (eval (first-operand operands)
				       calling-environment)
				   (evaluate-list (rest-operands
						   operands))))))
    (apply-primitive-procedure procedure
      (evaluate-list operands)))
  strict-primitive-procedure?)

(defhandler apply
  (lambda (procedure operands calling-environment)
    (if (not (= (length (procedure-parameters procedure))
		(length operands)))
	(error "Wrong number of operands supplied"))
    (let ((arguments
	      (map (lambda (parameter operand)
		       (evaluate-procedure-operand parameter
						         operand
							       calling-environment))
		   (procedure-parameters procedure)
		   operands)))
      (eval (procedure-body procedure)
	        (extend-environment
		      (map procedure-parameter-name
			     (procedure-parameters procedure))
		           arguments
			        (procedure-environment procedure)))))
  compound-procedure?)

(define evaluate-procedure-operand
  (make-generic-operator 3
			  'evaluate-operand
			   (lambda (parameter operand environment)
			        (eval operand environment))))

(define procedure-parameter-name
  (make-generic-operator 1 'parameter-name (lambda (x) x)))
