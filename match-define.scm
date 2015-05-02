(load "~/Documents/6.945/Match-Define/load.scm")
(define-syntax match-define
  (syntax-rules ()
    ((_ dict)
     ;(let ((dict ((match->combinators names) (list vals) '() 
     ;(lambda (d n) d))))
     (for-each (lambda name val)
       (define names vals))))


(define (succeed-fn d n) `(succeed ,d)))

(define dict ((match:->combinators '((? y) (? x))) '((1 2)) '()
	      (lambda (d n) d)))

dict


(match-define bar 1)

bar
  
