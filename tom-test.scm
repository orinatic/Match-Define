;;; Testing a different match let

(load "ghelper")
(load "matcher")

(define (dict-let dicts . body)
  (define (dict-let-single alist dicts body)
    (if (null? alist)
	())))

(define (dict-define dict)
  (let ((upper-level (environment-parent (procedure-environment dict-define))))
    (define (dict-define-loop dict)
      (if (null? dict)
	  'done
	  (let ((var (caar dict))
		(val (cadar dict)))
	    (environment-define upper-level var val)
	    (dict-define-loop (cdr dict)))))
    (if (alist? dict)
	(dict-define-loop dict)
	'not-an-alist)))

(let ((d 4))
  (pp d)
  (dict-define '((dog 35)))
  (pp dog))
