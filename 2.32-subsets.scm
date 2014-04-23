(define (subsets s)
  (if (null? s)
      (list ())
      (let ((rest (subsets (cdr s))))
	(append rest (map (lambda (item)
			    (append (list (car s)) item))
			  rest)))))

(define set (list 1 2 3))

(subsets set)
		      
