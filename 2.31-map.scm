(define la (list 1 (list 2 (list 3 4) 5 (list 6 7))))
la

(define (map proc items)
  (if (null? items)
      ()
      (cons (proc (car items))
	    (map proc (cdr items)))))

(define (tree-map proc tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (tree-map proc sub-tree)
	     (proc sub-tree)))
       tree))

(define (tree-square tree)
  (tree-map square tree))

(tree-square la)
