;; common

(define (enumerate-interval low high)
  (if (> low high)
      ()
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) ())
	((not (pair? tree)) (list tree))
	(else (append (enumerate-tree (car tree))
		      (enumerate-tree (cdr tree))))))

(define (map proc items)
  (if (null? items)
      ()
      (cons (proc (car items))
	    (map proc (cdr items)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (filter predicate sequence)
  (cond ((null? sequence) ())
	((predicate (car sequence))
	 (cons (car sequence)
	       (filter predicate (cdr sequence))))
	(else (filter predicate (cdr sequence)))))

;; math

(define (fib n)
  (define (iter a b k)
    (cond 
	  ((= k n) b)
	  (else (iter b (+ a b) (+ k 1)))))
  (iter 1 0 0))
  
;; test code

(define tree-1 (list 1 (list 2 (list 3 16)) 5 (list 6 7)))

(define list-1 (list 2 3 4 5 6 8 9))

(define (sum-odd-squares tree)
  (accumulate +
	      0
	      (filter odd? (map square
				(enumerate-tree tree)))))

(define (even-fibs n)
  (accumulate cons
	      ()
	      (filter even? (map fib
				 (enumerate-interval 0 n)))))

(define (list-fib-squares n)
  (accumulate cons
	      ()
	      (map square (map fib
			       (enumerate-interval 0 n)))))

; exec 2.33

(define (new-map p sequence)
  (accumulate (lambda (x y)
		(cons (p x) y))
	      ()
	      sequence))

(define (new-append seq1 seq2)
  (accumulate cons
	      seq1
	      seq2))

(define (new-length sequence)
  (accumulate (lambda (x y)
		(+ 1 y))
	      0
	      sequence))
	      
