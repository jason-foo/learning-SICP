;; binary search tree: basic

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

;; set

(define (element-of-set? x set)
  (cond ((null? set) false)
	((= x (entry set)) true)
	((< x (entry set))
	 (element-of-set? x (left-branch set)))
	(else
	 (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
	((= x (entry set)) set)
	((< x (entry set))
	 (make-tree (entry set)
		    (adjoin-set x (left-branch set))
		    (right-branch set)))
	((> x (entry set))
	 (make-tree (entry set)
		    (left-branch set)
		    (adjoin-set x (right-branch set))))))

;; test code

(define set-empty '())
(define set-1 (adjoin-set 1 set-empty))
(define set-2 (adjoin-set 2 set-1))

(define step 3)
(define (set-n n)
  (define (next s)
    (lambda (k)
      (+ k s)))
  (define (iter k res)
    (if (> k n)
	res
	(iter ((next step) k) (adjoin-set k res))))
  (iter 1 '()))

(define set-20 (set-n 20))
set-20
(define set-20+17 (adjoin-set 17 set-20))
set-20+17
