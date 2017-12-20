#! /usr/local/bin/scheme -q

;;通过index 找值

(define index
	(lambda (str x)
		(if (null? str)
			'()
			(if (equal? (caar str) x)
				(cdar str)
				(index (cdr str) x)))))
