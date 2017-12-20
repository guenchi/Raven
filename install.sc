#! /usr/local/bin/scheme -q

;;通过index 找值

(define index
	(lambda (str x)
		(if (null? str)
			'()
			(if (equal? (caar str) x)
				(cdar str)
				(index (cdr str) x)))))

;;增加键值对
(define push
	(lambda (str x y)
		(if (null? (cdr str))
			(if (null? (car str))
				(set-car! str (cons x y))
				(set-cdr! str (cons (cons x y) '())))
			(push (cdr str) x y))))

;;出栈
(define pop
	(lambda (str)
		(if (null? (cdr str))
			(let ((str str)(x (car str))) 
					(set-car! str '())
					x)
			(let loop ((str str)(x (cdr str)))
					(if (null? (cdr x))
						(begin    
							(set-cdr! str '())
							(car x))
						(loop (cdr str) (cdr x)))))))

;;出队
(define out
	(lambda (str)
		(let ((str str)(x (car str)))
			(if (null? (cdr str))
				(set-car! str '())
				(begin 
					(set-car! str (cadr str))
					(set-cdr! str (cddr str))))
			x)))				
				
				

;格式


(("name" . "Raven")
("Vesion" . "0.0.0")
("keywords" ("util" "package manager" "chez-scheme"))
("author" ("quenchi" "chclock"))
("contributors" ("quenchi" "chclock"))
("private" . "true")
("scripts" ("dev" . "") ("build" . "")("g" . "")("start" . "")("prcommit" . "")("lint" . ""))
("dependencies" ("raven" . "1.0.1") ("irreg" . "1.0.9"))
("devDependencies" ("raven" . "1.0.1") ("irreg" . "1.0.9")))
