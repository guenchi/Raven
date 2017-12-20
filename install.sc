#! /usr/local/bin/scheme -q

;;通过index 找值

(define index
	(lambda (str x)
		(if (null? str)
			'()
			(if (equal? (caar str) x)
				(cdar str)
				(index (cdr str) x)))))

;格式

(("name" . "Raven")
("Vesion" . "0.0.0")
("keywords" ("util" "package manager" "chez-scheme"))
("author" ("quenchi" "chclock"))
("contributors" ("quenchi" "chclock"))
("priv" . "true")
("priv" . "true")
("pend" ("raven" . "1.0.1") ("irreg" . "1.0.9"))
("deded" . "deded"))
