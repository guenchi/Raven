";"; exec scheme --script $0 "$@";

(define (test cmd)
  (printf "~a\n" (make-string 90 #\#))
  (printf "run: ~a\n" cmd)
  (printf "~a\n" (make-string 90 #\-))
  (system cmd)
  (printf "\n~a\n" (make-string 90 #\#)))

;; start
(test "raven")
(test "raven -v")
(test "raven -h")

(test "raven init -h")

(test "raven install")
(test "raven install -h")
(test "raven install irregex")
(test "raven install -dev json@0.5.1")
(test "raven install -g raven")

(test "raven uninstall")
(test "raven uninstall -h")
(test "raven uninstall raven")
(test "raven uninstall json -dev")

(test "raven run -h")






