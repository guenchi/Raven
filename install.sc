;;; Info Begin

(define install-version "0.2.0")

(define windows? 
  (case (machine-type)
    ((a6nt i3nt ta6nt ti3nt) #t)
    (else #f)))

(define target-linux-path "/usr/local/lib/raven")

(define target-window-path (string-append (or (getenv "UserProfile") "c:") "\\raven"))

(define target-path (if windows? target-window-path target-linux-path))

(define raven-url "http://ravensc.com/raven")

;;; Info End

;;; Helper Begin

(define (read-file file-name)
  (let ((p (open-input-file file-name)))
      (let loop ((lst '()) (c (read-char p)))
          (if (eof-object? c)
              (begin 
                  (close-input-port p)
                  (list->string (reverse lst)))
              (loop (cons c lst) (read-char p))))))

(define (system-return cmd)
  (define tmp "./##tmp##")
  (define rst "")
  (and (zero? (system (string-append cmd " > " tmp)))
    (file-exists? tmp)
    (begin (set! rst (read-file tmp))))
  (delete-file tmp)
  rst
)

(define (newest-version)
  (define ver (system-return (string-append "curl -s " raven-url)))
  (if (or (string-ci=? ver "#f") (string-ci=? ver ""))
      #f
      ver))

(define (clear-directory path)
  (when (file-directory? path)
    (for-each 
      (lambda (p)
        (let ([p2 (string-append path "/" p)])
          (if (file-directory? p2)
            (clear-directory p2)
            (delete-file p2)
          )))
      (directory-list path))
    (delete-directory path)))

(define (install)
  (define ver (newest-version))
  (if ver
    (begin
      (unless (file-directory? target-path)
        (mkdir target-path))
      (clear-directory (format "~a/raven" target-path))
      (printf "loading raven ~a ......\n" ver)
      (if windows?
        (if (and 
              (system (format "cd ~a && curl -# -o raven.tar.gz ~a/~a && 7z x raven.tar.gz -y -aoa >> install.log && 7z x raven.tar -o~a/raven -y -aoa >> install.log"
                                target-path raven-url ver target-path))
              (delete-file (format "~a/raven.tar.gz" target-path))
              (delete-file (format "~a/raven.tar" target-path))
              (delete-file (format "~a/install.log" target-path)))
          (begin
            (printf "The script has been downloaded in ~a\\raven\nYou should add this path to the system variables PATH before you enjoy the raven\n" target-path)
            (printf "install raven ~a success\n" ver))
          (printf "install raven ~a fail\n" ver)
        )
        (if (and
              (mkdir (format "~a/raven" target-path))
              (system (format "cd ~a && curl -# -o raven.tar.gz ~a/~a && tar -xzf raven.tar.gz -C ~a/raven"
                                target-path raven-url ver target-path))
              (delete-file (format "~a/raven.tar.gz" target-path)))
          (begin
            (delete-file "/usr/local/bin/raven")
            (system "ln -s /usr/local/lib/raven/raven/raven.sc /usr/local/bin/raven")
            (system "chmod +x /usr/local/bin/raven")
            (printf "install raven ~a success\n" ver))
          (printf "install raven ~a fail\n" ver)
        )
      )
    )
    (printf "dont't exist raven\n")
  )
)

;;; Helper End

(install)

(exit)
