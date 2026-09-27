;;; Info Begin

(define install-version "0.3.8")

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

(define (run! cmd)
  ;; Run a shell command; #t only when it exits with status 0.
  (zero? (system cmd)))

(define (system-return cmd)
  ;; Capture the output of a shell command, with surrounding whitespace trimmed
  (let* ([ports (process cmd)]
         [out (car ports)]
         [rst (get-string-all out)])
    (close-port out)
    (close-port (cadr ports))
    (if (eof-object? rst) "" (string-trim rst))))

(define (string-trim str)
  (let loop ([start 0] [end (string-length str)])
    (cond
      [(and (< start end) (char-whitespace? (string-ref str start))) (loop (1+ start) end)]
      [(and (< start end) (char-whitespace? (string-ref str (1- end)))) (loop start (1- end))]
      [else (substring str start end)])))

(define (delete-if-exists path)
  ;; Also removes dangling symbolic links (file-exists? would follow them)
  (when (file-exists? path #f)
    (delete-file path)))

(define (newest-version)
  (define ver (system-return (string-append "curl -s " raven-url)))
  ;; Accept only a plain version string, never an error page from a proxy/server.
  (if (and (> (string-length ver) 0)
           (for-all (lambda (c) (or (char-numeric? c) (char=? c #\.))) (string->list ver)))
      ver
      #f))

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

(define (download-raven ver)
  ;; Download raven@ver and extract it into target-path/raven.
  ;; The existing installation is only removed once the download succeeded.
  (let* ([dir (format "~a/raven" target-path)]
         [ok (and (run! (format "~a ~a && curl -f -# -o raven.tar.gz ~a/~a"
                          (if windows? "cd /d" "cd") target-path raven-url ver))
                  (begin
                    (clear-directory dir)
                    (mkdir dir)
                    (if windows?
                      (run! (format "cd /d ~a && 7z x raven.tar.gz -y -aoa >> install.log && 7z x raven.tar -o~a -y -aoa >> install.log"
                              target-path dir))
                      (run! (format "tar -xzf ~a/raven.tar.gz -C ~a" target-path dir)))))])
    (for-each delete-if-exists
      (list (format "~a/raven.tar.gz" target-path)
            (format "~a/raven.tar" target-path)
            (format "~a/install.log" target-path)))
    ok))

(define (install)
  (define ver (newest-version))
  (cond
    [(not ver)
      (printf "cannot get the latest raven version from ~a\n" raven-url)]
    [else
      (unless (file-directory? target-path)
        (mkdir target-path))
      (printf "loading raven ~a ......\n" ver)
      (cond
        [(not (download-raven ver))
          (printf "install raven ~a fail\n" ver)]
        [windows?
          (printf "The script has been downloaded in ~a\\raven\nYou should add this path to the system variables PATH before you enjoy the raven\n" target-path)
          (printf "install raven ~a success\n" ver)]
        [else
          (delete-if-exists "/usr/local/bin/raven")
          (if (and (run! "ln -s /usr/local/lib/raven/raven/raven.sc /usr/local/bin/raven")
                   (run! "chmod +x /usr/local/bin/raven"))
            (printf "install raven ~a success\n" ver)
            (printf "install raven ~a fail: cannot link /usr/local/bin/raven\n" ver))])]))

;;; Helper End

(install)

(exit)
