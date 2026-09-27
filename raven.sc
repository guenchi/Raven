#!/bin/bash
":"; export CHEZSCHEMELIBDIRS=.:lib:/usr/local/lib && export CHEZSCHEMELIBEXTS=.chezscheme.sls::.chezscheme.so:.ss::.so:.sls::.so:.scm::.so:.sch::.so:.sc::.so && exec scheme --script "$0" "$@";


;;; Association List Begin

(define package-sc->scm
  (case-lambda
    ([] (package-sc->scm raven-pkg-path))
    ([path] (call-with-input-file path read))))

(define asl-ref
  (case-lambda
    ([asl key] (asl-ref asl key #f))
    ([asl key default] (let ([rst (assoc key asl)])
                     (if rst (cdr rst) default)))))

(define asl-set!
  (case-lambda
    ([asl key x y] 
      (cond
        [(not (assoc key asl)) (asl-set! asl key (list (cons x y)))]
        [(null? (asl-ref asl key)) (set-cdr! (assoc key asl) (list (cons x y)))]
        [else (asl-set! (asl-ref asl key) x y)]))
    ([asl x y]
      (if (equal? x (caar asl))
        (set-cdr! (car asl) y)
        (if (null? (cdr asl))
          (set-cdr! asl (cons (cons x y) '()))
          (asl-set! (cdr asl) x y))))))

(define asl-delete!
  (case-lambda
    ([asl key x] 
      (unless (null? (asl-ref asl key '()))
        (if (equal? x (caar (asl-ref asl key)))
          (set-cdr! (assoc key asl) (cdr (asl-ref asl key)))
          (asl-delete! (asl-ref asl key) x))))
    ([asl x]
      (unless (null? (cdr asl))
        (if (equal? x (caadr asl))
          (set-cdr! asl (cddr asl))
          (asl-delete! (cdr asl) x))))))

(define (write-asl-format p asl level)
  (let loop ([ls asl] [space (make-string (* level 4) #\space)])
    (unless (null? ls)
        (display space p)
        (if (and (pair? (cdar ls)) (list? (cdar ls)) (pair? (cadar ls)))
            (begin
                (display #\( p)
                (write (caar ls) p)
                (display " \n" p)
                (write-asl-format p (cdar ls) (1+ level))
                (display #\) p))
            (write (car ls) p))
        (unless (null? (cdr ls))
            (newline p)
            (loop (cdr ls) space)))))

(define (write-package-file path asl)
  ;; Write to a temporary file first so an error cannot leave package.sc truncated
  (let ([tmp (string-append path ".tmp")])
    (when (file-exists? tmp)
      (delete-file tmp))
    (call-with-output-file tmp
      (lambda (p) 
        (display #\( p)
        (write-asl-format p asl 0)
        (display #\) p)))
    (when (file-exists? path)
      (delete-file path))
    (rename-file tmp path)))

;;; Association List End

;;; Helper Begin

(define (run! cmd)
  ;; Run a shell command; #t only when it exits with status 0.
  ;; (Chez's `system` returns the exit code, and any integer is truthy.)
  (zero? (system cmd)))

(define (version->list ver)
  ;; "1.10.2" -> (1 10 2); non-numeric parts count as 0
  (let loop ([chars (string->list ver)] [cur '()] [acc '()])
    (define (part) (or (string->number (list->string (reverse cur))) 0))
    (cond
      [(null? chars) (reverse (cons (part) acc))]
      [(char=? (car chars) #\.) (loop (cdr chars) '() (cons (part) acc))]
      [else (loop (cdr chars) (cons (car chars) cur) acc)])))

(define (version>=? a b)
  ;; Numeric, segment-wise version comparison: (version>=? "0.10.0" "0.9.0") => #t
  (let loop ([x (version->list a)] [y (version->list b)])
    (cond
      [(and (null? x) (null? y)) #t]
      [(null? x) (loop '(0) y)]
      [(null? y) (loop x '(0))]
      [(> (car x) (car y)) #t]
      [(< (car x) (car y)) #f]
      [else (loop (cdr x) (cdr y))])))

(define (string-matches? str ok-char?)
  (and (string? str)
       (> (string-length str) 0)
       (for-all ok-char? (string->list str))))

(define (valid-lib-name? name)
  ;; Library names are interpolated into shell commands and file paths,
  ;; so only allow a safe charset and reject names like "." or "..".
  (and (string? name)
       (> (string-length name) 0)
       (not (char=? (string-ref name 0) #\.))
       (string-matches? name
    (lambda (c) (or (char-alphabetic? c) (char-numeric? c) (memv c '(#\- #\_ #\.)))))))

(define (valid-version? ver)
  (string-matches? ver
    (lambda (c) (or (char-numeric? c) (char-alphabetic? c) (memv c '(#\. #\- #\_))))))

(define (read-file file-name)
  ;; Read a whole file into a string
  (call-with-input-file file-name
    (lambda (p)
      (let ([s (get-string-all p)])
        (if (eof-object? s) "" s)))))

(define (write-file file-name content)
  ;; Write a string to a file, replacing it
  (delete-file file-name)
  (call-with-output-file file-name
    (lambda (p) (put-string p content))))

(define (make-package-asl name version description author private)
  ;; Default package.sc content
  (list 
    (cons "name" name)
    (cons "version" version)
    (cons "description" description)
    (cons "keywords" '())
    (cons "author" `((,author)))
    (cons "private" private)
    (cons "scripts" '(("repl" . "scheme") ("run" . "scheme --script")))
    (cons "dependencies" '())
    (cons "devDependencies" '())
))

(define console-readline
  ;; Read a line from the console
  (case-lambda
    ([] (console-readline #f #f))
    ([prompt] (console-readline prompt #f))
    ([prompt default] (begin
      (when prompt (printf prompt))
      (let loop ([c (read-char)] [lst '()])
        (if (or (eof-object? c) (char=? c #\newline))
          (if (or (null? lst) (and (char=? (car lst) #\return) (= 1 (length lst))))
            (or default "")
            (if (char=? (car lst) #\return)
              (apply string (reverse (cdr lst)))
              (apply string (reverse lst))))
          (loop (read-char) (cons c lst))))))))

(define (create-pkg-file)
  (define name (console-readline "project name: "))
  (define version (console-readline "version(0.1.0): " "0.1.0"))
  (define description (console-readline "description: "))
  (define author (console-readline (format "author(~a): " raven-user) raven-user))
  (define private (console-readline "private?(Y/n): " "y"))
  (set! private (not (string-ci=? private "n")))
  (let ([asl (make-package-asl name version description author private)])
    (when (file-exists? raven-pkg-path)
      (let ([old-asl (package-sc->scm)])
        (asl-set! asl raven-depend-key (asl-ref old-asl raven-depend-key '()))
        (asl-set! asl raven-dev-depend-key (asl-ref old-asl raven-dev-depend-key '())))
      (delete-file raven-pkg-path))
    (write-package-file raven-pkg-path asl))
)

(define load-lib
  (case-lambda
    ([lib ver] (load-lib lib ver raven-library-path))
    ([lib ver lib-path] (load-lib lib ver lib-path #f))
    ([lib ver lib-path check?] (load-lib lib ver lib-path check? #t))
    ([lib ver lib-path check? printf?] 
      (begin
        (unless ver
          (set! ver (newest-version lib)))
        (cond
          [(not (valid-lib-name? lib))
            (printf "invalid library name: ~s\n" lib)
            #f]
          [(not ver)
            (printf "cannot find ~a in the registry\n" lib)
            #f]
          [(not (valid-version? ver))
            (printf "invalid version for ~a: ~s\n" lib ver)
            #f]
          [(and (not check?) (equal? (installed-version lib lib-path) ver))
            (when printf?
              (printf "~a ~a is already installed\n" lib ver))
            #t]
          [else
        (unless (file-directory? lib-path)
          (mkdir lib-path))
        (when printf?
          (printf (format "loading ~a ~a ......\n" lib ver)))
        (if (and check? 
              (file-exists? (format "~a/~a/~a" lib-path lib raven-pkg-file))
              (version>=? (asl-ref (package-sc->scm (format "~a/~a/~a" lib-path lib raven-pkg-file)) "version" "0.0.0") ver))
          (printf "a high version ~a ~a exists\nstop loading ~a ~a\n"
              lib (asl-ref (package-sc->scm (format "~a/~a/~a" lib-path lib raven-pkg-file)) "version") lib ver)
          (if (download-lib lib ver lib-path)
            (begin
              (when (file-exists? (format "~a/~a/~a" lib-path lib raven-pkg-file))
                (let* ([asl (package-sc->scm (format "~a/~a/~a" lib-path lib raven-pkg-file))]
                       [libs-asl (asl-ref asl raven-depend-key '())]
                       [scripts (asl-ref asl "scripts" '())]
                       [build (asl-ref scripts "build")])
                  (for-each 
                    (lambda (lib/ver) 
                      (load-lib (car lib/ver) (cdr lib/ver) lib-path #t #t))
                    libs-asl)
                  (when build
                    (if raven-ignore-scripts?
                      (printf "skip build script of ~a: ~a\n" lib build)
                      (begin
                        (printf "running build script of ~a: ~a\n" lib build)
                        (unless (run! build)
                          (printf "warning: build script of ~a failed\n" lib)))))))
              (when printf? (printf (format "load ~a ~a success\n" lib ver)))
              #t)
            (begin
              (when printf? (printf (format "load ~a ~a fail\n" lib ver)))
              #f)
          )
        )])
      )
    )
  )
)

(define (delete-if-exists path)
  ;; Also removes dangling symbolic links (file-exists? would follow them)
  (when (file-exists? path #f)
    (delete-file path)))

(define (download-lib lib ver lib-path)
  ;; Download lib@ver and extract it into lib-path/lib.
  ;; The existing installation is only removed once the download succeeded.
  (let* ([dir (format "~a/~a" lib-path lib)]
         [ok (and (run! (format "~a ~a && curl -f -# -o ~a.tar.gz ~a/~a/~a"
                          (if raven-windows? "cd /d" "cd") lib-path lib raven-url lib ver))
                  (begin
                    (clear-directory dir)
                    (mkdir dir)
                    (if raven-windows?
                      (run! (format "cd /d ~a && 7z x ~a.tar.gz -y -aoa >> install.log && 7z x ~a.tar -o~a -y -aoa >> install.log"
                              lib-path lib lib dir))
                      (run! (format "tar -xzf ~a/~a.tar.gz -C ~a" lib-path lib dir)))))])
    (for-each delete-if-exists
      (list (format "~a.tar.gz" dir) (format "~a.tar" dir) (format "~a/install.log" lib-path)))
    ok))

(define (lib-dependencies lib lib-path)
  ;; Names of the dependencies declared in lib-path/lib/package.sc
  (let ([path (format "~a/~a/~a" lib-path lib raven-pkg-file)])
    (if (file-exists? path)
        (filter valid-lib-name?
          (map car (asl-ref (package-sc->scm path) raven-depend-key '())))
        '())))

(define (lib-closure libs lib-path)
  ;; libs plus everything they depend on, transitively, as installed in lib-path
  (let loop ([todo libs] [seen '()])
    (cond
      [(null? todo) (reverse seen)]
      [(member (car todo) seen) (loop (cdr todo) seen)]
      [else (loop (append (lib-dependencies (car todo) lib-path) (cdr todo))
                  (cons (car todo) seen))])))

(define (installed-version lib lib-path)
  ;; Version recorded in lib-path/lib/package.sc, or #f when not installed
  (let ([path (format "~a/~a/~a" lib-path lib raven-pkg-file)])
    (and (file-exists? path)
         (asl-ref (package-sc->scm path) "version" #f))))

(define (opt-string? str)
  ;; Is this argument an option?
  (and (> (string-length str) 1)
       (string-ci=? (substring str 0 1) "-")))

(define (string->opt str)
  ;; Convert an option string to a symbol
  (string->symbol (substring str 1 (string-length str))))

(define (clear-directory path)
  ;; Recursively empty and remove a directory
  (when (file-directory? path)
    (for-each 
      (lambda (p)
        (let ([p2 (string-append path "/" p)])
          (if (file-directory? p2)
            (clear-directory p2)
            (delete-file p2 #t)
          )))
      (directory-list path))
    (delete-directory path #t)
  )
)

(define (delete-file/directory path)
  (if (file-directory? path)
    (clear-directory path)
    (delete-file path #t))
)

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

(define (newest-lib/version lib)
  ;; get lib's version from server
  (define splite-index (string-index lib #\@))
  (if splite-index
    (let ([name (substring lib 0 splite-index)]
          [ver (substring lib (1+ splite-index) (string-length lib))])
      (if (string=? ver "")
        (cons name (newest-version name))
        (cons name ver)))
    (cons lib (newest-version lib))
  )
)

(define (newest-version lib)
  ;; Fetch the latest version of a library from the registry
  (define ver
    (and (valid-lib-name? lib)
         (system-return (format "curl -s ~a/~a" raven-url lib))))
  (if (valid-version? ver)
      ver
      #f)
)

(define (ask-Y/n? tip)
  ;; Input request Y/n
  (printf (format "~a(Y/n)" tip))
  (not (string-ci=? (console-readline) "n"))
)

(define (string-index str chr)
  (define len (string-length str))
  (do ((pos 0 (+ 1 pos)))
      ((or (>= pos len) (char=? chr (string-ref str pos)))
       (and (< pos len) pos))))

(define (string-join string-list sep)
  (let loop ([new-s '()] [old-s string-list])
      (if (null? old-s)
        (if (null? new-s)
          ""  
          (apply string-append (reverse (cdr new-s))))
        (loop (cons* sep (car old-s) new-s) (cdr old-s)))
  )
)

;;; Helper End

;;; Command Begin

(define (init opts args)
  ;; Initial
  (cond
    ((member "-h" opts) (raven-printf-help "init-h"))
    (else (begin
      (create-pkg-file)
      (unless (file-directory? raven-library-path)
        (mkdir raven-library-path))
      (let ([libs (asl-ref (package-sc->scm) raven-current-key '())])
        (for-each (lambda (l/v) (load-lib (car l/v) (cdr l/v))) libs))
      (printf "raven init over\n")))
  )
)

(define (install opts libs)
  ;; Installation
  (cond
    ((member "-h" opts) (raven-printf-help "install-h"))
    (else (begin
      (unless (or raven-global? (file-exists? raven-pkg-path))
        (write-package-file raven-pkg-path (make-package-asl "" "" "" raven-user #f)))
      (unless (file-directory? raven-library-path)
        (mkdir raven-library-path))
      (if (null? libs)
          (if raven-global?
            (printf "please add library name\n")
            (let* ([asl (package-sc->scm)]
                  [libs-asl (asl-ref asl raven-current-key '())])
              (for-each (lambda (l/v) (load-lib (car l/v) (cdr l/v))) libs-asl)
              (printf "install all libraries over\n")))
          (if raven-global?
            (for-each
              (lambda (name)
                (let ([lib/ver (newest-lib/version name)])
                  (if (cdr lib/ver)
                    (let* ([lib (car lib/ver)]
                          [ver (cdr lib/ver)]
                          [rst (load-lib lib ver)])
                      (when rst
                        (if raven-windows?
                          (printf "~a has been downloaded in ~a\\~a\n" lib raven-library-path lib)
                          (begin
                            (delete-if-exists (format "/usr/local/bin/~a" lib))
                            (if (and (run! (format "ln -s ~a/~a/~a.sc /usr/local/bin/~a" raven-library-path lib lib lib))
                                     (run! (format "chmod +x /usr/local/bin/~a" lib)))
                              (printf "install ~a ~a success\n" lib ver)
                              (printf "install ~a ~a fail: cannot link /usr/local/bin/~a\n" lib ver lib))))))
                    (printf (format "wrong library name: ~a\n" (car lib/ver))))))
              libs)  
            (let ([asl (package-sc->scm)])
              (for-each
                (lambda (name)
                  (let ([lib/ver (newest-lib/version name)])
                    (if (cdr lib/ver)
                      (let* ([lib (car lib/ver)]
                            [ver (cdr lib/ver)]
                            [rst (load-lib lib ver)])
                        (when rst
                          (unless (asl-ref asl raven-current-key)
                            (asl-set! asl raven-current-key '()))
                          (asl-set! asl raven-current-key lib ver)))
                      (printf (format "wrong library name: ~a\n" (car lib/ver))))))
                libs)
              (write-package-file raven-pkg-path asl)
              (printf "raven install over\n"))))))
  )
)

(define (uninstall opts libs)
  ;; Uninstallation
  (cond
    ((member "-h" opts) (raven-printf-help "uninstall-h"))
    (else (cond
      [(null? libs) (printf "please add library name\n")]
      [(find (lambda (name) (not (valid-lib-name? name))) libs)
        => (lambda (name) (printf "invalid library name: ~s\n" name))]
      [else
      ;; uninstall libs
      (if raven-global?
        (for-each 
          (lambda (name) 
            (if (file-exists? (format "~a/~a" raven-library-path name))
              (begin
                (printf "deleting ~a/~a ......\n" raven-library-path name)
                (delete-file/directory (format "~a/~a" raven-library-path name))
                (unless raven-windows?
                  (delete-if-exists (format "/usr/local/bin/~a" name)))
                (printf "uninstall ~a success\n" name))
              (printf "~a is not installed\n" name)))
          libs)
        (if (and (file-directory? raven-library-path) (file-exists? raven-pkg-path))
          (let* ([asl (package-sc->scm)]
                 [libs-asl (asl-ref asl raven-current-key '())]
                 [candidates (lib-closure libs raven-library-path)])
            (for-each 
              (lambda (name)
                (if (or (assoc name libs-asl)
                        (file-directory? (format "~a/~a" raven-library-path name)))
                  (begin
                    (printf "deleting ~a/~a ......\n" raven-library-path name)
                    (clear-directory (format "~a/~a" raven-library-path name))
                    (asl-delete! asl raven-current-key name)
                    (printf "uninstall ~a success\n" name))
                  (printf "~a is not installed\n" name)))
              libs)
            (let ([required (lib-closure
                              (append (map car (asl-ref asl raven-depend-key '()))
                                      (map car (asl-ref asl raven-dev-depend-key '())))
                              raven-library-path)])
              (for-each
                (lambda (name)
                  (when (and (not (member name libs))
                             (not (member name required))
                             (file-directory? (format "~a/~a" raven-library-path name)))
                    (printf "removing unused dependency ~a\n" name)
                    (clear-directory (format "~a/~a" raven-library-path name))))
                candidates))
            (write-package-file raven-pkg-path asl)
            (printf "raven uninstall over\n"))
          (printf "please raven init first\n")
        ))]
    ))
  )
)

(define (pack opts args)
  (cond
    ((member "-h" opts) (raven-printf-help "pack-h"))
    ((not (file-exists? raven-pkg-path)) (printf "please run raven init first\n"))
    (else (let* ([asl (package-sc->scm)]
                 [ver (asl-ref asl "version" "")]
                 [lib (string-downcase (asl-ref asl "name" ""))]
                 [dir (if (null? args) "" (format "cd ~a &&" (car args)))])
     (cond
      [(not (valid-lib-name? lib))
        (printf "invalid package name in package.sc: ~s\n" lib)]
      [(not (valid-version? ver))
        (printf "invalid version in package.sc: ~s\n" ver)]
      [else
      (unless (null? args)
        (write-file (format "~a/~a/~a" raven-current-path (car args) raven-pkg-file) (read-file raven-pkg-path)))
      (if (if raven-windows?
        (and (run! 
               (format "~a 7z a ~a.tar ./ && 7z d ~a.tar lib -r && 7z d ~a.tar .* -r  && 7z d ~a.tar .tar -r && 7z d ~a.tar .tar.gz -r && 7z a ~a-~a.tar.gz ~a.tar"
                 dir ver ver ver ver ver lib ver ver))
          (begin
            (delete-file (format "~a/~a.tar" (if (null? args) "." (format"./~a" (car args))) ver))
            #t))
        (run! (format "~a tar -zcf ~a-~a.tar.gz --exclude lib --exclude \"*.tar.gz\" --exclude \".*\" *" dir lib ver)))
        (begin
          (unless (null? args)
            (if raven-windows?
              (system (format "move ~a\\~a-~a.tar.gz ~a-~a.tar.gz" (car args) lib ver lib ver))
              (system (format "mv ~a/~a-~a.tar.gz ~a-~a.tar.gz" (car args) lib ver lib ver))))
          (printf "raven library : ~a-~a.tar.gz is ready\n" lib ver))
        (printf "raven pack fail\n"))
      (unless (null? args)
        (delete-if-exists (format "~a/~a/~a" raven-current-path (car args) raven-pkg-file)))])))
  )
)

(define (self-command opts cmds)
  ;; Run a custom script from package.sc
  (cond
    ((and (string-ci=? (car cmds) "run") (member "-h" opts)) (raven-printf-help "run-h"))
    (else (if (file-exists? raven-pkg-path)
      (let* ([scripts (asl-ref (package-sc->scm) "scripts")]
             [cmd (if scripts (asl-ref scripts (car cmds)) #f)]
             [args (append (cdr cmds) opts)])
        (if cmd
          (system (format "~a ~a" cmd (string-join args " ")))
          (if (string-ci=? (car cmds) "run")
            (system (format "scheme --script ~a" (string-join args " ")))
            (printf "invaild command\n"))))
      (printf "please run raven init first\n")))
  )
)

;;; Command End

;;; Info Begin

(define raven-url "http://ravensc.com")

(define raven-windows? 
  (case (machine-type)
    ((a6nt i3nt ta6nt ti3nt) #t)
    (else #f)))

(define raven-user (if raven-windows? (or (getenv "USERNAME") "") (or (getenv "USER")"")))

(define raven-current-path (current-directory))

(define raven-library-dir "lib")

(define raven-library-path (format "~a/~a" raven-current-path raven-library-dir))

(define raven-pkg-file "package.sc")

(define raven-pkg-path (format "~a/~a" raven-current-path raven-pkg-file))

(define raven-depend-key "dependencies")

(define raven-dev-depend-key "devDependencies")

(define raven-current-key raven-depend-key)

(define raven-global-path (if raven-windows? (string-append (or (getenv "UserProfile") "C:") "\\raven") "/usr/local/lib/raven"))

(define raven-global-dir "raven")

(define raven-global? #f)

(define raven-ignore-scripts? #f)

(define raven-version
  ;; Read from the global installation, or from package.sc next to this script
  (let ([paths (list (format "~a/raven/~a" raven-global-path raven-pkg-file)
                     (format "~a/~a" (path-parent (car (command-line))) raven-pkg-file))])
    (cond
      [(find file-exists? paths) => (lambda (path) (asl-ref (package-sc->scm path) "version" "unknown"))]
      [else "unknown"])))

;;; Info End

;;; Main Begin

(define (raven-init)
  ;; Initialize the environment
  #f
)

(define (init-opts opts)
  ;; Apply command-line options
  (when (member "-g" opts)
    (set! raven-library-dir raven-global-dir)
    (set! raven-library-path raven-global-path)
    (set! raven-global? #t))
  (when (member "-dev" opts)
    (set! raven-current-key raven-dev-depend-key))
  (when (member "-ignore-scripts" opts)
    (set! raven-ignore-scripts? #t))
)

(define (check-version)
  ;; Print the raven version
  (printf (format "Raven version: ~a\n" raven-version))
)

(define raven-help
  '(
    ("raven-h"
      . "\nUsage: raven <command> [option]\n\nwhere <command> is one of:\n\tinit, install, uninstall, run, pack\n\nraven <cmd> -h\tquick help on <cmd>\n\n")
    ("init-h"
      . "\nUsage:\n\nraven init\n\tcreat a file package.sc for a new project\n\n")
    ("install-h"
      . "\nUsage:\n\nraven install [option]\n\tinstall the \"dependencies\" of the package.sc\n\nraven install [option] <packageName>\n\tinstall the package of current version and update package.sc\n\nraven install [option] <packageName>@<version>\n\tinstall the package of specified version and update package.sc\n\n[option]:\n\t-ignore-scripts: do not run the \"build\" scripts of installed packages\n\t-dev: work with \"devDependencies\" instead of \"dependencies\"\n\t-g: install package as a CLI tool. need root permissions.\n\n")
    ("uninstall-h"
      . "\nUsage:\n\nraven uninstall [option] <packageName>\n\tremove the package and update package.sc\n\n[option]:\n\t-dev: work with \"devDependencies\" instead of \"dependencies\"\n\t-g: remove a CLI tool. need root permissions.\n\n")
    ("pack-h"
      . "\nUsage:\n\nraven pack\n\tpacking the current project in file tar.gz\n\n")
    ("run-h"
      . "\nUsage:\n\nraven run\n\truning the current project\n\n")
  )
)

(define raven-printf-help
  (case-lambda
    ([key] (raven-printf-help key ""))
    ([key default] (printf (asl-ref raven-help key default))))
)

(define (global-opts opts)
  (case (car opts)
    [("-v" "--version") (check-version)]
    [("-h" "--help") (raven-printf-help "raven-h")]
    [else (raven-printf-help "raven-h")]
  )
)

(define (raven)
  ;; raven entry point
  (define args (command-line-arguments))
  (raven-init)
  (if (null? args)
    (raven-printf-help "raven-h")
    (let-values 
      ([(opts cmds) (partition opt-string? args)])
      (init-opts opts)
      (if (null? cmds)
        (global-opts opts)
        (case (car cmds)
          [("init") (init opts (cdr cmds))]
          [("install") (install opts (cdr cmds))]
          [("uninstall") (uninstall opts (cdr cmds))]
          [("pack") (pack opts (cdr cmds))]
          [else (self-command opts cmds)]))
    )
  )
)

;;; Main End

;; start
(raven)
