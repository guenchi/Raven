":"; export CHEZSCHEMELIBDIRS=.:lib:/usr/local/lib && export CHEZSCHEMELIBEXTS=.chezscheme.sls::.chezscheme.so:.ss::.so:.sls::.so:.scm::.so:.sch::.so:.sc::.so && exec scheme --script $0 "$@";


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
      (if (null? (asl-ref asl key))
        (set-cdr! (assoc key asl) (cons (cons x y) '()))
        (asl-set! (asl-ref asl key) x y)))
    ([asl x y]
      (if (equal? x (caar asl))
        (set-cdr! (car asl) y)
        (if (null? (cdr asl))
          (set-cdr! asl (cons (cons x y) '()))
          (asl-set! (cdr asl) x y))))))

(define asl-delete!
  (case-lambda
    ([asl key x] 
      (unless (null? (asl-ref asl key))
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
  (delete-file path)
  (call-with-output-file path 
    (lambda (p) 
      (display #\( p)
      (write-asl-format p asl 0)
      (display #\) p))))

;;; Association List End

;;; Helper Begin

(define (read-file file-name)
  ;; 读取文件
  (let ((p (open-input-file file-name)))
      (let loop ((lst '()) (c (read-char p)))
          (if (eof-object? c)
              (begin 
                  (close-input-port p)
                  (list->string (reverse lst)))
              (loop (cons c lst) (read-char p))))))

(define (write-file file-name content)
  ;; 写文件
  (delete-file file-name)
  (let ([p (open-output-file file-name)] [len (string-length content)])
    (let loop ([idx 0])
      (when (< idx len)
          (write-char (string-ref content idx) p)
          (loop (add1 idx))))
    (close-output-port p)
  )
)

(define (make-package-asl name version description author private)
  ;; 默认package内容
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
  ;; 获取控制台输入
  (case-lambda
    ([] (console-readline #f #f))
    ([prompt] (console-readline prompt #f))
    ([prompt default] (begin
      (when prompt (printf prompt))
      (let loop ([c (read-char)] [lst '()])
        (if (char=? c #\newline)
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
        (unless (file-directory? lib-path)
          (mkdir lib-path))
        (unless check?
          (clear-directory (format "~a/~a" lib-path lib)))
        (when printf?
          (printf (format "loading ~a ~a ......\n" lib ver)))
        (if (and check? 
              (file-exists? (format "~a/~a/~a" lib-path lib raven-pkg-file))
              (string-ci>=? (asl-ref (package-sc->scm (format "~a/~a/~a" lib-path lib raven-pkg-file)) "version" "0.0.0") ver))
          (printf "a high version ~a ~a exists\nstop loading ~a ~a\n"
              lib (asl-ref (package-sc->scm (format "~a/~a/~a" lib-path lib raven-pkg-file)) "version") lib ver)
          (if
            (if raven-windows?
              (and
                  (system (format "cd ~a && curl -# -o ~a.tar.gz ~a/~a/~a && 7z x ~a.tar.gz -y -aoa >> install.log && 7z x ~a.tar -o~a/~a -y -aoa >> install.log"
                            lib-path lib raven-url lib ver lib lib lib-path lib))
                  (delete-file (format "~a/~a.tar.gz" lib-path lib) #t)
                  (delete-file (format "~a/~a.tar" lib-path lib) #t)
                  (delete-file (format "~a/install.log" lib-path) #t))
              (and
                (mkdir (format "~a/~a" lib-path lib))
                (system (format "cd ~a && curl -# -o ~a.tar.gz ~a/~a/~a && tar -xzf ~a.tar.gz -C ~a/~a"
                          lib-path lib raven-url lib ver lib lib-path lib))
                (delete-file (format "~a/~a.tar.gz" lib-path lib) #t)))
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
                  (when build (system build))))
              (when printf? (printf (format "load ~a ~a success\n" lib ver)))
              #t)
            (begin
              (when printf? (printf (format "load ~a ~a fail\n" lib ver)))
              #f)
          )
        )
      )
    )
  )
)

(define (opt-string? str)
  ;; 是否为选项
  (and (> (string-length str) 1)
       (string-ci=? (substring str 0 1) "-")))

(define (string->opt str)
  ;; 获取选项
  (string->symbol (substring str 1 (string-length str))))

(define (clear-directory path)
  ;; 清空并删除文件夹
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
  ;; 读取命令行返回内容
  (define tmp "./._##tmp##")
  (define rst "")
  (and (zero? (system (string-append cmd " > " tmp)))
    (file-exists? tmp)
    (begin (set! rst (read-file tmp))))
  (delete-file tmp)
  rst
)

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
  ;; 获取最新库版本
  (define ver (system-return (format "curl -s ~a/~a" raven-url lib)))
  (if (or (string-ci=? ver "#f") (string-ci=? ver ""))
      #f
      ver)
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
                            (delete-file (format "/usr/local/bin/~a" lib) #t)
                            (system (format "ln -s ~a/~a/~a.sc /usr/local/bin/~a" raven-library-path lib lib lib))
                            (system (format "chmod +x /usr/local/bin/~a" lib))
                            (printf "install ~a ~a success\n" lib ver)))))
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
    (else (if (null? libs)
      (printf "please add library name\n")
      ;; uninstall libs
      (if raven-global?
        (for-each 
          (lambda (name) 
            (printf "deleting ~a/~a ......\n" raven-library-path name)
            (delete-file/directory (format "~a/~a" raven-library-path name))
            (unless raven-windows?
              (delete-file (format "/usr/local/bin/~a" name)))
            (printf "uninstall ~a success\n" name))
          libs)
        (if (and (file-directory? raven-library-path) (file-exists? raven-pkg-path))
          (let* ([asl (package-sc->scm)]
                 [libs-asl (asl-ref asl raven-current-key '())])
            (for-each 
              (lambda (name)
                (printf "deleting ~a/~a ......\n" raven-library-path name)
                (clear-directory (format "~a/~a" raven-library-path name))
                (asl-delete! asl raven-current-key name) 
                (printf "uninstall ~a success\n" name)) 
              libs)
            (write-package-file raven-pkg-path asl)
            (printf "raven uninstall over\n"))
          (printf "please raven init first\n")
        ))
    ))
  )
)

(define (pack opts args)
  (cond
    ((member "-h" opts) (raven-printf-help "pack-h"))
    (else (let* ([asl (package-sc->scm)]
                 [ver (asl-ref asl "version" "")]
                 [lib (string-downcase (asl-ref asl "name" ""))]
                 [dir (if (null? args) "" (format "cd ~a &&" (car args)))])
      (unless (null? args)
        (write-file (format "~a/~a/~a" raven-current-path (car args) raven-pkg-file) (read-file raven-pkg-path)))
      (if raven-windows?
        (and (system 
               (format "~a 7z a ~a.tar ./ && 7z d ~a.tar lib -r && 7z d ~a.tar .* -r  && 7z d ~a.tar .tar -r && 7z d ~a.tar .tar.gz -r && 7z a ~a-~a.tar.gz ~a.tar"
                 dir ver ver ver ver ver lib ver ver))
          (delete-file (format "~a/~a.tar" (if (null? args) "." (format"./~a" (car args))) ver)))
        (system (format "~a tar -zcf ~a-~a.tar.gz --exclude lib --exclude \"*.tar.gz\" --exclude \".*\" *" dir lib ver)))
      (unless (null? args)
        (if raven-windows?
          (system (format "move ~a\\~a-~a.tar.gz ~a-~a.tar.gz" (car args) lib ver lib ver))
          (system (format "mv ~a/~a-~a.tar.gz ~a-~a.tar.gz" (car args) lib ver lib ver)))
        (delete-file (format "~a/~a/~a" raven-current-path (car args) raven-pkg-file)))
      (printf "raven library : ~a-~a.tar.gz is ready\n" lib ver)))
  )
)

(define (self-command opts cmds)
  ;; 自定义命令
  (cond
    ((and (string-ci=? (car cmds) "run") (member "-h" opts)) (raven-printf-help "run-h"))
    (else (if (file-exists? raven-pkg-path)
      (let* ([scripts (asl-ref (package-sc->scm) "scripts")]
             [cmd (if scripts (asl-ref scripts (car cmds)) #f)]
             [args (append (cdr cmds) opts)])
        (if cmd
          (system (format "~a ~a" cmd (apply string-append args)))
          (if (string-ci=? (car cmds) "run")
            (system (format "scheme --script ~a" (apply string-append args)))
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

(define raven-version (asl-ref (package-sc->scm (format "~a/raven/~a" raven-global-path raven-pkg-file)) "version" ""))

;;; Info End

;;; Main Begin

(define (raven-init)
  ;; 初始化环境
  #f
)

(define (init-opts opts)
  ;; 检测环境
  (when (member "-g" opts)
    (set! raven-library-dir raven-global-dir)
    (set! raven-library-path raven-global-path)
    (set! raven-global? #t))
  (when (member "-dev" opts)
    (set! raven-current-key raven-dev-depend-key))
)

(define (check-version)
  ;; 运行前检查版本
  (printf (format "Raven version: ~a\n" raven-version))
)

(define raven-help
  '(
    ("raven-h"
      . "\nUsage: raven <command> [option]\n\nwhere <command> is one of:\n\tinit, install, uninstall, run, pack\n\nraven <cmd> -h\tquick help on <cmd>\n\n")
    ("init-h"
      . "\nUsage:\n\nraven init\n\tcreat a file package.sc for a new project\n\n")
    ("install-h"
      . "\nUsage:\n\nraven install [option]\n\tinstall the \"dependencies\" of the package.sc\n\nraven install [option] <packageName>\n\tinstall the package of current version and update package.sc\n\nraven install [option] <packageName>@<version>\n\tinstall the package of specified version and update package.sc\n\n[option]:\n\t-clean: clean the C source files after complie\n\t-dev: work with \"devDependencies\" instead of \"dependencies\"\n\t-g: install package as a CLI tool. need root permissions.\n\n")
    ("uninstall-h"
      . "\nUsage:\n\nraven install [option] <packageName>\n\tremove the package and update package.sc\n\n[option]:\n\t-dev: work with \"devDependencies\" instead of \"dependencies\"\n\t-g: remove a CLI tool. need root permissions.\n\n")
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
    [("--clean") (printf "todo\n")]
    [else (raven-printf-help "raven-h")]
  )
)

(define (raven)
  ;; raven 启动方法
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
