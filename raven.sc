":"; export CHEZSCHEMELIBDIRS=.:./lib:/usr/local/lib/raven && exec scheme --script $0 "$@";

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

(define asl-push!
  ;;增加键值对
  (lambda (asl x y)
    (if (null? (cdr asl))
      (if (null? (car asl))
        (set-car! asl (cons x y))
        (set-cdr! asl (cons (cons x y) '())))
      (push (cdr asl) x y))))

(define asl-pop!
  ;;出栈
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

(define asl-out!
  ;;出队
  (lambda (str)
    (let ((str str)(x (car str)))
      (if (null? (cdr str))
        (set-car! str '())
        (begin 
          (set-car! str (cadr str))
          (set-cdr! str (cddr str))))
      x)))				

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
    (cons "author" `((,author)))
    (cons "private" private)
    (cons "scripts" '(("dev" . "") ("build" . "")("g" . "")("start" . "")("prcommit" . "")("lint" . "")))
    (cons "dependencies" '())
    (cons "devDependencies" '())
))

(define read-line
  ;; 获取控制台输入
  (case-lambda
    ([] (read-line #f #f))
    ([prompt] (read-line prompt #f))
    ([prompt default] (begin
      (when prompt (printf prompt))
      (let loop ([c (read-char)] [lst '()])
        (if (char=? c #\newline)
          (if (null? lst)
            (or default "")
            (if (char=? (car lst) #\return)
              (apply string (reverse (cdr lst)))
              (apply string (reverse lst))))
          (loop (read-char) (cons c lst))))))))

(define (create-pkg-file)
  (define name (read-line "project name: "))
  (define version (read-line "version(0.1.0): " "0.1.0"))
  (define description (read-line "description: "))
  (define author (read-line (format "author(~a): " raven-user) raven-user))
  (define private (read-line "private?(Y/n): " "y"))
  (set! private (not (string-ci=? private "n")))
  (delete-file raven-pkg-path)
  (write-package-file raven-pkg-path (make-package-asl name version description author private))
)

(define load-lib
  (case-lambda
    ([lib ver] (load-lib lib ver raven-library-path))
    ([lib ver lib-path] (load-lib lib ver lib-path #t))
    ([lib ver lib-path printf?] 
      (begin
        (unless ver
          (set! ver (newest-version lib)))
        (unless (file-directory? lib-path)
          (mkdir lib-path))
        (clear-directory (format "~a/~a" lib-path lib))
        (when printf?
          (printf (format "loading ~a ~a ......\n" lib ver)))
        (if 
          (if raven-windows?
            (and
                (system (format "cd ~a && curl -s -o ~a.tar.gz ~a/~a/~a && 7z x ~a.tar.gz -y -aoa >> install.log && 7z x ~a.tar -o~a/~a -y -aoa >> install.log"
                          lib-path lib raven-url lib ver lib lib lib-path lib))
                (delete-file (format "~a/~a.tar.gz" lib-path lib) #t)
                (delete-file (format "~a/~a.tar" lib-path lib) #t)
                (delete-file (format "~a/install.log" lib-path) #t))
            (and
              (mkdir (format "~a/~a" lib-path lib))
              (system (format "cd ~a && curl -s -o ~a.tar.gz ~a/~a/~a && tar -xzf ~a.tar.gz -C ~a/~a"
                        lib-path lib raven-url lib ver lib lib-path lib))
              (delete-file (format "~a/~a.tar.gz" lib-path lib) #t)))
          (begin
            (when (file-exists? (format "~a/~a/~a" lib-path lib raven-pkg-file))
              (let* ([asl (package-sc->scm (format "~a/~a/~a" lib-path lib raven-pkg-path))]
                     [libs-asl (asl-ref asl raven-depend-key '())])
                (for-each 
                  (lambda (lib/ver) 
                    (load-lib (car lib/ver) (cdr ver/ver) (format "~a/~a/~a" lib-path lib raven-library-dir) #f))
                  libs-asl)))
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
  (define tmp "./##tmp##")
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
        (cons lib (newest-version lib))
        (cons lib ver)))
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
  (not (string-ci=? (read-line) "n"))
)

(define (string-index str chr)
  (define len (string-length str))
  (do ((pos 0 (+ 1 pos)))
      ((or (>= pos len) (char=? chr (string-ref str pos)))
       (and (< pos len) pos))))

;;; Helper End

;;; Command Begin

(define (init opt args)
  ;; Initial
  (create-pkg-file)
  (clear-directory raven-library-path)
  (mkdir raven-library-path)
  (let ([libs (asl-ref (package-sc->scm) raven-current-key '())])
    (for-each (lambda (l/v) (load-lib (car l/v) (cdr l/v))) libs))
  (printf "raven init over\n")
)

(define (install opt libs)
  ;; Installation
  (unless (or raven-global? (file-exists? raven-pkg-path))
    (write-package-file raven-pkg-path (make-package-asl "" "" "" raven-user #f)))
  (unless (file-directory? raven-library-path)
    (mkdir raven-library-path))
  (if (null? libs)
      (if raven-global?
        (printf "please add library name\n")
        (when (ask-Y/n? "install all libraries?")
          (let* ([asl (package-sc->scm)]
                 [libs-asl (asl-ref asl raven-current-key '())])
            (for-each (lambda (l/v) (load-lib (car l/v) (cdr l/v))) libs-asl))
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
                      (printf "~a has been downloaded in ~a\\~a\n" lib target-path lib)
                      (begin
                        (delete-file (format "/usr/local/bin/~a" lib) #t)
                        (system (format "ln -s ~a/~a/~a.sc /usr/local/bin/~a" target-path lib lib lib))
                        (system (format "chmod +x /usr/local/bin/~a" lib))
                        (printf "install ~a ~a success\n" lib ver)))))
                (printf (format "wrong library name: ~a\n" lib)))))
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
                  (printf (format "wrong library name: ~a\n" lib)))))
            libs)
          (write-package-file raven-pkg-path asl)
          (printf "raven install over\n")))
  )
)

(define (uninstall opt libs)
  ;; Uninstallation
  (if (null? libs)
    (when (ask-Y/n? "uninstall all libraries?")
      (if raven-global?
        (for-each 
          (lambda (path) 
            (printf "deleting ~a/~a ......\n" raven-library-path path)
            (delete-file/directory (format "~a/~a" raven-library-path path))
            (unless raven-windows?
              (delete-file (format "/usr/local/bin/~a" path))))
          (directory-list raven-library-path))
        (begin
          (for-each 
            (lambda (lib/ver) 
              (printf "uninstall ~a ~a ......\n" (car lib/ver) (cdr lib/ver))
              (clear-directory (format "~a/~a" raven-library-path (car lib/ver))))
            (asl-ref (package-sc->scm) raven-current-key '()))
          (let ([asl (package-sc->scm)])
            (asl-set! asl raven-current-key '())
            (write-package-file raven-pkg-path asl))))
      (printf "uninstall all libraries over\n"))
    ;; uninstall libs
    (if raven-global?
      (for-each 
        (lambda (path) 
          (printf "deleting ~a/~a ......\n" raven-library-path path)
          (delete-file/directory (format "~a/~a" raven-library-path path))
          (unless raven-windows?
            (delete-file (format "/usr/local/bin/~a" path))))
        libs)
      (if (and (file-directory? raven-library-path) (file-exists? raven-pkg-path))
        (let* ([asl (package-sc->scm)]
               [libs-asl (asl-ref asl raven-current-key '())])
          (for-each 
            (lambda (lib)
              (clear-directory (format "~a/~a" raven-library-path lib))
              (asl-delete! asl raven-current-key lib) 
              (printf "uninstall ~a success\n" lib)) 
            libs)
          (write-package-file raven-pkg-path asl)
          (printf "raven uninstall over\n"))
        (printf "please raven init first\n")
      ))
  )
)

(define (packing opts args)
  (define ver (asl-ref (package-sc->scm) "version"))
  (if raven-windows?
    (and (system 
      (format "7z a ~a.tar ./ && 7z d ~a.tar lib -r && 7z d ~a.tar .* -r  && 7z d ~a.tar .tar -r && 7z d ~a.tar .tar.gz -r && 7z a ~a.tar.gz ~a.tar"
                ver ver ver ver ver ver ver))
      (delete-file (format "./~a.tar" ver)))
    (system (format "tar -zcf ~a.tar.gz * --exclude lib --exclude \"*.tar.gz\"" ver)))
  (printf "raven library : ~a.tar.gz is ready\n" ver)
)

(define (self-command . args)
  ;; 自定义命令
  (if (file-exists? raven-pkg-path)
    (let* ([scripts (asl-ref (package-sc->scm) "scripts")]
           [cmd (if scripts (asl-ref scripts (car args)) #f)])
      (if cmd
        (system (format "~a ~a" cmd (apply string-append (cdr args))))
        (printf "invaild command\n")))
    (printf "please run raven init first\n")
  )
)

;;; Command End

;;; Info Begin

(define raven-version "0.2.2")

(define raven-library-dir "lib")

(define raven-library-path (string-append "./" raven-library-dir))

(define raven-pkg-file "package.sc")

(define raven-pkg-path (string-append "./" raven-pkg-file))

(define raven-url "http://localhost:5000.com")

(define raven-windows? 
  (case (machine-type)
    ((a6nt i3nt ta6nt ti3nt) #t)
    (else #f)))

(define raven-depend-key "dependencies")

(define raven-dev-depend-key "dependencies")

(define raven-current-key raven-depend-key)

(define raven-global-path (if raven-windows? (string-append (or (getenv "UserProfile") "C:") "\\raven") "/usr/local/lib/raven"))

(define raven-global-dir "raven")

(define raven-global? #f)

(define raven-user (if raven-windows? (or (getenv "USERNAME") "") (or (getenv "USER")"")))

;;; Info End

;;; Main Begin

(define (raven-init opts)
  ;; 初始化环境
  (when (member "-g" opts)
    (set! raven-library-dir raven-global-dir)
    (set! raven-library-path raven-global-path)
    (set! raven-global? #t))
  (when (member "-dev" opts)
    (set! raven-current-key raven-dev-depend-key))
  (when (member "-clean" opts)
    (printf "todo: init -clean\n"))
)

(define (check-version)
  ;; 运行前检查版本
  (define ver (newest-version "raven"))
  (when (and ver (not (string=? ver raven-version)))
    (printf (format "the raven newest version is ~a, you can upgrade it by run 'raven install -g raven'\n" ver))
  )
)

(define (raven)
  ;; raven 启动方法
  (define args (command-line-arguments))
  (check-version)
  (if (null? args)
      (printf "need command init/install/uninstall or scripts-command\n")
      (let-values 
        ([(opts cmds) (partition opt-string? args)])
        (raven-init opts)
        (case (car cmds)
          [("init") (init opts (cdr cmds))]
          [("install") (install opts (cdr cmds))]
          [("uninstall") (uninstall opts (cdr cmds))]
          [("packing") (packing opts (cdr cmds))]
          [else (apply self-command args)]
        )
      )
  )
)

;;; Main End

;; start
(raven)
