":"; exec scheme --script "$0" "$@"


;;; JSON Function Begin

(define (json-string->scm str)
  ;; json-string->hashtable

  ;;
  ;; Parser record and read helpers
  ;;

  (define (parser-peek-char parser)
    (peek-char (json-parser-port parser)))

  (define (parser-read-char parser)
    (read-char (json-parser-port parser)))

  ;(define (parser-read-delimited parser delim handle-delim)
  ;  (let ((port (json-parser-port parser)))
  ;    (read-delimited delim port handle-delim)))

  ;;
  ;; Number parsing helpers
  ;;

  ;; Read + or -. . If something different is found, return empty string.
  (define (read-sign parser)
    (let loop ((c (parser-peek-char parser)) (s ""))
      (case c
        ((#\+ #\-)
         (let ((ch (parser-read-char parser)))
           (string-append s (string ch))))
        (else s))))

  ;; Read digits [0..9]. If something different is found, return empty
  ;; string.
  (define (read-digits parser)
    (let loop ((c (parser-peek-char parser)) (s ""))
      (case c
        ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
         (let ((ch (parser-read-char parser)))
           (loop (parser-peek-char parser)
                 (string-append s (string ch)))))
        (else s))))

  (define (string-null? str)
    (string=? str ""))

  (define (read-exp-part parser)
    (let ((c (parser-peek-char parser)) (s ""))
      (case c
        ;; Stop parsing if whitespace found.
        ((#\tab #\vtab #\newline #\return #\space) s)
        ;; We might be in an array or object, so stop here too.
        ((#\, #\] #\}) s)
        ;; We might have the exponential part
        ((#\e #\E)
         (let* ((ch (parser-read-char parser)) ; current char
                (sign (read-sign parser))
                (digits (read-digits parser)))
           ;; If we don't have sign or digits, we have an invalid
           ;; number.
           (if (not (and (string-null? sign)
                         (string-null? digits)))
               (string-append s (string ch) sign digits)
               #f)))
        ;; If we have a character different than e or E, we have an
        ;; invalid number.
        (else #f))))

  (define (read-real-part parser)
    (let ((c (parser-peek-char parser)) (s ""))
      (case c
        ;; Stop parsing if whitespace found.
        ((#\tab #\vtab #\newline #\return #\space) s)
        ;; We might be in an array or object, so stop here too.
        ((#\, #\] #\}) s)
        ;; If we read . we might have a real number
        ((#\.)
         (let* ((ch (parser-read-char parser))
                (digits (read-digits parser)))
           ;; If we have digits, try to read the exponential part,
           ;; otherwise we have an invalid number.
           (cond
            ((not (string-null? digits))
             (let ((exp (read-exp-part parser)))
               (cond
                (exp (string-append s (string ch) digits exp))
                (else #f))))
            (else #f))))
        ;; If we have a character different than . we might continue
        ;; processing.
        (else #f))))

  (define (read-number parser)
    (let loop ((c (parser-peek-char parser)) (s ""))
      (case c
        ;; Stop parsing if whitespace found.
        ((#\tab #\vtab #\newline #\return #\space) s)
        ;; We might be in an array or object, so stop here too.
        ((#\, #\] #\}) s)
        ((#\-)
         (let ((ch (parser-read-char parser)))
           (loop (parser-peek-char parser)
                 (string-append s (string ch)))))
        ((#\0)
         (let* ((ch (parser-read-char parser))
                (tail (or (read-real-part parser)
                          (error 'json-invalid "json invalid" parser))))
           (string-append s
                          (string ch)
                          tail)))
        ((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
         (let* ((ch (parser-read-char parser))
                (digit (read-digits parser))
                (tail  (or (read-real-part parser)
                           (read-exp-part parser)
                           (error 'json-invalid "json invalid" parser))))
           (string-append s
                          (string ch)
                          digit
                          tail)))
        (else (error 'json-invalid "json invalid" parser)))))

  ;;
  ;; Object parsing helpers
  ;;

  (define (read-pair parser)
    ;; Read string key
    (let ((key (json-read-string parser)))
      (let loop ((c (parser-peek-char parser)))
        (case c
          ;; Skip whitespaces
          ((#\tab #\vtab #\newline #\return #\space)
           (parser-read-char parser)
           (loop (parser-peek-char parser)))
          ;; Skip colon and read value
          ((#\:)
           (parser-read-char parser)
           (cons key (json-read parser)))
          ;; invalid object
          (else (error 'json-invalid "json invalid" parser))))))

  (define (read-object parser)
    (let loop ((c (parser-peek-char parser))
               (pairs (make-hashtable equal-hash equal?)))
      (case c
        ;; Skip whitespaces
        ((#\tab #\vtab #\newline #\return #\space)
         (parser-read-char parser)
         (loop (parser-peek-char parser) pairs))
        ;; end of object
        ((#\})
         (parser-read-char parser)
         pairs)
        ;; Read one pair and continue
        ((#\")
         (let ((pair (read-pair parser)))
           (hashtable-set! pairs (car pair) (cdr pair))
           (loop (parser-peek-char parser) pairs)))
        ;; Skip comma and read more pairs
        ((#\,)
         (parser-read-char parser)
         (loop (parser-peek-char parser) pairs))
        ;; invalid object
        (else (error 'json-invalid "json invalid" parser)))))

  ;;
  ;; Array parsing helpers
  ;;

  (define (read-array parser)
    (let loop ((c (parser-peek-char parser)) (values '()))
      (case c
        ;; Skip whitespace and comma
        ((#\tab #\vtab #\newline #\return #\space #\,)
         (parser-read-char parser)
         (loop (parser-peek-char parser) values))
        ;; end of array
        ((#\])
         (parser-read-char parser)
         values)
        ;; this can be any json object
        (else
         (let ((value (json-read parser)))
           (loop (parser-peek-char parser)
                 (append values (list value))))))))

  ;;
  ;; String parsing helpers
  ;;

  (define (expect parser expected)
    (let ((ch (parser-read-char parser)))
      (if (not (char=? ch expected))
          (error 'json-invalid "json invalid" parser)
          ch)))

  (define (expect-string parser expected)
    (for-all (lambda (ch) (expect parser ch))
             (string->list expected)))

  (define (read-hex-digit parser)
    (let ((c (parser-read-char parser)))
      (case c
        ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
          #\A #\B #\C #\D #\E #\F #\a #\b #\c #\d #\e #\f) c)
        (else (error 'json-invalid "json invalid" parser)))))

  (define (read-control-char parser)
    (let ((c (parser-read-char parser)))
      (case c
        ((#\" #\\ #\/) (string c))
        ((#\b) (string #\backspace))
        ((#\f) (string #\page))
        ((#\n) (string #\newline))
        ((#\r) (string #\return))
        ((#\t) (string #\tab))
        ((#\u)
         (let* ((utf1 (format "~a~a"
                              (read-hex-digit parser)
                              (read-hex-digit parser)))
                (utf2 (format "~a~a"
                              (read-hex-digit parser)
                              (read-hex-digit parser)))
                (vu8 (list (string->number utf1 16)
                           (string->number utf2 16)))
                (utf (u8-list->bytevector vu8)))
           (utf16->string utf 'big)))
        (else #f))))

  (define (read-until-quote-or-slash parser)
    (let ([port (json-parser-port parser)])
      (let loop ([buf '()] [c (read-char port)])
        (case c
          [(#\" #\\ #!eof) (cons (list->string (reverse! buf)) c)]
          (else (loop (cons c buf) (read-char port)))))))

  (define (read-string parser)
    ;; Read characters until \ or " are found.
    (let loop ((result "")
               ;(current (parser-read-delimited parser "\\\"" 'split))
               (current (read-until-quote-or-slash parser)))
      (case (cdr current)
        ((#\")
         (string-append result (car current)))
        ((#\\)
         (let ((ch (read-control-char parser)))
           (if ch
               (loop (string-append result (car current) ch)
                     ;(parser-read-delimited parser "\\\"" 'split)
                     (read-until-quote-or-slash parser))
               (error 'json-invalid "json invalid" parser))))
        (else
         (error 'json-invalid "json invalid" parser)))))

  ;;
  ;; Main parser functions
  ;;

  (define-syntax json-read-delimited
    (syntax-rules ()
      ((json-read-delimited parser delim read-func)
       (let loop ((c (parser-read-char parser)))
         (case c
           ;; skip whitespace
           ((#\tab #\vtab #\newline #\return #\space) (loop (parser-peek-char parser)))
           ;; read contents
           ((delim) (read-func parser))
           (else (error 'json-invalid "json invalid" parser)))))))

  (define (json-read-true parser)
    (expect-string parser "true")
    #t)

  (define (json-read-false parser)
    (expect-string parser "false")
    #f)

  (define (json-read-null parser)
    (expect-string parser "null")
    '())

  (define (json-read-object parser)
    (json-read-delimited parser #\{ read-object))

  (define (json-read-array parser)
    (json-read-delimited parser #\[ read-array))

  (define (json-read-string parser)
    (json-read-delimited parser #\" read-string))

  (define (json-read-number parser)
    (string->number (read-number parser)))

  (define (json-read parser)
    (let loop ((c (parser-peek-char parser)))
      (cond
       ;;If we reach the end we might have an incomplete document
       ((eof-object? c) (error 'json-invalid "json invalid" parser))
       (else
        (case c
          ;; skip whitespaces
          ((#\tab #\vtab #\newline #\return #\space)
           (parser-read-char parser)
           (loop (parser-peek-char parser)))
          ;; read json values
          ((#\t) (json-read-true parser))
          ((#\f) (json-read-false parser))
          ((#\n) (json-read-null parser))
          ((#\{) (json-read-object parser))
          ((#\[) (json-read-array parser))
          ((#\") (json-read-string parser))
          ;; anything else should be a number
          (else (json-read-number parser)))))))

  ;;
  ;; Public procedures
  ;;

  ;(define* (json->scm #:optional (port (current-input-port)))
  ;  "Parse a JSON document into native. Takes one optional argument,
  ;   @var{port}, which defaults to the current input port from where the JSON
  ;   document is read."
  ;  (json-read (make-json-parser port)))

  (define json->scm
    (case-lambda
      [() (json->scm (current-input-port))]
      [(port) (json-read (make-json-parser port))]))

  (define-structure (json-parser port))

  (json->scm (open-input-string str))
)

;;; JSON Function End

;;; Helper Begin

(define (show-hashtable hash)
  ;; 递归显示hashtable
  (vector-map 
      (lambda (k)
          (let ([value (hashtable-ref hash k "")])
              (if (hashtable? value)
                  (show-hashtable value)
                  (printf "~a => ~a\n" k (hashtable-ref hash k ""))
              )
          )
      ) 
      (hashtable-keys hash))
)

(define (list->hash-table lst)
  ;; 关联表转哈希表
  (let loop ((table (make-hash-table))
            (lst lst))
    (if (not (null? lst))
        (let* ([pair (car list)]
               [key (car pair)]
               [value (cdr pair)])
          (hashtable-set! table key value)
          (loop table (cdr list)))
        table)))

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
  (let ([p (open-output-file file-name)] [len (string-length content)])
      (let loop ([idx 0])
          (when (< idx len)
              (write-char (string-ref content idx) p)
              (loop (add1 idx))))
      (close-output-port p)
  )
)
      
(define (check-json)
  ;; 判断是否存在package.json文件
  (define file "./package.json")
  (unless (file-exists? file)
    (create-json))
  (let ([json (read-file file)])
    ;(show-hashtable (json-string->scm json))
    (json-string->scm json)
  )
)

(define (make-package-json name version description author private)
  ;; 默认json内容
(string-append 
"{
  \"name\": \"" name "\",
  \"version\": \"" version "\",
  \"description\": \"" description "\",
  \"author\": \"" author "\",
  \"private\": " private ",
  \"scripts\": {
    \"dev\": \"\",
    \"build\": \"\",
    \"g\": \"\",
    \"start\": \"\",
    \"precommit\": \"\",
    \"lint\": \"\"
  },
  \"dependencies\": {
    \"LIBNAME\": \"^0.15.3\"
  },
  \"devDependencies\": {
    \"LIBNAME\": \"^7.1.1\"
  }
}"
))

(define (read-line)
  ;; 获取控制台输入
  (let loop ([c (read-char)] [lst '()])
    (if (char=? c #\newline)
      (apply string (reverse lst))
      (loop (read-char) (cons c lst)))
  )
)

(define (create-json)
  ;; 新建json
  (define name "")
  (define version "1.0.0")
  (define description "")
  (define author "")
  (define private "")
  (printf "project name: ")
  (set! name (read-line))
  (printf "version(1.0.0): ")
  (let ([input (read-line)])
    (unless (string=? input "")
      (set! version input)
    )
  )
  (printf "description: ")
  (set! description (read-line))
  (printf "author: ")
  (set! author (read-line))
  (printf "private?(Y/n): ")
  (let ([input (read-line)])
    (if (string-ci=? input "n")
      (set! private "false")
      (set! private "true")
    )
  )
  (write-file "./package.json" (make-package-json name version description author private))
)

(define (load-lib lib ver)
  (unless (file-directory? "./lib") 
    (mkdir "./lib")
  )
  ;;(system cmd)
  (printf "todo: load ~a ~a\n" lib ver)
)
;;; Helper End

;;; Command Begin

(define (init)
  (define libs (hashtable-ref (check-json) "dependencies" (make-eq-hashtable)))
  (vector-map (lambda (k) (load-lib k (hashtable-ref libs k "^1.0.0"))) (hashtable-keys libs))
  (printf "raven init over\n")
)

(define (install . libs)
  (if (null? libs)
      (printf "please add lib name")
      (map load-lib libs)
  )
)

(define (uninstall . libs)
  (if (null? libs)
    (printf "please add lib name")
    (map 
      (lambda (lib)
        (printf "todo\n")
      ) 
      libs)
  )
)

;;; Command End

(define (raven)
  ;; raven 启动方法
  (define args (command-line-arguments))
  (if (null? args)
      (printf "need command init/install/uninstall \n")
      (case (car args)
        [("init") (init)]
        [("install") (install (cdr args))]
        [("uninstall") (install (cdr args))]
        [else (display "invalid command")]
      )
  )
)

;; start
(raven)