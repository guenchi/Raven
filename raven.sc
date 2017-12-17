":"; export CHEZSCHEMELIBDIRS=.:./lib && exec scheme --script $0 "$@";

;;; JSON Function Begin

(define (json-string->scm str)
  ;;; (json parser) --- Guile JSON implementation.

  ;; Copyright (C) 2013 Aleix Conchillo Flaque <aconchillo@gmail.com>
  ;;
  ;; This file is part of guile-json.
  ;;
  ;; guile-json is free software; you can redistribute it and/or
  ;; modify it under the terms of the GNU Lesser General Public
  ;; License as published by the Free Software Foundation; either
  ;; version 3 of the License, or (at your option) any later version.
  ;;
  ;; guile-json is distributed in the hope that it will be useful,
  ;; but WITHOUT ANY WARRANTY; without even the implied warranty of
  ;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  ;; Lesser General Public License for more details.
  ;;
  ;; You should have received a copy of the GNU Lesser General Public
  ;; License along with guile-json; if not, write to the Free Software
  ;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
  ;; 02110-1301 USA

  ;;; Commentary:

  ;; JSON module for Guile

  ;;; Code:

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

   (define json->scm
    (case-lambda
      [() (json->scm (current-input-port))]
      [(port) (json-read (make-json-parser port))]))

  (define-structure (json-parser port))

  (json->scm (open-input-string str))
  ;;; (json parser) ends here
)

(define (scm->json-string scm)
  ;;; (json builder) --- Guile JSON implementation.

  ;; Copyright (C) 2013 Aleix Conchillo Flaque <aconchillo@gmail.com>
  ;; Copyright (C) 2015,2016 Jan Nieuwenhuizen <janneke@gnu.org>
  ;;
  ;; This file is part of guile-json.
  ;;
  ;; guile-json is free software; you can redistribute it and/or
  ;; modify it under the terms of the GNU Lesser General Public
  ;; License as published by the Free Software Foundation; either
  ;; version 3 of the License, or (at your option) any later version.
  ;;
  ;; guile-json is distributed in the hope that it will be useful,
  ;; but WITHOUT ANY WARRANTY; without even the implied warranty of
  ;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  ;; Lesser General Public License for more details.
  ;;
  ;; You should have received a copy of the GNU Lesser General Public
  ;; License along with guile-json; if not, write to the Free Software
  ;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
  ;; 02110-1301 USA

  ;;; Commentary:

  ;; JSON module for Guile

  ;;; Code:

  ;;
  ;; String builder helpers
  ;;

  (define (unicode->string unicode)
    (format #f "\\u~4,'0x" unicode))

  (define (char->unicode-string c)
    (let ((unicode (char->integer c)))
      (if (< unicode 32)
          (unicode->string unicode)
          (string c))))

  (define (u8v-2->unicode bv)
    (let ((bv0 (bytevector-u8-ref bv 0))
          (bv1 (bytevector-u8-ref bv 1)))
      (+ (ash (logand bv0 #b00011111) 6)
        (logand bv1 #b00111111))))

  (define (u8v-3->unicode bv)
    (let ((bv0 (bytevector-u8-ref bv 0))
          (bv1 (bytevector-u8-ref bv 1))
          (bv2 (bytevector-u8-ref bv 2)))
      (+ (ash (logand bv0 #b00001111) 12)
        (ash (logand bv1 #b00111111) 6)
        (logand bv2 #b00111111))))

  (define (build-char-string c)
    (let* ((bv (string->utf8 (string c)))
          (len (bytevector-length bv)))
      (cond
      ;; A single byte UTF-8
      ((eq? len 1) (char->unicode-string c))
      ;; If we have a 2 or 3 byte UTF-8 we need to output it as \uHHHH
      ((or (eq? len 2) (eq? len 3))
        (let ((unicode (if (eq? len 2)
                          (u8v-2->unicode bv)
                          (u8v-3->unicode bv))))
          (unicode->string unicode)))
      ;; Anything else should wrong, hopefully.
      (else (error 'json-invalid "json invalid")))))

  ;;
  ;; Object builder functions
  ;;

  (define (build-object-pair p port escape pretty level)
    (display (indent-string pretty level) port)
    (json-build-string (car p) port escape)
    (display ": " port)
    (json-build (cdr p) port escape pretty level))

  (define (build-newline port pretty)
    (cond (pretty (newline port))))

  (define (indent-string pretty level)
    ;(if pretty (format #f "~v_" (* 4 level)) "")
    ; fix for chez:
    (if pretty (format #f "~vA" (* 2 level) "") ""))

  ;;
  ;; Main builder functions
  ;;

  (define (json-build-null port)
    (display "null" port))

  (define (json-build-boolean scm port)
    (display (if scm "true" "false") port))

  (define (json-build-number scm port)
    (if (and (rational? scm) (not (integer? scm)))
        (display (number->string (exact->inexact scm)) port)
        (display (number->string scm) port)))

  (define (->string x)
    (cond ((char? x) (make-string 1 x))
          ((number? x) (number->string x))
          ((symbol? x) (symbol->string x))
          (else x)))

  ;(define (atom? x)
  ;  (or (char? x) (number? x) (string? x) (symbol? x)))

  (define (json-alist? x)
    (and (pair? x)
        (let loop ((x x))
          (or (null? x)
              (null? (car x))
              (and (pair? (car x)) (atom? (caar x))
                    (loop (cdr x)))))))

  (define (json-build-string scm port escape)
    (display "\"" port)
    (display
    (list->string
      (fold-right append '()
                  (map
                  (lambda (c)
                    (case c
                      ((#\" #\\) `(#\\ ,c))
                      ((#\backspace) '(#\\ #\b))
                      ((#\page) '(#\\ #\f))
                      ((#\newline) '(#\\ #\n))
                      ((#\return) '(#\\ #\r))
                      ((#\tab) '(#\\ #\t))
                      ((#\/) (if escape `(#\\ ,c) (list c)))
                      (else (string->list (build-char-string c)))))
                  (string->list (->string scm)))))
    port)
    (display "\"" port))

  (define (json-build-array scm port escape pretty level)
    (display "[" port)
    (unless (null? scm)
      (json-build (car scm) port escape pretty (+ level 1))
      (for-each (lambda (v)
                  (display ", " port)
                  (json-build v port escape pretty (+ level 1)))
                (cdr scm)))
    (display "]" port))

  (define (json-build-object scm port escape pretty level)
    ;(build-newline port pretty)
    (format port "~A{" (indent-string #f level))
    (build-newline port pretty)
    (let ((pairs scm))
      (unless (null? pairs)
        (build-object-pair (car pairs) port escape pretty (+ level 1))
        (for-each (lambda (p)
                    (display "," port)
                    (build-newline port pretty)
                    (build-object-pair p port escape pretty (+ level 1)))
                  (cdr pairs))))
    (build-newline port pretty)
    (format port "~A}" (indent-string pretty level)))

  (define (hash-table->list hash-table)
    (hash-table-map hash-table (lambda (k v)
                                (cons k v))))

  (define (hashtable->list hashtable)
    (map (lambda (k)
           (cons k (hashtable-ref hashtable k "")))
         (reverse (vector->list (hashtable-keys hashtable)))))

  (define (json-build scm port escape pretty level)
    (cond
    ((null? scm) (json-build-null port))
    ((boolean? scm) (json-build-boolean scm port))
    ((number? scm) (json-build-number scm port))
    ((symbol? scm) (json-build-string (symbol->string scm) port escape))
    ((string? scm) (json-build-string scm port escape))
    ((json-alist? scm) (json-build-object scm port escape pretty level))
    ((list? scm) (json-build-array scm port escape pretty level))
    ((hash-table? scm)
      (json-build-object (hash-table->list scm) port escape pretty level))
    ((hashtable? scm)
      (json-build-object (hashtable->list scm) port escape pretty level))
    (else (error 'json-invalid "json invalid"))))

  ;;
  ;; Public procedures
  ;;

  (define scm->json
    (case-lambda
      [(scm) (scm->json scm (current-output-port))]
      [(scm port) (scm->json scm port #f #f)]
      [(scm port escape pretty) (json-build scm port escape pretty 0)]))

  (define scm->json-string-inside
    (case-lambda
      [(scm) (scm->json-string-inside scm #f #f)]
      [(scm escape pretty) (call-with-string-output-port
                            (lambda (p)
                              (scm->json scm p escape pretty)))]))
  (scm->json-string-inside scm #t #t)
  ;;; (json builder) ends here
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
  (delete-file file-name)
  (let ([p (open-output-file file-name)] [len (string-length content)])
    (let loop ([idx 0])
      (when (< idx len)
          (write-char (string-ref content idx) p)
          (loop (add1 idx))))
    (close-output-port p)
  )
)
      
(define (package-json->scm . args)
  ;; 转化package.json文件为hashtable
  (define file (if (null? args) raven-json-path (car args)))
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
  },
  \"devDependencies\": {
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
  (write-file raven-json-path (make-package-json name version description author private))
)

(define (load-lib lib ver . args)
  ;; 下载单个包
  (define file (string-append lib ".tar.gz"))
  (define lib-path (if (null? args) raven-library-path (car args)))
  (define printf? (if (> (length args) 1) (not (not (cadr args))) #t))
  (define dep-key "dependencies")
  (unless ver
    (set! ver (cmd-arg->version lib ver)))
  (unless (file-directory? lib-path)
    (mkdir lib-path))
  (when printf?
    (printf (string-append "loading " lib " " ver " ......\n")))
  (if (and (zero? (system (string-append "cd " lib-path " && curl -s -o " file " " raven-url lib "/" ver ".tar.gz"
            " && tar -xzf " file)))
           (delete-file (string-append lib-path "/" file)))
      (begin
        (when (file-exists? (string-append lib-path "/" lib "/" raven-json-file))
          (let* ([hs (package-json->scm (string-append lib-path "/" lib "/" raven-json-path))]
                 [libs-hs (hashtable-ref hs dep-key #f)]
                 [libs (hashtable-keys libs-hs)])
              (vector-map (lambda (lib) (load-lib lib (hashtable-ref libs-hs lib #f) (string-append lib-path "/" lib "/" raven-library-dir) #f)) libs))
        )
        (when printf?
          (printf (string-append "load " lib " " ver " success\n")))
        #t
      )
      (begin
        (when printf?
          (printf (string-append "load " lib " " ver " fail\n")))
        #f
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
            (delete-file p2)
          )))
      (directory-list path))
    (delete-directory path)
  )
)

(define (system-return cmd)
  ;; 读取命令行返回内容
  (define tmp "./##tmp##")
  (define rst "")
  (and (zero? (system (string-append cmd " > " tmp)))
    (file-exists? tmp)
    (begin (set! rst (read-file tmp)) (delete-file tmp)))
  rst
)

(define (cmd-arg->version lib ver)
  ;; 获取库版本号
  (if (and ver (not (null? ver)) (char=? (string-ref (car ver) 0) #\@))
    (substring (car ver) 1 (string-length (car ver)))
    (system-return (string-append "curl -s " raven-url lib))
  )
)

(define (ask-Y/n? tip)
  ;; 请求输入Y/n
  (printf (string-append tip "(Y/n)"))
  (not (string-ci=? (read-line) "n"))
)


;;; Helper End

;;; Command Begin

(define (init opt args)
  ;; 初始化项目
  (delete-file raven-json-path)
  (create-json)
  (clear-directory raven-library-path)
  (mkdir raven-library-path)
  (let ([libs (hashtable-ref (package-json->scm) "dependencies" (make-eq-hashtable))])
    (vector-map (lambda (k) (load-lib k (hashtable-ref libs k #f))) (hashtable-keys libs)))
  (printf "raven init over\n")
)

(define (install opt args)
  ;; 安装包
  (define key "dependencies")
  (unless (file-exists? raven-json-path)
    (write-file raven-json-path (make-package-json "" "" "" "" "false")))
  (unless (file-directory? raven-library-path)
    (mkdir raven-library-path))
  (if (null? args)
      (when (ask-Y/n? "install all libraries?")
        (let* ([hs (package-json->scm)]
               [libs-hs (hashtable-ref hs key #f)]
               [libs (hashtable-keys libs-hs)])
          (vector-map (lambda (lib) (load-lib lib (hashtable-ref libs-hs lib #f))) libs))
        (printf "install all libraries over\n")
      )
      (let* ([lib (car args)]
             [ver (cmd-arg->version lib (cdr args))]
             [rst (load-lib lib ver)]
             [hs (package-json->scm)])
        (when rst
          (unless (hashtable-contains? hs key)
            (hashtable-set! hs key (make-hashtable string-hash string=?))
          )
          (hashtable-set! (hashtable-ref hs key #f) lib ver)
          (write-file raven-json-path (scm->json-string hs))
        )
        (printf "raven install over\n")
      )
  )
)

(define (uninstall opt libs)
  ;; 卸载包
  (define key "dependencies")
  (if (null? libs)
    (when (ask-Y/n? "uninstall all libraries?")
      (clear-directory raven-library-path)
      (mkdir raven-library-path)
      (let ([hs (package-json->scm)])
          (hashtable-set! hs key (make-hashtable string-hash string=?))
          (write-file raven-json-path (scm->json-string hs)))
      (printf "uninstall all libraries over\n")
    )
    (if (and (file-directory? raven-library-path) (file-exists? raven-json-path))
      (let ([hs (package-json->scm)])
        (map 
          (lambda (lib)
            (clear-directory (string-append "./lib/" lib))
            (hashtable-delete! (hashtable-ref hs "dependencies" (make-eq-hashtable)) lib) 
            (printf "unstall ~a success\n" lib)
          ) 
          libs)
          ;(show-hashtable hs)
          ;(display (scm->json-string hs))
          (write-file raven-json-path (scm->json-string hs))
          (printf "raven uninstall over\n")
      
      )
      (printf "please raven init first\n")
    )
  )
)

(define (self-command . args)
  ;; 自定义命令
  (if (file-exists? raven-json-path)
    (let ([hs (package-json->scm)])
      (if (and (hashtable-contains? hs "scripts") (hashtable-contains? (hashtable-ref hs "scripts" #f) (car args)))
        (system 
          (string-append 
            (hashtable-ref (hashtable-ref hs "scripts" #f) (car args) #f) 
            " "
            (apply string-append (cdr args))))
        (printf "invaild command\n")
      )
    )
    (printf "please run raven init first\n")
  )
)

(define (update)
  ;; 更新Raven
  ;; TODO
  #f
)

;;; Command End

;;; Info Begin

(define raven-version "0.0.9")

(define raven-library-dir "lib")

(define raven-library-path (string-append "./" raven-library-dir))

(define raven-json-file "package.json")

(define raven-json-path (string-append "./" raven-json-file))

(define raven-url "http://ravensc.com/")

;;; Info End

;;; Main Begin

(define (raven-init)
  ;; 初始化环境
  ;; TODO
  #f
)

(define (check-version)
  ;; TODO: 运行前检查版本
  (define newest-version (system-content (string-append "curl -s " raven-url "VERSION")))
  (if (string-ci? lastest-version raven-version)
    (printf "the newest-version is ~a\n" newest-version)
    (when (ask-Y/n? "would you want to update now?")
      (system "")
      (printf "update over\n")
    )
  )
)

(define (raven)
  ;; raven 启动方法
  (define args (command-line-arguments))
  ;;(raven-init)
  ;;(check-version)
  (if (null? args)
      (printf "need command init/install/uninstall \n")
      (let-values 
        ([(opts cmds) (partition opt-string? args)])
        (case (car cmds)
          [("init") (init opts (cdr cmds))]
          [("install") (install opts (cdr cmds))]
          [("uninstall") (uninstall opts (cdr cmds))]
          [("update") (update)]
          [else (apply self-command args)]
        )
      )
  )
)

;;; Main End

;; start
(raven)