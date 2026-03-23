#!chezscheme
;;; test.sls -- Evaluate conditional expressions

(library (jerboa-coreutils test)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name)
          (jerboa core)
          (only (std sugar) with-catch)
          (only (std format) eprintf format)
          (jerboa-coreutils common)
          (jerboa-coreutils common version)
          (jerboa-coreutils common security))

  (define _load-ffi (begin (load-shared-object #f) (void)))

  ;; FFI for access() and stat() checks
  (define ffi-access (foreign-procedure "access" (string int) int))

  ;; Access mode constants (POSIX)
  (define R_OK 4)
  (define W_OK 2)
  (define X_OK 1)

  ;; Use system stat to get file type info
  ;; Returns: field value or -1 on error
  ;; Fields: 0=exists, 1=isreg, 2=isdir, 3=issym, 4=size, 5=isblk, 6=ischr, 7=isfifo, 8=issock
  (def (ffi-test-stat path field)
    (with-catch
      (lambda (e) -1)
      (lambda ()
        (let* ((stat-flag (if (= field 3) "-L" ""))
               (format-str (cond
                            ((= field 0) "%F")
                            ((= field 1) "%F")
                            ((= field 2) "%F")
                            ((= field 3) "%F")
                            ((= field 4) "%s")
                            ((= field 5) "%F")
                            ((= field 6) "%F")
                            ((= field 7) "%F")
                            ((= field 8) "%F")
                            (else "%F")))
               (cmd (string-append "stat " stat-flag " -c '" format-str "' "
                                   (shell-quote path) " 2>/dev/null")))
          (let-values (((to-stdin from-stdout from-stderr pid)
                        (open-process-ports cmd (buffer-mode block) (native-transcoder))))
            (close-port to-stdin)
            (let ((result (get-line from-stdout)))
              (close-port from-stdout)
              (close-port from-stderr)
              (if (or (not result) (eof-object? result))
                -1
                (cond
                  ((= field 0) 1)  ;; exists if stat succeeded
                  ((= field 1) (if (string-contains-str? result "regular") 1 0))
                  ((= field 2) (if (string-contains-str? result "directory") 1 0))
                  ((= field 3) (if (string-contains-str? result "link") 1 0))
                  ((= field 4) (let ((n (string->number result))) (if n (inexact->exact n) -1)))
                  ((= field 5) (if (string-contains-str? result "block") 1 0))
                  ((= field 6) (if (string-contains-str? result "character") 1 0))
                  ((= field 7) (if (string-contains-str? result "fifo") 1 0))
                  ((= field 8) (if (string-contains-str? result "socket") 1 0))
                  (else -1)))))))))

  (def (ffi-test-access path mode)
    (if (= (ffi-access path mode) 0) 1 0))

  (def (string-contains-str? str sub)
    (let ((slen (string-length str))
          (sublen (string-length sub)))
      (if (> sublen slen) #f
        (let loop ((i 0))
          (cond
            ((> (+ i sublen) slen) #f)
            ((string=? (substring str i (+ i sublen)) sub) #t)
            (else (loop (+ i 1))))))))

  ;; Token stream
  (define *test-tokens* '#())
  (define *test-pos* 0)

  (def (test-peek)
    (if (< *test-pos* (vector-length *test-tokens*))
      (vector-ref *test-tokens* *test-pos*)
      #f))

  (def (test-next!)
    (let ((t (test-peek)))
      (set! *test-pos* (+ *test-pos* 1))
      t))

  (def (test-expect! expected)
    (let ((t (test-next!)))
      (unless (and t (string=? t expected))
        (eprintf "test: syntax error\n")
        (exit 2))))

  ;; Parse and evaluate expression
  (def (parse-test-expr)
    (parse-or-expr))

  (def (parse-or-expr)
    (let ((left (parse-and-expr)))
      (let loop ((left left))
        (let ((t (test-peek)))
          (if (and t (string=? t "-o"))
            (begin
              (test-next!)
              (let ((right (parse-and-expr)))
                (loop (or left right))))
            left)))))

  (def (parse-and-expr)
    (let ((left (parse-not-expr)))
      (let loop ((left left))
        (let ((t (test-peek)))
          (if (and t (string=? t "-a"))
            (begin
              (test-next!)
              (let ((right (parse-not-expr)))
                (loop (and left right))))
            left)))))

  (def (parse-not-expr)
    (let ((t (test-peek)))
      (if (and t (string=? t "!"))
        (begin
          (test-next!)
          (not (parse-not-expr)))
        (parse-primary))))

  (def (parse-primary)
    (let ((t (test-peek)))
      (cond
        ((not t) #f)
        ((string=? t "(")
         (test-next!)
         (let ((result (parse-test-expr)))
           (test-expect! ")")
           result))
        ;; Unary file tests
        ((string=? t "-e") (test-next!) (let ((f (test-next!))) (if f (>= (ffi-test-stat f 0) 1) #f)))
        ((string=? t "-f") (test-next!) (let ((f (test-next!))) (if f (= (ffi-test-stat f 1) 1) #f)))
        ((string=? t "-d") (test-next!) (let ((f (test-next!))) (if f (= (ffi-test-stat f 2) 1) #f)))
        ((string=? t "-L") (test-next!) (let ((f (test-next!))) (if f (= (ffi-test-stat f 3) 1) #f)))
        ((string=? t "-h") (test-next!) (let ((f (test-next!))) (if f (= (ffi-test-stat f 3) 1) #f)))
        ((string=? t "-s") (test-next!) (let ((f (test-next!))) (if f (> (ffi-test-stat f 4) 0) #f)))
        ((string=? t "-b") (test-next!) (let ((f (test-next!))) (if f (= (ffi-test-stat f 5) 1) #f)))
        ((string=? t "-c") (test-next!) (let ((f (test-next!))) (if f (= (ffi-test-stat f 6) 1) #f)))
        ((string=? t "-p") (test-next!) (let ((f (test-next!))) (if f (= (ffi-test-stat f 7) 1) #f)))
        ((string=? t "-S") (test-next!) (let ((f (test-next!))) (if f (= (ffi-test-stat f 8) 1) #f)))
        ((string=? t "-r") (test-next!) (let ((f (test-next!))) (if f (= (ffi-test-access f R_OK) 1) #f)))
        ((string=? t "-w") (test-next!) (let ((f (test-next!))) (if f (= (ffi-test-access f W_OK) 1) #f)))
        ((string=? t "-x") (test-next!) (let ((f (test-next!))) (if f (= (ffi-test-access f X_OK) 1) #f)))
        ;; Unary string tests
        ((string=? t "-z") (test-next!) (let ((s (test-next!))) (if s (string=? s "") #t)))
        ((string=? t "-n") (test-next!) (let ((s (test-next!))) (if s (not (string=? s "")) #f)))
        ;; Look ahead for binary operators
        (else
         (test-next!)
         (let ((op (test-peek)))
           (cond
             ((not op) (not (string=? t "")))
             ((string=? op "=")
              (test-next!)
              (let ((right (test-next!)))
                (string=? t (or right ""))))
             ((string=? op "!=")
              (test-next!)
              (let ((right (test-next!)))
                (not (string=? t (or right "")))))
             ((string=? op "-eq")
              (test-next!)
              (let ((right (test-next!)))
                (test-int-compare = t (or right "0"))))
             ((string=? op "-ne")
              (test-next!)
              (let ((right (test-next!)))
                (test-int-compare (lambda (a b) (not (= a b))) t (or right "0"))))
             ((string=? op "-lt")
              (test-next!)
              (let ((right (test-next!)))
                (test-int-compare < t (or right "0"))))
             ((string=? op "-le")
              (test-next!)
              (let ((right (test-next!)))
                (test-int-compare <= t (or right "0"))))
             ((string=? op "-gt")
              (test-next!)
              (let ((right (test-next!)))
                (test-int-compare > t (or right "0"))))
             ((string=? op "-ge")
              (test-next!)
              (let ((right (test-next!)))
                (test-int-compare >= t (or right "0"))))
             (else
              ;; No binary op - just check if t is non-empty
              (not (string=? t "")))))))))

  (def (test-int-compare op a-str b-str)
    (let ((a (string->number a-str))
          (b (string->number b-str)))
      (if (and a b)
        (op (inexact->exact a) (inexact->exact b))
        (begin
          (eprintf "test: integer expression expected\n")
          (exit 2)))))

  (def (main . args)
    (parameterize ((program-name "test"))
      (init-security!)
      (install-readonly-seccomp!)
      (when (and (pair? args) (string=? (car args) "--help"))
        (displayln "Usage: test EXPRESSION")
        (displayln "  or:  test")
        (displayln "  or:  [ EXPRESSION ]")
        (displayln "  or:  [ ]")
        (displayln "Exit with the status determined by EXPRESSION.")
        (exit 0))
      (when (and (pair? args) (string=? (car args) "--version"))
        (version-info "test")
        (exit 0))
      ;; Handle [ invocation - strip trailing ]
      (let ((targs (if (and (pair? args) (string=? (car args) "["))
                     (let ((rest (cdr args)))
                       ;; Should end with ]
                       (if (and (pair? rest)
                                (string=? (car (reverse rest)) "]"))
                         (reverse (cdr (reverse rest)))
                         rest))
                     args)))
        (set! *test-tokens* (list->vector targs))
        (set! *test-pos* 0)
        (if (= (vector-length *test-tokens*) 0)
          (exit 1)
          (let ((result (parse-test-expr)))
            (exit (if result 0 1)))))))

  ) ;; end library
