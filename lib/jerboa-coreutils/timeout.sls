#!chezscheme
;;; timeout.sls -- Run command with time limit

(library (jerboa-coreutils timeout)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name
            string-upcase)
          (jerboa core)
          (only (std sugar) with-catch)
          (only (std format) eprintf)
          (jerboa-coreutils common)
          (jerboa-coreutils common version))

  (def (string-join strs sep)
    (if (null? strs) ""
      (let loop ((rest (cdr strs)) (acc (car strs)))
        (if (null? rest) acc
          (loop (cdr rest) (string-append acc sep (car rest)))))))

  (define _load-ffi (begin (load-shared-object #f) (void)))
  (define ffi-kill (foreign-procedure "kill" (int int) int))

  (def (parse-duration str)
    ;; Parse duration: NUMBER[smhd] (floating point allowed)
    (let* ((len (string-length str))
           (suffix (if (> len 0)
                     (string-ref str (- len 1))
                     #\s))
           (numstr (if (and (> len 0) (char-alphabetic? suffix))
                     (substring str 0 (- len 1))
                     str))
           (n (string->number numstr)))
      (unless n
        (die "invalid time interval '~a'" str))
      (case suffix
        ((#\s) n)
        ((#\m) (* n 60))
        ((#\h) (* n 3600))
        ((#\d) (* n 86400))
        (else n))))

  (def (signal-name->number name)
    (let ((uname (string-upcase name)))
      (cond
        ((string=? uname "TERM") 15)
        ((string=? uname "KILL") 9)
        ((string=? uname "HUP") 1)
        ((string=? uname "INT") 2)
        ((string=? uname "QUIT") 3)
        ((string=? uname "USR1") 10)
        ((string=? uname "USR2") 12)
        ((string->number name) => values)
        (else (die "invalid signal '~a'" name)))))

  (def (string-upcase str)
    (let ((result (string-copy str)))
      (let loop ((i 0))
        (when (< i (string-length result))
          (string-set! result i (char-upcase (string-ref result i)))
          (loop (+ i 1))))
      result))

  (def (main . args)
    (parameterize ((program-name "timeout"))
      ;; Manual parsing since timeout has unusual syntax: timeout [OPTS] DURATION COMMAND [ARG...]
      (let loop ((args args) (signal 15) (kill-after #f) (preserve-status #f) (foreground #f))
        (cond
          ((null? args)
           (die "missing operand"))
          ((member (car args) '("--help" "-h"))
           (displayln "Usage: timeout [OPTION] DURATION COMMAND [ARG]...")
           (displayln "Start COMMAND, and kill it if still running after DURATION."))
          ((member (car args) '("--version"))
           (version-info "timeout"))
          ((string=? (car args) "--preserve-status")
           (loop (cdr args) signal kill-after #t foreground))
          ((string=? (car args) "--foreground")
           (loop (cdr args) signal kill-after preserve-status #t))
          ((and (>= (string-length (car args)) 2)
                (string=? (substring (car args) 0 2) "-s"))
           (let ((sig (if (> (string-length (car args)) 2)
                        (substring (car args) 2 (string-length (car args)))
                        (if (pair? (cdr args)) (cadr args)
                          (die "option requires an argument -- 's'")))))
             (loop (if (> (string-length (car args)) 2) (cdr args) (cddr args))
                   (signal-name->number sig) kill-after preserve-status foreground)))
          ((and (>= (string-length (car args)) 2)
                (string=? (substring (car args) 0 2) "-k"))
           (let ((dur (if (> (string-length (car args)) 2)
                        (substring (car args) 2 (string-length (car args)))
                        (if (pair? (cdr args)) (cadr args)
                          (die "option requires an argument -- 'k'")))))
             (loop (if (> (string-length (car args)) 2) (cdr args) (cddr args))
                   signal (parse-duration dur) preserve-status foreground)))
          ((null? (cdr args))
           (die "missing operand"))
          (else
           ;; First non-option is DURATION, rest is COMMAND
           (let* ((duration (parse-duration (car args)))
                  (cmd (cadr args))
                  (cmd-args (cddr args))
                  (cmd-str (string-join (cons cmd cmd-args) " "))
                  (pid #f)
                  (timed-out #f))
             ;; Use process-based approach
             (let-values (((to-stdin from-stdout from-stderr process-id)
                           (open-process-ports
                             cmd-str
                             (buffer-mode block)
                             (native-transcoder))))
               (set! pid process-id)
               ;; Start timer thread
               (let ((timer (fork-thread
                              (lambda ()
                                (thread-sleep! duration)
                                (set! timed-out #t)
                                (ffi-kill pid signal)
                                (when kill-after
                                  (thread-sleep! kill-after)
                                  (ffi-kill pid 9))))))
                 ;; Wait for process to complete
                 ;; Read and discard stdout/stderr to avoid blocking
                 (with-catch void
                   (lambda ()
                     (let loop ()
                       (let ((c (read-char from-stdout)))
                         (unless (eof-object? c)
                           (write-char c)
                           (loop))))))
                 (close-port to-stdin)
                 (close-port from-stdout)
                 (close-port from-stderr)
                 (if (and timed-out (not preserve-status))
                   (exit 124)
                   (exit 0))))))))))

  ) ;; end library
