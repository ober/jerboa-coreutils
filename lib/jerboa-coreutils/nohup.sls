#!chezscheme
;;; nohup.sls -- Run immune to hangups

(library (jerboa-coreutils nohup)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name)
          (jerboa core)
          (only (std sugar) with-catch)
          (only (std format) eprintf)
          (jerboa-coreutils common)
          (jerboa-coreutils common version))

  (define _load-ffi (begin (load-shared-object #f) (void)))

  ;; Ignore SIGHUP by setting its handler to SIG_IGN (1)
  ;; signal(SIGHUP, SIG_IGN) - SIGHUP=1
  (define ffi-signal (foreign-procedure "signal" (int void*) void*))

  (def (ignore-sighup)
    ;; SIG_IGN is typically 1 on Linux
    (ffi-signal 1 1))

  (def (main . args)
    (parameterize ((program-name "nohup"))
      (cond
        ((null? args)
         (die "missing operand"))
        ((member (car args) '("--help"))
         (displayln "Usage: nohup COMMAND [ARG]...")
         (displayln "Run COMMAND, ignoring hangup signals."))
        ((member (car args) '("--version"))
         (version-info "nohup"))
        (else
         (ignore-sighup)
         ;; If stdout is a terminal, redirect to nohup.out
         (let* ((outfile (or (with-catch (lambda (e) #f)
                               (lambda ()
                                 (let ((f (open-output-file "nohup.out")))
                                   (close-port f) "nohup.out")))
                             (let ((home (getenv "HOME" "")))
                               (string-append home "/nohup.out")))))
           (eprintf "nohup: appending output to '~a'\n" outfile)
           (let* ((cmd (car args))
                  (cmd-args (cdr args))
                  (cmd-str (string-join (cons cmd cmd-args) " "))
                  (full-cmd (string-append cmd-str " >> "
                              (shell-quote outfile) " 2>&1"))
                  (status (system full-cmd)))
             (exit status)))))))


  (def (string-join strs sep)
    (if (null? strs) ""
      (let loop ((rest (cdr strs)) (acc (car strs)))
        (if (null? rest) acc
          (loop (cdr rest) (string-append acc sep (car rest)))))))

  ) ;; end library
