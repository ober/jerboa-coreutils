#!chezscheme
;;; nice.sls -- Run with different priority

(library (jerboa-coreutils nice)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name)
          (jerboa core)
          (only (std sugar) with-catch)
          (only (std format) eprintf)
          (jerboa-coreutils common)
          (jerboa-coreutils common version)
          (jerboa-coreutils common security))

  (define _load-ffi (begin (load-shared-object #f) (void)))

  (define ffi-nice (foreign-procedure "nice" (int) int))

  (def (main . args)
    (parameterize ((program-name "nice"))
      (init-security!)
      (install-process-seccomp!)
      (let loop ((args args) (adjustment 10))
        (cond
          ((null? args)
           ;; No command: print current niceness
           (displayln (ffi-nice 0)))
          ((member (car args) '("--help" "-h"))
           (displayln "Usage: nice [OPTION] [COMMAND [ARG]...]")
           (displayln "Run COMMAND with an adjusted niceness."))
          ((member (car args) '("--version"))
           (version-info "nice"))
          ((and (>= (string-length (car args)) 2)
                (string=? (substring (car args) 0 2) "-n"))
           (let ((adj (if (> (string-length (car args)) 2)
                        (substring (car args) 2 (string-length (car args)))
                        (if (pair? (cdr args)) (cadr args)
                          (die "option requires an argument -- 'n'")))))
             (let ((n (string->number adj)))
               (unless n (die "invalid adjustment '~a'" adj))
               (loop (if (> (string-length (car args)) 2) (cdr args) (cddr args)) n))))
          (else
           ;; Run command with adjusted niceness
           (ffi-nice adjustment)
           (let* ((cmd (car args))
                  (cmd-args (cdr args))
                  (cmd-str (string-join (cons cmd cmd-args) " "))
                  (status (system cmd-str)))
             (exit status)))))))

  (def (string-join strs sep)
    (if (null? strs) ""
      (let loop ((rest (cdr strs)) (acc (car strs)))
        (if (null? rest) acc
          (loop (cdr rest) (string-append acc sep (car rest)))))))

  ) ;; end library
