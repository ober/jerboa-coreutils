#!chezscheme
;;; hostname.sls -- Print or set the system hostname

(library (jerboa-coreutils hostname)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name)
          (jerboa core)
          (only (std sugar) with-catch)
          (jerboa-coreutils common)
          (jerboa-coreutils common version)
          (jerboa-coreutils common security))

  ;; Read hostname from /etc/hostname or shell out to hostname command
  (def (get-hostname)
    (with-catch
      (lambda (e)
        ;; Fallback: try HOSTNAME env var
        (let ((h (getenv "HOSTNAME" #f)))
          (or h "localhost")))
      (lambda ()
        (let ((p (process "/usr/bin/hostname")))
          (let ((result (get-line (car p))))
            (close-input-port (car p))
            (close-output-port (cadr p))
            (if (and (string? result) (not (eof-object? result)))
              result
              (let ((h (getenv "HOSTNAME" #f)))
                (or h "localhost"))))))))

  (def (main . args)
    (parameterize ((program-name "hostname"))
      (init-security!)
      (install-proc-only-landlock!)
      (install-readonly-seccomp!)
      (if (null? args)
        (displayln (get-hostname))
        (cond
          ((member (car args) '("--help" "-h"))
           (displayln "Usage: hostname [NAME]")
           (displayln "Print or set the system hostname.")
           (displayln "")
           (displayln "  --help     display this help and exit")
           (displayln "  --version  output version information and exit"))
          ((member (car args) '("--version"))
           (version-info "hostname"))
          (else
           (die "cannot set hostname: not supported"))))))

  ) ;; end library
