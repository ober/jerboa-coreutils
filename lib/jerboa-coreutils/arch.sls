#!chezscheme
;;; arch.sls -- Print architecture

(library (jerboa-coreutils arch)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name)
          (jerboa core)
          (only (std sugar) with-catch)
          (jerboa-coreutils common)
          (jerboa-coreutils common version))

  (def (arch-machine)
    (with-catch
      (lambda (e) "unknown")
      (lambda ()
        (let-values (((to-stdin from-stdout from-stderr pid)
                      (open-process-ports
                        "/usr/bin/uname -m"
                        (buffer-mode block)
                        (native-transcoder))))
          (let ((result (get-line from-stdout)))
            (close-port to-stdin)
            (close-port from-stdout)
            (close-port from-stderr)
            (if (eof-object? result) "unknown" result))))))

  (def (main . args)
    (parameterize ((program-name "arch"))
      (cond
        ((and (pair? args) (member (car args) '("--help" "-h")))
         (displayln "Usage: arch")
         (displayln "Print machine hardware name (same as uname -m)."))
        ((and (pair? args) (member (car args) '("--version")))
         (version-info "arch"))
        (else
         (displayln (arch-machine))))))

  ) ;; end library
