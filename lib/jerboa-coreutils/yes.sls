#!chezscheme
;;; yes.sls -- Repeatedly output a line

(library (jerboa-coreutils yes)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name)
          (jerboa core)
          (only (std sugar) with-catch)
          (only (std misc string) string-join)
          (jerboa-coreutils common)
          (jerboa-coreutils common version)
          (std cli getopt))

  (def (main . args)
    (parameterize ((program-name "yes"))
      (when (and (pair? args) (member (car args) '("--help" "--version")))
        (cond
          ((equal? (car args) "--help")
           (displayln "Usage: yes [STRING]...")
           (displayln "Repeatedly output a line with all specified STRING(s), or 'y'.")
           (exit 0))
          ((equal? (car args) "--version")
           (version-info "yes")
           (exit 0))))
      (let* ((line (if (null? args) "y" (string-join args " ")))
             ;; Build a buffer of repeated lines for performance
             (buf (let ((out (open-output-string)))
                    (let loop ((i 0))
                      (when (< i 512)
                        (display line out)
                        (newline out)
                        (loop (+ i 1))))
                    (get-output-string out))))
        (with-catch
          (lambda (e) (void)) ;; exit silently on broken pipe
          (lambda ()
            (let loop ()
              (display buf)
              (flush-output-port (current-output-port))
              (loop)))))))

  ) ;; end library
