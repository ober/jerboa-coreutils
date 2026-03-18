#!chezscheme
;;; true.sls -- Exit with a status code indicating success

(library (jerboa-coreutils true)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name)
          (jerboa core)
          (jerboa-coreutils common)
          (jerboa-coreutils common version)
          (std cli getopt))

  (def (main . args)
    (parameterize ((program-name "true"))
      (when (and (pair? args) (member (car args) '("--help" "--version")))
        (cond
          ((equal? (car args) "--help")
           (displayln "Usage: true")
           (displayln "Exit with a status code indicating success."))
          ((equal? (car args) "--version")
           (version-info "true"))))
      (exit 0)))

  ) ;; end library
