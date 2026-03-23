#!chezscheme
;;; false.sls -- Exit with a status code indicating failure

(library (jerboa-coreutils false)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name)
          (jerboa core)
          (jerboa-coreutils common)
          (jerboa-coreutils common version)
          (jerboa-coreutils common security)
          (std cli getopt))

  (def (main . args)
    (init-security!)
    (parameterize ((program-name "false"))
      (when (and (pair? args) (member (car args) '("--help" "--version")))
        (cond
          ((equal? (car args) "--help")
           (displayln "Usage: false")
           (displayln "Exit with a status code indicating failure."))
          ((equal? (car args) "--version")
           (version-info "false"))))
      (exit 1)))

  ) ;; end library
