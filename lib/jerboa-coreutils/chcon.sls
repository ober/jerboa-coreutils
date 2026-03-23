#!chezscheme
;;; chcon.sls -- Change file SELinux security context (stub)

(library (jerboa-coreutils chcon)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name)
          (jerboa core)
          (only (std format) eprintf)
          (jerboa-coreutils common)
          (jerboa-coreutils common security))

  (def (main . args)
    (parameterize ((program-name "chcon"))
      (init-security!)
      (eprintf "chcon: SELinux is not supported in this implementation\n")
      (exit 1)))

  ) ;; end library
