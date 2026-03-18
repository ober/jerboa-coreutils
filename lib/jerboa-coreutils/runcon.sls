#!chezscheme
;;; runcon.sls -- Run command with specified SELinux security context (stub)

(library (jerboa-coreutils runcon)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name)
          (jerboa core)
          (only (std format) eprintf)
          (jerboa-coreutils common))

  (def (main . args)
    (parameterize ((program-name "runcon"))
      (eprintf "runcon: SELinux is not supported in this implementation\n")
      (exit 1)))

  ) ;; end library
