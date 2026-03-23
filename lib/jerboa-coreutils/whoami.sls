#!chezscheme
;;; whoami.sls -- Print the current user name

(library (jerboa-coreutils whoami)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name)
          (jerboa core)
          (std cli getopt)
          (jerboa-coreutils common)
          (jerboa-coreutils common version)
          (jerboa-coreutils common security))

  (def (main . args)
    (parameterize ((program-name "whoami"))
      (init-security!)
      (install-proc-only-landlock!)
      (install-readonly-seccomp!)
      (call-with-getopt
        (lambda (_ opt)
          (displayln (user-name)))
        args
        'program: "whoami"
        'help: "Print the user name associated with the current effective user ID.")))

  ) ;; end library
