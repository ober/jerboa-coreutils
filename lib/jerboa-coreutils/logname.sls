#!chezscheme
;;; logname.sls -- Print current login name

(library (jerboa-coreutils logname)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name)
          (jerboa core)
          (std cli getopt)
          (jerboa-coreutils common)
          (jerboa-coreutils common version))

  (def (main . args)
    (parameterize ((program-name "logname"))
      (call-with-getopt
        (lambda (_ opt)
          (let ((name (getenv "LOGNAME" #f)))
            (if name
              (displayln name)
              (die "no login name"))))
        args
        'program: "logname"
        'help: "Print the name of the current user.")))

  ) ;; end library
