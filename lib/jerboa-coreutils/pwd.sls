#!chezscheme
;;; pwd.sls -- Print the current working directory

(library (jerboa-coreutils pwd)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name)
          (jerboa core)
          (only (std sugar) with-catch)
          (std cli getopt)
          (jerboa-coreutils common)
          (jerboa-coreutils common version)
          (jerboa-coreutils common security))

  (def (main . args)
    (parameterize ((program-name "pwd"))
      (init-security!)
      (install-proc-only-landlock!)
      (install-readonly-seccomp!)
      (call-with-getopt
        (lambda (_ opt)
            (if (hash-get opt 'logical)
              ;; Logical: use $PWD if valid
              (let ((pwd (getenv "PWD" #f)))
                (if (and pwd (file-exists? pwd))
                  (displayln pwd)
                  (displayln (strip-trailing-slash (current-directory)))))
              ;; Physical (default): resolve symlinks
              (displayln (strip-trailing-slash (current-directory)))))
        args
        'program: "pwd"
        'help: "Print the full filename of the current working directory."
        (flag 'logical "-L" "--logical"
          'help: "use PWD from environment, even if it contains symlinks")
        (flag 'physical "-P" "--physical"
          'help: "avoid all symlinks (default)"))))

  (def (strip-trailing-slash path)
    (let ((len (string-length path)))
      (if (and (> len 1) (eqv? (string-ref path (- len 1)) #\/))
        (substring path 0 (- len 1))
        path)))

  ) ;; end library
