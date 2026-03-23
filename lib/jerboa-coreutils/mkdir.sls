#!chezscheme
;;; mkdir.sls -- Create directories

(library (jerboa-coreutils mkdir)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name)
          (jerboa core)
          (only (std sugar) with-catch)
          (only (std format) eprintf)
          (std cli getopt)
          (jerboa-coreutils common)
          (jerboa-coreutils common version)
          (jerboa-coreutils common security))

  (define _load-ffi (begin (load-shared-object #f) (void)))

  (define ffi-chmod (foreign-procedure "chmod" (string int) int))

  (def (parse-octal-mode str)
    (let ((n (string->number str 8)))
      (if n n
        (begin (warn "invalid mode '~a'" str) #o777))))

  (def (main . args)
    (parameterize ((program-name "mkdir"))
      (init-security!)
      (install-io-seccomp!)
      (call-with-getopt
        (lambda (_ opt)
            (when (null? (hash-ref opt 'rest))
              (die "missing operand"))
            (let ((mode (if (hash-get opt 'mode) (parse-octal-mode (hash-ref opt 'mode)) #f)))
              (for-each
                (lambda (dir)
                  (with-catch
                    (lambda (e)
                      (warn "cannot create directory '~a': ~a" dir (error-message e)))
                    (lambda ()
                      (if (hash-get opt 'parents)
                        (begin
                          (create-directory* dir)
                          (when mode
                            (ffi-chmod dir mode)))
                        (begin
                          (create-directory dir)
                          (when mode
                            (ffi-chmod dir mode))))
                      (when (hash-get opt 'verbose)
                        (eprintf "~a: created directory '~a'\n" (program-name) dir)))))
                (hash-ref opt 'rest))))
        args
        'program: "mkdir"
        'help: "Create the DIRECTORY(ies), if they do not already exist."
        (flag 'parents "-p" "--parents"
          'help: "no error if existing, make parent directories as needed")
        (option 'mode "-m" "--mode"
          'help: "set file mode (as in chmod), not a=rwx - umask"
          'default: #f)
        (flag 'verbose "-v" "--verbose"
          'help: "print a message for each created directory")
        (rest-arguments 'rest))))

  ) ;; end library
