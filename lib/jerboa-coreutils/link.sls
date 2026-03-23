#!chezscheme
;;; link.sls -- Create a hard link

(library (jerboa-coreutils link)
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

  ;; Create a hard link using POSIX link() syscall via FFI
  (define _load-ffi (begin (load-shared-object #f) (void)))
  (define ffi-link (foreign-procedure "link" (string string) int))

  (def (create-hard-link target linkname)
    (let ((result (ffi-link target linkname)))
      (unless (zero? result)
        (error 'link "link failed" target linkname))))

  (def (main . args)
    (parameterize ((program-name "link"))
      (init-security!)
      (install-io-seccomp!)
      (call-with-getopt
        (lambda (_ opt)
          (let ((files (hash-ref opt 'rest)))
              (cond
                ((< (length files) 2)
                 (die "missing operand"))
                ((> (length files) 2)
                 (die "extra operand '~a'" (caddr files)))
                (else
                  (with-catch
                    (lambda (e)
                      (die "cannot create link '~a' to '~a': ~a"
                           (cadr files) (car files)
                           (if (message-condition? e)
                             (condition-message e)
                             "unknown error")))
                    (lambda ()
                      (create-hard-link (car files) (cadr files))))))))
        args
        'program: "link"
        'help: "Call the link function to create a link named FILE2 to an existing FILE1."
        (rest-arguments 'rest))))

  ) ;; end library
