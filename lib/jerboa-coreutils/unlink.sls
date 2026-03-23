#!chezscheme
;;; unlink.sls -- Remove a file

(library (jerboa-coreutils unlink)
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
    (parameterize ((program-name "unlink"))
      (init-security!)
      (install-io-seccomp!)
      (call-with-getopt
        (lambda (_ opt)
          (let ((files (hash-ref opt 'rest)))
              (cond
                ((null? files)
                 (die "missing operand"))
                ((> (length files) 1)
                 (die "extra operand '~a'" (cadr files)))
                (else
                  (with-catch
                    (lambda (e)
                      (die "cannot unlink '~a': No such file or directory" (car files)))
                    (lambda ()
                      (delete-file (car files))))))))
        args
        'program: "unlink"
        'help: "Call the unlink function to remove the specified FILE."
        (rest-arguments 'rest))))

  ) ;; end library
