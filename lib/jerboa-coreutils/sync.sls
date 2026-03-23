#!chezscheme
;;; sync.sls -- Synchronize cached writes to persistent storage

(library (jerboa-coreutils sync)
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

  ;; Call system sync via shell
  (def (sync-filesystems)
    (with-catch
      (lambda (e) (void))
      (lambda ()
        (let ((p (process "/bin/sync")))
          (close-input-port (car p))
          (close-output-port (cadr p))))))

  (def (main . args)
    (parameterize ((program-name "sync"))
      (init-security!)
      (install-io-seccomp!)
      (call-with-getopt
        (lambda (_ opt)
          (if (null? (hash-ref opt 'rest))
            ;; No args: sync everything
            (sync-filesystems)
            ;; With file args: open and flush each
            (for-each
              (lambda (file)
                (with-catch
                  (lambda (e) (warn "error opening '~a': ~a" file
                                    (if (message-condition? e)
                                      (condition-message e)
                                      "unknown error")))
                  (lambda ()
                    (let ((port (open-file-input/output-port file)))
                      (flush-output-port port)
                      (close-port port)))))
              (hash-ref opt 'rest))))
        args
        'program: "sync"
        'help: "Synchronize cached writes to persistent storage."
        (flag 'data "-d" "--data"
          'help: "sync only file data, no unneeded metadata")
        (flag 'file-system "-f" "--file-system"
          'help: "sync the file systems that contain the files")
        (rest-arguments 'rest))))

  ) ;; end library
