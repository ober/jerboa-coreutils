#!chezscheme
;;; rmdir.sls -- Remove empty directories

(library (jerboa-coreutils rmdir)
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

  (def (main . args)
    (parameterize ((program-name "rmdir"))
      (init-security!)
      (install-io-seccomp!)
      (call-with-getopt
        (lambda (_ opt)
            (when (null? (hash-ref opt 'rest))
              (die "missing operand"))
            (let ((had-error #f))
              (for-each
                (lambda (dir)
                  (if (hash-get opt 'parents)
                    (remove-parents dir (hash-get opt 'verbose) (hash-get opt 'ignore-fail-on-non-empty)
                      (lambda () (set! had-error #t)))
                    (remove-one dir (hash-get opt 'verbose) (hash-get opt 'ignore-fail-on-non-empty)
                      (lambda () (set! had-error #t)))))
                (hash-ref opt 'rest))
              (when had-error
                (exit EXIT_FAILURE))))
        args
        'program: "rmdir"
        'help: "Remove the DIRECTORY(ies), if they are empty."
        (flag 'parents "-p" "--parents"
          'help: "remove DIRECTORY and its ancestors")
        (flag 'verbose "-v" "--verbose"
          'help: "output a diagnostic for every directory processed")
        (flag 'ignore-fail-on-non-empty "--ignore-fail-on-non-empty"
          'help: "ignore each failure that is solely because a directory is non-empty")
        (rest-arguments 'rest))))

  (def (remove-one dir verbose? ignore-nonempty? on-error)
    (with-catch
      (lambda (e)
        (let ((msg (error-message e)))
          (unless (and ignore-nonempty?
                       (or (string-contains msg "not empty")
                           (string-contains msg "Directory not empty")))
            (warn "failed to remove '~a': ~a" dir msg)
            (on-error))))
      (lambda ()
        (delete-directory dir)
        (when verbose?
          (eprintf "~a: removing directory, '~a'\n" (program-name) dir)))))

  (def (remove-parents dir verbose? ignore-nonempty? on-error)
    (let loop ((d dir))
      (when (and (> (string-length d) 0)
                 (not (string=? d "/"))
                 (not (string=? d ".")))
        (remove-one d verbose? ignore-nonempty? on-error)
        ;; Move to parent
        (let ((parent (path-directory d)))
          (when (and parent
                     (> (string-length parent) 0)
                     (not (string=? parent d))
                     (not (string=? parent ".")))
            (loop (let ((p parent))
                    ;; Remove trailing /
                    (if (and (> (string-length p) 1)
                             (eqv? (string-ref p (- (string-length p) 1)) #\/))
                      (substring p 0 (- (string-length p) 1))
                      p))))))))

  (def (string-contains haystack needle)
    (let ((hlen (string-length haystack))
          (nlen (string-length needle)))
      (let loop ((i 0))
        (cond
          ((> (+ i nlen) hlen) #f)
          ((string=? (substring haystack i (+ i nlen)) needle) #t)
          (else (loop (+ i 1)))))))

  ) ;; end library
