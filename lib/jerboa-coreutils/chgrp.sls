#!chezscheme
;;; chgrp.sls -- Change file group

(library (jerboa-coreutils chgrp)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name)
          (jerboa core)
          (only (std sugar) with-catch)
          (only (std format) eprintf format)
          (std cli getopt)
          (jerboa-coreutils common)
          (jerboa-coreutils common version))

  (define _load-ffi (begin (load-shared-object #f) (void)))

  (define exit-status 0)

  (define ffi-chown (foreign-procedure "chown" (string int int) int))
  (define ffi-lchown (foreign-procedure "lchown" (string int int) int))

  (def (ffi-chgrp-chown path gid)
    (ffi-chown path -1 gid))

  (def (ffi-chgrp-lchown path gid)
    (ffi-lchown path -1 gid))

  (def (ffi-stat-isdir path)
    (if (file-directory? path) 1 0))

  (def (ffi-getgrnam-gid name)
    (with-catch
      (lambda (e) -1)
      (lambda ()
        (let-values (((to-stdin from-stdout from-stderr pid)
                      (open-process-ports
                        (string-append "getent group " name " 2>/dev/null | cut -d: -f3")
                        (buffer-mode block)
                        (native-transcoder))))
          (close-port to-stdin)
          (let ((result (get-line from-stdout)))
            (close-port from-stdout)
            (close-port from-stderr)
            (if (and result (not (eof-object? result)))
              (let ((n (string->number result)))
                (if n (inexact->exact n) -1))
              -1))))))

  (def (resolve-group-id spec)
    (let ((n (string->number spec)))
      (if n
        (inexact->exact n)
        (ffi-getgrnam-gid spec))))

  (def (do-chgrp path gid no-deref verbose)
    (let ((rc ((if no-deref ffi-chgrp-lchown ffi-chgrp-chown) path gid)))
      (cond
        ((< rc 0)
         (warn "cannot chgrp '~a'" path)
         (set! exit-status 1))
        (verbose
         (displayln "changed group of '" path "'")))))

  (def (do-chgrp-recursive path gid no-deref verbose)
    (do-chgrp path gid no-deref verbose)
    (when (= (ffi-stat-isdir path) 1)
      (with-catch
        (lambda (e)
          (warn "cannot open directory '~a'" path)
          (set! exit-status 1))
        (lambda ()
          (let ((files (directory-list path)))
            (for-each
              (lambda (name)
                (do-chgrp-recursive (string-append path "/" name) gid no-deref verbose))
              files))))))

  (def (main . args)
    (parameterize ((program-name "chgrp"))
      (call-with-getopt
        (lambda (_ opt)
          (when (null? (hash-ref opt 'rest))
            (die "missing operand"))
          (let* ((group-spec (car (hash-ref opt 'rest)))
                 (files (cdr (hash-ref opt 'rest))))
            (when (null? files)
              (die "missing operand after '~a'" group-spec))
            (let ((gid (resolve-group-id group-spec)))
              (when (< gid 0)
                (die "invalid group: '~a'" group-spec))
              (for-each
                (lambda (file)
                  (if (hash-get opt 'recursive)
                    (do-chgrp-recursive file gid (hash-get opt 'no-deref) (hash-get opt 'verbose))
                    (do-chgrp file gid (hash-get opt 'no-deref) (hash-get opt 'verbose))))
                files))))
        args
        'program: "chgrp"
        'help: "Change the group of each FILE to GROUP."
        (flag 'recursive "-R" "--recursive"
          'help: "operate on files and directories recursively")
        (flag 'no-deref "--no-dereference"
          'help: "affect symbolic links instead of referenced files")
        (flag 'verbose "-v" "--verbose"
          'help: "output a diagnostic for every file processed")
        (flag 'changes "-c" "--changes"
          'help: "like verbose but report only when a change is made")
        (rest-arguments 'rest))
      (unless (= exit-status 0) (exit exit-status))))

  ) ;; end library
