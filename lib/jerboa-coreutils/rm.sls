#!chezscheme
;;; rm.sls -- Remove files or directories

(library (jerboa-coreutils rm)
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
          (jerboa-coreutils common version)
          (jerboa-coreutils common security))

  (define _load-ffi (begin (load-shared-object #f) (void)))

  (define ffi-unlink (foreign-procedure "unlink" (string) int))
  (define ffi-rmdir (foreign-procedure "rmdir" (string) int))

  (define *exit-code* 0)

  (define ffi-lstat-type (foreign-procedure "coreutils_lstat_type" (string) int))

  (def (file-type path)
    ;; -1=not found, 0=file/symlink/other, 1=directory
    ;; Uses lstat so symlinks are not followed — a symlink to a directory
    ;; is reported as 0 (file/other), preventing rm -rf from following
    ;; symlinks into other directory trees.
    (ffi-lstat-type path))

  ;; Confirm removal
  (def (confirm-remove path type-str)
    (eprintf "rm: remove ~a '~a'? " type-str path)
    (let ((resp (get-line (current-input-port))))
      (and (string? resp)
           (> (string-length resp) 0)
           (or (eqv? (string-ref resp 0) #\y)
               (eqv? (string-ref resp 0) #\Y)))))

  ;; Remove a single file or empty directory
  (def (remove-one path force interactive verbose dir-flag)
    (let ((ftype (file-type path)))
      (cond
        ((< ftype 0)
         (unless force
           (warn "cannot remove '~a': No such file or directory" path)
           (set! *exit-code* 1)))
        ((= ftype 1)
         ;; directory
         (if dir-flag
           (begin
             (when (and interactive (not (confirm-remove path "directory")))
               (set! ftype -1))
             (when (>= ftype 0)
               (let ((rc (ffi-rmdir path)))
                 (cond
                   ((< rc 0)
                    (warn "cannot remove '~a': Directory not empty" path)
                    (set! *exit-code* 1))
                   (else
                    (when verbose
                      (eprintf "removed directory '~a'\n" path)))))))
           (begin
             (warn "cannot remove '~a': Is a directory" path)
             (set! *exit-code* 1))))
        (else
         ;; file, symlink, or other
         (when (or (not interactive)
                   (confirm-remove path "file"))
           (audit-file-delete! path)
           (let ((rc (ffi-unlink path)))
             (cond
               ((< rc 0)
                (warn "cannot remove '~a': Permission denied" path)
                (set! *exit-code* 1))
               (else
                (when verbose
                  (eprintf "removed '~a'\n" path))))))))))

  ;; Recursively remove a directory and all contents
  (def (remove-recursive path force interactive verbose)
    (let ((ftype (file-type path)))
      (cond
        ((< ftype 0)
         (unless force
           (warn "cannot remove '~a': No such file or directory" path)
           (set! *exit-code* 1)))
        ((= ftype 1)
         ;; Directory: remove contents first, then the directory itself
         (when (or (not interactive)
                   (confirm-remove path "directory"))
           (with-catch
             (lambda (e)
               (warn "cannot open directory '~a': ~a" path (error-message e))
               (set! *exit-code* 1))
             (lambda ()
               (let ((entries (directory-list path)))
                 (for-each
                   (lambda (name)
                     (remove-recursive (string-append path "/" name)
                                       force interactive verbose))
                   entries))))
           (let ((rc (ffi-rmdir path)))
             (cond
               ((< rc 0)
                (warn "cannot remove '~a'" path)
                (set! *exit-code* 1))
               (else
                (when verbose
                  (eprintf "removed directory '~a'\n" path)))))))
        (else
         ;; file/symlink/other
         (when (or (not interactive)
                   (confirm-remove path "file"))
           (let ((rc (ffi-unlink path)))
             (cond
               ((< rc 0)
                (warn "cannot remove '~a': Permission denied" path)
                (set! *exit-code* 1))
               (else
                (when verbose
                  (eprintf "removed '~a'\n" path))))))))))

  (def (main . args)
    (parameterize ((program-name "rm"))
      (init-security!)
      (install-io-seccomp!)
      (with-fs-write-capability
      (call-with-getopt
        (lambda (_ opt)
            (when (null? (hash-ref opt 'rest))
              (unless (hash-get opt 'force)
                (die "missing operand")))
            (when (and (not (hash-get opt 'no-preserve-root))
                       (member "/" (hash-ref opt 'rest)))
              (die "it is dangerous to operate recursively on '/'\nUse --no-preserve-root to override this failsafe."))
            (set! *exit-code* 0)
            (for-each
              (lambda (path)
                (if (hash-get opt 'recursive)
                  (remove-recursive path (hash-get opt 'force) (hash-get opt 'interactive) (hash-get opt 'verbose))
                  (remove-one path (hash-get opt 'force) (hash-get opt 'interactive) (hash-get opt 'verbose) (hash-get opt 'dir))))
              (hash-ref opt 'rest))
            (exit *exit-code*))
        args
        'program: "rm"
        'help: "Remove (unlink) the FILE(s)."
        (flag 'force "-f" "--force"
          'help: "ignore nonexistent files and arguments, never prompt")
        (flag 'interactive "-i"
          'help: "prompt before every removal")
        (flag 'recursive "-r" "--recursive"
          'help: "remove directories and their contents recursively")
        (flag 'dir "-d" "--dir"
          'help: "remove empty directories")
        (flag 'verbose "-v" "--verbose"
          'help: "explain what is being done")
        (flag 'no-preserve-root "--no-preserve-root"
          'help: "do not treat '/' specially")
        (rest-arguments 'rest)))))

  ) ;; end library
