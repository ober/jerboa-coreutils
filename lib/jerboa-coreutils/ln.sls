#!chezscheme
;;; ln.sls -- Create links (hard/symbolic)

(library (jerboa-coreutils ln)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name
)
          (except (jerboa core) last)
          (only (std sugar) with-catch)
          (only (std format) eprintf)
          (std cli getopt)
          (jerboa-coreutils common)
          (jerboa-coreutils common version)
          (jerboa-coreutils common security))

  (define _load-ffi (begin (load-shared-object #f) (void)))

  (define ffi-symlink (foreign-procedure "symlink" (string string) int))
  (define ffi-link (foreign-procedure "link" (string string) int))

  (def (main . args)
    (parameterize ((program-name "ln"))
      (init-security!)
      (install-io-seccomp!)
      (call-with-getopt
        (lambda (_ opt)
            (when (null? (hash-ref opt 'rest))
              (die "missing file operand"))
            (let* ((targets (hash-ref opt 'rest))
                   (make-link (if (hash-get opt 'symbolic) ffi-symlink ffi-link))
                   (target-dir (hash-get opt 'target-directory)))
              (cond
                ;; ln -t DIR TARGET...
                (target-dir
                 (for-each
                   (lambda (target)
                     (let ((dest (string-append target-dir "/"
                                   (path-strip-directory target))))
                       (do-link make-link target dest (hash-get opt 'force) (hash-get opt 'verbose) (hash-get opt 'symbolic))))
                   targets))
                ;; ln TARGET (create link in current dir)
                ((= (length targets) 1)
                 (let ((dest (path-strip-directory (car targets))))
                   (do-link make-link (car targets) dest (hash-get opt 'force) (hash-get opt 'verbose) (hash-get opt 'symbolic))))
                ;; ln TARGET LINK_NAME
                ((= (length targets) 2)
                 (let ((target (car targets))
                       (dest (cadr targets)))
                   ;; If dest is a directory, link inside it
                   (let ((dest (if (and (file-exists? dest)
                                       (file-directory? dest))
                                 (string-append dest "/" (path-strip-directory target))
                                 dest)))
                     (do-link make-link target dest (hash-get opt 'force) (hash-get opt 'verbose) (hash-get opt 'symbolic)))))
                ;; ln TARGET... DIRECTORY
                (else
                 (let ((dir (last targets))
                       (srcs (butlast targets)))
                   (unless (and (file-exists? dir)
                                (file-directory? dir))
                     (die "target '~a' is not a directory" dir))
                   (for-each
                     (lambda (target)
                       (let ((dest (string-append dir "/" (path-strip-directory target))))
                         (do-link make-link target dest (hash-get opt 'force) (hash-get opt 'verbose) (hash-get opt 'symbolic))))
                     srcs))))))
        args
        'program: "ln"
        'help: "Make links between files."
        (flag 'symbolic "-s" "--symbolic"
          'help: "make symbolic links instead of hard links")
        (flag 'force "-f" "--force"
          'help: "remove existing destination files")
        (flag 'verbose "-v" "--verbose"
          'help: "print name of each linked file")
        (option 'target-directory "-t" "--target-directory"
          'help: "specify the DIRECTORY in which to create the links"
          'default: #f)
        (flag 'no-target-directory "-T" "--no-target-directory"
          'help: "treat LINK_NAME as a normal file always")
        (rest-arguments 'rest))))

  (def (do-link make-link target dest force? verbose? symbolic?)
    (when force?
      (with-catch void (lambda () (delete-file dest))))
    (let ((rc (make-link target dest)))
      (unless (zero? rc)
        (warn "failed to create ~a link '~a' -> '~a'"
          (if symbolic? "symbolic" "hard") dest target))
      (when (and verbose? (zero? rc))
        (eprintf "'~a' -> '~a'\n" dest target))))

  (def (last lst)
    (if (null? (cdr lst)) (car lst) (last (cdr lst))))

  (def (butlast lst)
    (if (null? (cdr lst)) '()
      (cons (car lst) (butlast (cdr lst)))))

  ) ;; end library
