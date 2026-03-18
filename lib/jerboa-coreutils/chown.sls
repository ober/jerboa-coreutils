#!chezscheme
;;; chown.sls -- Change file owner/group

(library (jerboa-coreutils chown)
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

  (def (ffi-stat-isdir path)
    (if (file-directory? path) 1 0))

  (def (ffi-getpwnam-uid name)
    (with-catch
      (lambda (e) -1)
      (lambda ()
        (let-values (((to-stdin from-stdout from-stderr pid)
                      (open-process-ports
                        (string-append "id -u " name " 2>/dev/null")
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

  (def (string-find-char str ch)
    (let loop ((i 0))
      (cond
        ((>= i (string-length str)) #f)
        ((eqv? (string-ref str i) ch) i)
        (else (loop (+ i 1))))))

  (def (resolve-uid spec)
    (let ((n (string->number spec)))
      (if n
        (inexact->exact n)
        (ffi-getpwnam-uid spec))))

  (def (resolve-gid spec)
    (let ((n (string->number spec)))
      (if n
        (inexact->exact n)
        (ffi-getgrnam-gid spec))))

  ;; Parse USER:GROUP, USER, :GROUP -> (uid . gid) with -1 for "no change"
  (def (parse-owner-spec spec)
    (let ((colon-pos (string-find-char spec #\:)))
      (if colon-pos
        (let ((user-part (substring spec 0 colon-pos))
              (group-part (substring spec (+ colon-pos 1) (string-length spec))))
          (let ((uid (if (string=? user-part "") -1 (resolve-uid user-part)))
                (gid (if (string=? group-part "") -1 (resolve-gid group-part))))
            (cons uid gid)))
        ;; No colon -> just user
        (let ((uid (resolve-uid spec)))
          (cons uid -1)))))

  (def (do-chown path uid gid no-deref verbose changes-only)
    (let ((rc ((if no-deref ffi-lchown ffi-chown) path uid gid)))
      (cond
        ((< rc 0)
         (warn "cannot chown '~a'" path)
         (set! exit-status 1))
        (verbose
         (displayln "changed ownership of '" path "'")))))

  (def (do-chown-recursive path uid gid no-deref verbose changes-only)
    (let ((isdir (ffi-stat-isdir path)))
      (let ((rc ((if no-deref ffi-lchown ffi-chown) path uid gid)))
        (when (< rc 0)
          (warn "cannot chown '~a'" path)
          (set! exit-status 1))
        (when verbose
          (displayln "changed ownership of '" path "'")))
      (when (= isdir 1)
        (with-catch
          (lambda (e)
            (warn "cannot open directory '~a'" path)
            (set! exit-status 1))
          (lambda ()
            (let ((files (directory-list path)))
              (for-each
                (lambda (name)
                  (do-chown-recursive (string-append path "/" name)
                                      uid gid no-deref verbose changes-only))
                files)))))))

  (def (main . args)
    (parameterize ((program-name "chown"))
      (call-with-getopt
        (lambda (_ opt)
          (when (null? (hash-ref opt 'rest))
            (die "missing operand"))
          (let* ((owner-spec (car (hash-ref opt 'rest)))
                 (files (cdr (hash-ref opt 'rest))))
            (when (null? files)
              (die "missing operand after '~a'" owner-spec))
            (let ((owner (parse-owner-spec owner-spec)))
              (let ((uid (car owner))
                    (gid (cdr owner)))
                (for-each
                  (lambda (file)
                    (if (hash-get opt 'recursive)
                      (do-chown-recursive file uid gid (hash-get opt 'no-deref) (hash-get opt 'verbose) #f)
                      (do-chown file uid gid (hash-get opt 'no-deref) (hash-get opt 'verbose) #f)))
                  files)))))
        args
        'program: "chown"
        'help: "Change the owner and/or group of each FILE to OWNER and/or GROUP."
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
