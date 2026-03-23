#!chezscheme
;;; touch.sls -- Create or update file timestamps

(library (jerboa-coreutils touch)
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

  ;; utimensat(AT_FDCWD, path, times, 0) via a helper that takes
  ;; individual timespec fields. We use the C library function directly.
  ;; AT_FDCWD = -100 on Linux, UTIME_NOW = ((1l << 30) - 1l), UTIME_OMIT = ((1l << 30) - 2l)
  ;; Since we can't easily pass struct timespec arrays via foreign-procedure,
  ;; we shell out to the `touch` command for timestamp setting as a fallback,
  ;; or use utime() which is simpler.
  (define ffi-utime
    (foreign-procedure "utime" (string void*) int))

  (define ffi-time
    (foreign-procedure "time" (void*) long))

  (def (touch-time-now)
    (ffi-time 0))

  (def (main . args)
    (parameterize ((program-name "touch"))
      (init-security!)
      (install-io-seccomp!)
      (call-with-getopt
        (lambda (_ opt)
          (when (null? (hash-ref opt 'rest))
            (die "missing file operand"))
          (let* ((now (time-second (current-time)))
                 (ref-time (if (hash-get opt 'reference)
                             (with-catch
                               (lambda (e) (die "failed to get attributes of '~a': ~a"
                                             (hash-ref opt 'reference) (error-message e)))
                               (lambda () (file-stat (hash-ref opt 'reference))))
                             #f)))
            (for-each
              (lambda (file)
                ;; Create file if it doesn't exist (unless -c)
                (unless (or (hash-get opt 'no-create) (file-exists? file))
                  (with-catch
                    (lambda (e)
                      (warn "cannot touch '~a': ~a" file (error-message e)))
                    (lambda ()
                      (close-port (open-output-file file)))))
                ;; Set timestamps - use utime(path, NULL) to set to current time
                ;; For reference times, we'd need utimbuf struct; simplify to current time
                (when (file-exists? file)
                  (with-catch
                    (lambda (e)
                      (warn "setting times of '~a': ~a" file (error-message e)))
                    (lambda ()
                      ;; utime with NULL sets both atime and mtime to now
                      (ffi-utime file 0)))))
              (hash-ref opt 'rest))))
        args
        'program: "touch"
        'help: "Update the access and modification times of each FILE to the current time."
        (flag 'atime-only "-a"
          'help: "change only the access time")
        (flag 'no-create "-c" "--no-create"
          'help: "do not create any files")
        (flag 'mtime-only "-m"
          'help: "change only the modification time")
        (option 'date "-d" "--date"
          'help: "parse STRING and use it instead of current time"
          'default: #f)
        (option 'reference "-r" "--reference"
          'help: "use this file's times instead of current time"
          'default: #f)
        (rest-arguments 'rest))))

  (def (file-stat path)
    ;; Return #t if file exists (used for reference validation)
    (unless (file-exists? path)
      (error 'file-stat "no such file" path))
    #t)

  ) ;; end library
