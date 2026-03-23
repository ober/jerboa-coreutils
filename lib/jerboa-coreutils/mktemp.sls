#!chezscheme
;;; mktemp.sls -- Create temporary files/directories

(library (jerboa-coreutils mktemp)
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

  (define _load-ffi (begin (load-shared-object #f) (void)))
  (define ffi-mkstemp (foreign-procedure "coreutils_mkstemp" (string) int))
  (define ffi-mkdtemp (foreign-procedure "coreutils_mkdtemp" (string) int))
  (define ffi-mkstemp-get-path (foreign-procedure "coreutils_mkstemp_get_path" () string))
  (define ffi-close (foreign-procedure "close" (int) int))

  (def (count-trailing-x template)
    (let loop ((i (- (string-length template) 1)) (count 0))
      (if (and (>= i 0) (eqv? (string-ref template i) #\X))
        (loop (- i 1) (+ count 1))
        count)))

  (def (main . args)
    (parameterize ((program-name "mktemp"))
      (init-security!)
      (install-io-seccomp!)
      (call-with-getopt
        (lambda (_ opt)
            (let* ((template (if (pair? (hash-ref opt 'rest))
                               (car (hash-ref opt 'rest))
                               "tmp.XXXXXXXXXX"))
                   (tmpdir (or (hash-get opt 'tmpdir)
                               (getenv "TMPDIR" "/tmp")))
                   (x-count (count-trailing-x template)))
              (when (< x-count 3)
                (die "too few X's in template '~a'" template))
              ;; Build full path with XXXXXX suffix for mkstemp/mkdtemp
              (let* ((base (substring template 0 (- (string-length template) x-count)))
                     (mktemp-template (string-append base (make-string (max x-count 6) #\X)))
                     (full-template (if (string-index mktemp-template #\/)
                                      mktemp-template
                                      (string-append tmpdir "/" mktemp-template))))
                (if (hash-get opt 'directory)
                  ;; Create directory atomically via mkdtemp(3)
                  (let ((rc (ffi-mkdtemp full-template)))
                    (if (< rc 0)
                      (die "failed to create directory via template '~a'" full-template)
                      (displayln (ffi-mkstemp-get-path))))
                  ;; Create file atomically via mkstemp(3) — O_EXCL guaranteed
                  (let ((fd (ffi-mkstemp full-template)))
                    (if (< fd 0)
                      (die "failed to create file via template '~a'" full-template)
                      (begin
                        (ffi-close fd)
                        (displayln (ffi-mkstemp-get-path)))))))))
        args
        'program: "mktemp"
        'help: "Create a temporary file or directory, safely, and print its name."
        (flag 'directory "-d" "--directory"
          'help: "create a directory, not a file")
        (flag 'quiet "-q" "--quiet"
          'help: "suppress diagnostics about file/dir-creation failure")
        (flag 'dry-run "-u" "--dry-run"
          'help: "do not create anything; merely print a name (unsafe)")
        (option 'tmpdir "-p" "--tmpdir"
          'help: "interpret TEMPLATE relative to DIR"
          'default: #f)
        (option 'suffix "--suffix"
          'help: "append SUFF to TEMPLATE"
          'default: #f)
        (rest-arguments 'rest))))

  (def (string-index str ch)
    (let loop ((i 0))
      (cond
        ((>= i (string-length str)) #f)
        ((eqv? (string-ref str i) ch) i)
        (else (loop (+ i 1))))))

  ) ;; end library
