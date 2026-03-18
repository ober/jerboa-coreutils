#!chezscheme
;;; install.sls -- Copy files and set attributes

(library (jerboa-coreutils install)
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

  (define ffi-chmod (foreign-procedure "chmod" (string int) int))
  (define ffi-chown (foreign-procedure "chown" (string int int) int))
  (define ffi-mkdir (foreign-procedure "mkdir" (string int) int))

  ;; Parse octal mode string
  (def (parse-octal-mode str)
    (string->number str 8))

  ;; Copy file contents from src to dst
  (def (copy-file-contents src dst)
    (let* ((in (with-catch
                 (lambda (e) (die "cannot open '~a' for reading: ~a" src (error-message e)))
                 (lambda () (open-file-input-port src))))
           (out (with-catch
                  (lambda (e)
                    (close-port in)
                    (die "cannot open '~a' for writing: ~a" dst (error-message e)))
                  (lambda () (open-file-output-port dst (file-options no-fail))))))
      (let ((buf (make-bytevector 65536)))
        (let loop ()
          (let ((n (get-bytevector-n! in buf 0 65536)))
            (unless (eof-object? n)
              (put-bytevector out buf 0 n)
              (loop)))))
      (close-port in)
      (close-port out)))

  ;; Resolve owner name or numeric uid
  (def (resolve-uid-install spec)
    (if (not spec) -1
      (let ((n (string->number spec)))
        (if n (inexact->exact n)
          (with-catch
            (lambda (e) (die "invalid user '~a'" spec) -1)
            (lambda ()
              (let-values (((to-stdin from-stdout from-stderr pid)
                            (open-process-ports
                              (string-append "id -u " spec " 2>/dev/null")
                              (buffer-mode block) (native-transcoder))))
                (close-port to-stdin)
                (let ((result (get-line from-stdout)))
                  (close-port from-stdout)
                  (close-port from-stderr)
                  (if (and result (not (eof-object? result)))
                    (let ((uid (string->number result)))
                      (if uid (inexact->exact uid) -1))
                    -1)))))))))

  ;; Resolve group name or numeric gid
  (def (resolve-gid-install spec)
    (if (not spec) -1
      (let ((n (string->number spec)))
        (if n (inexact->exact n)
          (with-catch
            (lambda (e) (die "invalid group '~a'" spec) -1)
            (lambda ()
              (let-values (((to-stdin from-stdout from-stderr pid)
                            (open-process-ports
                              (string-append "getent group " spec " 2>/dev/null | cut -d: -f3")
                              (buffer-mode block) (native-transcoder))))
                (close-port to-stdin)
                (let ((result (get-line from-stdout)))
                  (close-port from-stdout)
                  (close-port from-stderr)
                  (if (and result (not (eof-object? result)))
                    (let ((gid (string->number result)))
                      (if gid (inexact->exact gid) -1))
                    -1)))))))))

  ;; Create all components of a directory path (like mkdir -p)
  (def (mkdir-parents dir)
    (unless (file-directory? dir)
      (let ((parent (path-dirname dir)))
        (when (and (not (string=? parent dir))
                   (not (string=? parent ""))
                   (not (string=? parent "/")))
          (mkdir-parents parent)))
      (with-catch
        (lambda (e) #t) ;; may already exist
        (lambda ()
          (ffi-mkdir dir #o755)))))

  (def (path-dirname path)
    (let loop ((i (- (string-length path) 1)))
      (cond
        ((< i 0) ".")
        ((eqv? (string-ref path i) #\/) (if (= i 0) "/" (substring path 0 i)))
        (else (loop (- i 1))))))

  ;; Create directory with attributes
  (def (install-directory dir mode-val uid gid verbose)
    (mkdir-parents dir)
    (when mode-val
      (ffi-chmod dir mode-val))
    (when (or (>= uid 0) (>= gid 0))
      (ffi-chown dir uid gid))
    (when verbose
      (eprintf "install: creating directory '~a'\n" dir)))

  ;; Install a single file
  (def (install-file src dst mode-val uid gid verbose strip)
    (copy-file-contents src dst)
    (when mode-val
      (let ((rc (ffi-chmod dst mode-val)))
        (when (< rc 0)
          (warn "cannot change permissions of '~a'" dst))))
    (when (or (>= uid 0) (>= gid 0))
      (let ((rc (ffi-chown dst uid gid)))
        (when (< rc 0)
          (warn "cannot change ownership of '~a'" dst))))
    (when verbose
      (eprintf "install: '~a' -> '~a'\n" src dst)))

  ;; Determine destination path when copying to a directory
  (def (path-basename path)
    (let loop ((i (- (string-length path) 1)))
      (cond
        ((< i 0) path)
        ((eqv? (string-ref path i) #\/) (substring path (+ i 1) (string-length path)))
        (else (loop (- i 1))))))

  ;; List utility helpers
  (def (last-element lst)
    (if (null? (cdr lst)) (car lst) (last-element (cdr lst))))

  (def (butlast lst)
    (if (null? (cdr lst)) '()
      (cons (car lst) (butlast (cdr lst)))))

  (def (main . args)
    (parameterize ((program-name "install"))
      (call-with-getopt
        (lambda (_ opt)
            (let* ((mode-val (if (hash-get opt 'mode) (parse-octal-mode (hash-ref opt 'mode)) #o755))
                   (uid (resolve-uid-install (hash-get opt 'owner)))
                   (gid (resolve-gid-install (hash-get opt 'group)))
                   (verbose (hash-get opt 'verbose))
                   (files (hash-ref opt 'rest)))
              (cond
                ((hash-get opt 'directory)
                 ;; -d mode: create directories
                 (for-each
                   (lambda (dir)
                     (install-directory dir mode-val uid gid verbose))
                   files))
                ((null? files)
                 (die "missing file operand"))
                ((null? (cdr files))
                 (die "missing destination file operand after '~a'" (car files)))
                (else
                 ;; Copy files to destination
                 (let* ((dst (last-element files))
                        (srcs (butlast files))
                        (dst-is-dir (file-directory? dst)))
                   (if (and (> (length srcs) 1) (not dst-is-dir))
                     (die "target '~a' is not a directory" dst)
                     (for-each
                       (lambda (src)
                         (let ((target (if dst-is-dir
                                         (string-append dst "/" (path-basename src))
                                         dst)))
                           (install-file src target mode-val uid gid verbose (hash-get opt 'strip))))
                       srcs)))))))
        args
        'program: "install"
        'help: "Copy files and set attributes."
        (flag 'directory "-d" "--directory"
          'help: "treat all arguments as directory names; create all components")
        (option 'mode "-m" "--mode"
          'help: "set permission mode (as in chmod), instead of rwxr-xr-x"
          'default: #f)
        (option 'owner "-o" "--owner"
          'help: "set ownership (super-user only)"
          'default: #f)
        (option 'group "-g" "--group"
          'help: "set group ownership"
          'default: #f)
        (flag 'verbose "-v" "--verbose"
          'help: "print the name of each directory as it is created")
        (flag 'strip "-s" "--strip"
          'help: "strip symbol tables (ignored in this implementation)")
        (rest-arguments 'rest))))

  ) ;; end library
