#!chezscheme
;;; cp.sls -- Copy files and directories

(library (jerboa-coreutils cp)
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
  (define ffi-symlink (foreign-procedure "symlink" (string string) int))
  (define ffi-link (foreign-procedure "link" (string string) int))
  (define ffi-mkdir (foreign-procedure "mkdir" (string int) int))
  (define ffi-unlink (foreign-procedure "unlink" (string) int))
  (define ffi-cp-lstat (foreign-procedure "coreutils_cp_lstat" (string) int))
  (define ffi-cp-stat-get (foreign-procedure "coreutils_cp_stat_get" (int) long))
  (define ffi-cp-readlink (foreign-procedure "coreutils_cp_readlink" (string) string))
  (define ffi-cp-stat-atime (foreign-procedure "coreutils_stat_atime" (string) long))
  (define ffi-cp-stat-mtime (foreign-procedure "coreutils_stat_mtime" (string) long))
  (define ffi-utime (foreign-procedure "coreutils_utime" (string long long) int))
  (define ffi-stat-mode (foreign-procedure "coreutils_stat_get_mode" (string) int))

  (define *exit-code* 0)

  ;; Get file type via lstat FFI: 'regular, 'directory, 'symlink, 'other, or #f
  (def (get-file-type path)
    (let ((rc (ffi-cp-lstat path)))
      (if (< rc 0)
        #f
        (let ((type-code (ffi-cp-stat-get 7)))
          (cond
            ((= type-code 0) 'regular)
            ((= type-code 1) 'directory)
            ((= type-code 2) 'symlink)
            (else 'other))))))

  (def (read-symlink path)
    (ffi-cp-readlink path))

  (def (path-basename path)
    (let loop ((i (- (string-length path) 1)))
      (cond
        ((< i 0) path)
        ((eqv? (string-ref path i) #\/)
         (substring path (+ i 1) (string-length path)))
        (else (loop (- i 1))))))

  ;; Copy file contents using buffered I/O
  (def (copy-file-data src dst)
    (let ((in (open-file-input-port src))
          (out (open-file-output-port dst (file-options no-fail))))
      (dynamic-wind
        (lambda () (void))
        (lambda ()
          (let ((buf (make-bytevector 65536)))
            (let loop ()
              (let ((n (get-bytevector-n! in buf 0 65536)))
                (unless (eof-object? n)
                  (put-bytevector out buf 0 n)
                  (loop))))))
        (lambda ()
          (close-port in)
          (close-port out)))))

  ;; Confirm overwrite
  (def (confirm-overwrite dst)
    (eprintf "cp: overwrite '~a'? " dst)
    (let ((resp (get-line (current-input-port))))
      (and (string? resp)
           (> (string-length resp) 0)
           (or (eqv? (string-ref resp 0) #\y)
               (eqv? (string-ref resp 0) #\Y)))))

  ;; Preserve mode and timestamps via FFI
  (def (preserve-attributes src dst)
    (let ((src-mode (ffi-stat-mode src))
          (src-atime (ffi-cp-stat-atime src))
          (src-mtime (ffi-cp-stat-mtime src)))
      (when (>= src-mode 0)
        (ffi-chmod dst src-mode))
      (when (and (>= src-atime 0) (>= src-mtime 0))
        (ffi-utime dst src-atime src-mtime))))

  ;; Copy a single item
  (def (copy-one src dst force interactive verbose preserve
                 make-symlink make-hardlink recursive)
    (let ((src-type (get-file-type src)))
      (unless src-type
        (warn "cannot stat '~a': No such file or directory" src)
        (set! *exit-code* 1))
      (when src-type
        ;; Handle destination existing
        (let ((dst-type (get-file-type dst)))
          (when dst-type
            (when interactive
              (unless (confirm-overwrite dst)
                (set! src-type #f))))  ;; skip
          (when (and dst-type src-type (not interactive))
            (with-catch (lambda (e) #t) (lambda () (ffi-unlink dst)))))

        (when src-type
          (cond
            (make-symlink
             (let ((rc (ffi-symlink src dst)))
               (unless (zero? rc)
                 (warn "cannot create symbolic link '~a' -> '~a'" dst src)
                 (set! *exit-code* 1))
               (when (and verbose (zero? rc))
                 (eprintf "'~a' -> '~a'\n" dst src))))

            (make-hardlink
             (let ((rc (ffi-link src dst)))
               (unless (zero? rc)
                 (warn "cannot create hard link '~a' -> '~a'" dst src)
                 (set! *exit-code* 1))
               (when (and verbose (zero? rc))
                 (eprintf "'~a' -> '~a'\n" dst src))))

            ((eq? src-type 'directory)
             (if (not recursive)
               (begin
                 (warn "-r not specified; omitting directory '~a'" src)
                 (set! *exit-code* 1))
               (copy-directory src dst force interactive verbose preserve)))

            ((eq? src-type 'symlink)
             (let ((target (read-symlink src)))
               (if (not target)
                 (begin
                   (warn "cannot read symbolic link '~a'" src)
                   (set! *exit-code* 1))
                 (let ((rc (ffi-symlink target dst)))
                   (unless (zero? rc)
                     (warn "cannot create symbolic link '~a'" dst)
                     (set! *exit-code* 1))
                   (when (and verbose (zero? rc))
                     (eprintf "'~a' -> '~a'\n" dst src))))))

            (else
             ;; Regular file copy
             (with-catch
               (lambda (e)
                 (warn "cannot copy '~a' to '~a': ~a" src dst (error-message e))
                 (set! *exit-code* 1))
               (lambda ()
                 (copy-file-data src dst)
                 (when preserve
                   (preserve-attributes src dst))
                 (when verbose
                   (eprintf "'~a' -> '~a'\n" dst src))))))))))

  ;; Copy a directory recursively
  (def (copy-directory src dst force interactive verbose preserve)
    (let ((dst-type (get-file-type dst)))
      (unless (eq? dst-type 'directory)
        (ffi-mkdir dst #o755)))
    (with-catch
      (lambda (e)
        (warn "cannot open directory '~a': ~a" src (error-message e))
        (set! *exit-code* 1))
      (lambda ()
        (let ((entries (directory-list src)))
          (for-each
            (lambda (name)
              (let ((s (string-append src "/" name))
                    (d (string-append dst "/" name)))
                ;; Skip symlinks that point outside the source tree
                (let ((s-type (get-file-type s)))
                  (if (and (eq? s-type 'symlink)
                           (let ((target (read-symlink s)))
                             (and target
                                  ;; If symlink target is absolute, check it
                                  (> (string-length target) 0)
                                  (eqv? (string-ref target 0) #\/)
                                  (not (path-within-base? target src)))))
                    (begin
                      (warn "skipping symlink '~a' that points outside source tree" s)
                      (set! *exit-code* 1))
                    (copy-one s d force interactive verbose preserve #f #f #t)))))
            entries))))
    (when preserve
      (preserve-attributes src dst))
    (when verbose
      (eprintf "'~a' -> '~a'\n" dst src)))

  (def (main . args)
    (parameterize ((program-name "cp"))
      (call-with-getopt
        (lambda (_ opt)
          (when (null? (hash-ref opt 'rest))
            (die "missing file operand"))
          (when (null? (cdr (hash-ref opt 'rest)))
            (die "missing destination file operand after '~a'" (car (hash-ref opt 'rest))))
          (set! *exit-code* 0)
          (let* ((all (hash-ref opt 'rest))
                 (dst (let loop ((l all))
                        (if (null? (cdr l)) (car l) (loop (cdr l)))))
                 (srcs (let loop ((l all) (acc '()))
                         (if (null? (cdr l)) (reverse acc)
                           (loop (cdr l) (cons (car l) acc)))))
                 (dst-type (get-file-type dst))
                 (recursive (or (hash-get opt 'recursive) (hash-get opt 'archive)))
                 (preserve (or (hash-get opt 'preserve) (hash-get opt 'archive)))
                 (force (or (hash-get opt 'force) (hash-get opt 'archive))))
            (when (and (> (length srcs) 1)
                       (not (eq? dst-type 'directory)))
              (die "target '~a' is not a directory" dst))
            (for-each
              (lambda (src)
                (let ((target (if (eq? dst-type 'directory)
                                (string-append dst "/" (path-basename src))
                                dst)))
                  (copy-one src target force (hash-get opt 'interactive) (hash-get opt 'verbose) preserve
                            (hash-get opt 'symlink) (hash-get opt 'link) recursive)))
              srcs))
          (exit *exit-code*))
        args
        'program: "cp"
        'help: "Copy SOURCE to DEST, or multiple SOURCE(s) to DIRECTORY."
        (flag 'recursive "-r" "--recursive"
          'help: "copy directories recursively")
        (flag 'force "-f" "--force"
          'help: "if destination cannot be opened, remove and try again")
        (flag 'interactive "-i" "--interactive"
          'help: "prompt before overwrite")
        (flag 'verbose "-v" "--verbose"
          'help: "explain what is being done")
        (flag 'preserve "-p"
          'help: "preserve mode, ownership, timestamps")
        (flag 'archive "-a" "--archive"
          'help: "same as -rpf (archive mode)")
        (flag 'link "-l" "--link"
          'help: "hard link files instead of copying")
        (flag 'symlink "-s" "--symbolic-link"
          'help: "make symbolic links instead of copying")
        (rest-arguments 'rest))))

  ) ;; end library
