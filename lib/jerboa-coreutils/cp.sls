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

  (define *exit-code* 0)

  ;; Get file type via stat command: 'regular, 'directory, 'symlink, 'other, or #f
  (def (get-file-type path)
    (with-catch
      (lambda (e) #f)
      (lambda ()
        (let ((cmd (string-append "stat -c '%F' " (shell-quote path) " 2>/dev/null")))
          (let-values (((to-stdin from-stdout from-stderr pid)
                        (open-process-ports cmd (buffer-mode block) (native-transcoder))))
            (close-port to-stdin)
            (let ((line (get-line from-stdout)))
              (close-port from-stdout)
              (close-port from-stderr)
              (if (or (not line) (eof-object? line))
                #f
                (cond
                  ((string-contains? line "symbolic link") 'symlink)
                  ((string-contains? line "directory") 'directory)
                  ((string-contains? line "regular") 'regular)
                  (else 'other)))))))))

  ;; Get file type via lstat (does not follow symlinks)
  (def (get-lfile-type path)
    (with-catch
      (lambda (e) #f)
      (lambda ()
        (let ((cmd (string-append "stat -L -c '%F' " (shell-quote path) " 2>/dev/null")))
          (let-values (((to-stdin from-stdout from-stderr pid)
                        (open-process-ports cmd (buffer-mode block) (native-transcoder))))
            (close-port to-stdin)
            (let ((line (get-line from-stdout)))
              (close-port from-stdout)
              (close-port from-stderr)
              (if (or (not line) (eof-object? line))
                ;; Fall back to non-L stat for the raw type
                (get-file-type path)
                (cond
                  ((string-contains? line "symbolic link") 'symlink)
                  ((string-contains? line "directory") 'directory)
                  ((string-contains? line "regular") 'regular)
                  (else 'other)))))))))

  (def (string-contains? str sub)
    (let ((slen (string-length str))
          (sublen (string-length sub)))
      (if (> sublen slen) #f
        (let loop ((i 0))
          (cond
            ((> (+ i sublen) slen) #f)
            ((string=? (substring str i (+ i sublen)) sub) #t)
            (else (loop (+ i 1))))))))

  (def (shell-quote str)
    (string-append "'" (let loop ((i 0) (acc '()))
      (if (>= i (string-length str))
        (list->string (reverse acc))
        (let ((c (string-ref str i)))
          (if (eqv? c #\')
            (loop (+ i 1) (append (reverse (string->list "'\\''")) acc))
            (loop (+ i 1) (cons c acc)))))) "'"))

  (def (read-symlink path)
    (with-catch
      (lambda (e) #f)
      (lambda ()
        (let ((cmd (string-append "readlink " (shell-quote path) " 2>/dev/null")))
          (let-values (((to-stdin from-stdout from-stderr pid)
                        (open-process-ports cmd (buffer-mode block) (native-transcoder))))
            (close-port to-stdin)
            (let ((line (get-line from-stdout)))
              (close-port from-stdout)
              (close-port from-stderr)
              (if (or (not line) (eof-object? line)) #f line)))))))

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
      (let ((buf (make-bytevector 65536)))
        (let loop ()
          (let ((n (get-bytevector-n! in buf 0 65536)))
            (unless (eof-object? n)
              (put-bytevector out buf 0 n)
              (loop)))))
      (close-port in)
      (close-port out)))

  ;; Confirm overwrite
  (def (confirm-overwrite dst)
    (eprintf "cp: overwrite '~a'? " dst)
    (let ((resp (get-line (current-input-port))))
      (and (string? resp)
           (> (string-length resp) 0)
           (or (eqv? (string-ref resp 0) #\y)
               (eqv? (string-ref resp 0) #\Y)))))

  ;; Preserve mode and timestamps via cp --preserve
  (def (preserve-attributes src dst)
    (with-catch
      (lambda (e) #t)
      (lambda ()
        (let ((cmd (string-append "chmod --reference=" (shell-quote src) " " (shell-quote dst)
                     " 2>/dev/null; touch --reference=" (shell-quote src) " " (shell-quote dst)
                     " 2>/dev/null")))
          (let-values (((to-stdin from-stdout from-stderr pid)
                        (open-process-ports cmd (buffer-mode block) (native-transcoder))))
            (close-port to-stdin)
            (close-port from-stdout)
            (close-port from-stderr))))))

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
                (copy-one s d force interactive verbose preserve #f #f #t)))
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
