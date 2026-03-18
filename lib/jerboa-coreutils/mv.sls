#!chezscheme
;;; mv.sls -- Move (rename) files

(library (jerboa-coreutils mv)
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

  (define ffi-rename (foreign-procedure "rename" (string string) int))
  (define ffi-unlink (foreign-procedure "unlink" (string) int))
  (define ffi-rmdir (foreign-procedure "rmdir" (string) int))
  (define ffi-chmod (foreign-procedure "chmod" (string int) int))

  (define *exit-code* 0)

  (def (path-basename path)
    (let loop ((i (- (string-length path) 1)))
      (cond
        ((< i 0) path)
        ((eqv? (string-ref path i) #\/)
         (substring path (+ i 1) (string-length path)))
        (else (loop (- i 1))))))

  (def (file-type path)
    ;; -1=not found, 0=file/symlink/other, 1=directory
    (with-catch
      (lambda (e) -1)
      (lambda ()
        (if (file-directory? path) 1
          (if (file-exists? path) 0 -1)))))

  ;; Copy file contents
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

  ;; Copy directory recursively
  (def (copy-dir-recursive src dst)
    (with-catch
      (lambda (e)
        (warn "cannot create directory '~a': ~a" dst (error-message e))
        (set! *exit-code* 1))
      (lambda ()
        (mkdir dst)))
    ;; Copy contents
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
                (let ((ftype (file-type s)))
                  (cond
                    ((= ftype 1)
                     (copy-dir-recursive s d))
                    ((>= ftype 0)
                     (with-catch
                       (lambda (e)
                         (warn "cannot copy '~a' to '~a': ~a" s d (error-message e))
                         (set! *exit-code* 1))
                       (lambda ()
                         (copy-file-data s d))))))))
            entries)))))

  ;; Remove directory recursively
  (def (remove-recursive path)
    (let ((ftype (file-type path)))
      (cond
        ((= ftype 1)
         (with-catch
           (lambda (e) #f)
           (lambda ()
             (let ((entries (directory-list path)))
               (for-each
                 (lambda (name)
                   (remove-recursive (string-append path "/" name)))
                 entries))))
         (ffi-rmdir path))
        ((>= ftype 0)
         (ffi-unlink path)))))

  ;; Confirm overwrite
  (def (confirm-overwrite dst)
    (eprintf "mv: overwrite '~a'? " dst)
    (let ((resp (get-line (current-input-port))))
      (and (string? resp)
           (> (string-length resp) 0)
           (or (eqv? (string-ref resp 0) #\y)
               (eqv? (string-ref resp 0) #\Y)))))

  ;; Move a single source to a destination
  (def (move-one src dst force interactive no-clobber verbose)
    (let ((src-type (file-type src)))
      (when (< src-type 0)
        (warn "cannot stat '~a': No such file or directory" src)
        (set! *exit-code* 1))
      (when (>= src-type 0)
        (let ((dst-exists (>= (file-type dst) 0)))
          (when (and dst-exists no-clobber)
            (set! src-type -1))  ;; skip
          (when (not (and dst-exists no-clobber))
            (when (and dst-exists interactive (not force))
              (unless (confirm-overwrite dst)
                (set! src-type -1)))  ;; skip
            (when (>= src-type 0)
              ;; Try rename first (same filesystem, fast path)
              (let ((rc (ffi-rename src dst)))
                (if (zero? rc)
                  (when verbose
                    (eprintf "renamed '~a' -> '~a'\n" src dst))
                  ;; rename failed - cross-device; do copy + delete
                  (begin
                    (if (= src-type 1)
                      ;; directory: recursive copy then remove
                      (begin
                        (copy-dir-recursive src dst)
                        (remove-recursive src))
                      ;; file: copy then remove
                      (with-catch
                        (lambda (e)
                          (warn "cannot move '~a' to '~a': ~a" src dst (error-message e))
                          (set! *exit-code* 1))
                        (lambda ()
                          (copy-file-data src dst)
                          (ffi-unlink src))))
                    (when verbose
                      (eprintf "'~a' -> '~a'\n" src dst)))))))))))

  (def (main . args)
    (parameterize ((program-name "mv"))
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
                   (dst-type (file-type dst)))
              ;; If multiple sources, destination must be a directory
              (when (and (> (length srcs) 1)
                         (not (= dst-type 1)))
                (die "target '~a' is not a directory" dst))
              (for-each
                (lambda (src)
                  (let ((target (if (= dst-type 1)
                                  (string-append dst "/" (path-basename src))
                                  dst)))
                    (move-one src target (hash-get opt 'force) (hash-get opt 'interactive) (hash-get opt 'no-clobber) (hash-get opt 'verbose))))
                srcs))
            (exit *exit-code*))
        args
        'program: "mv"
        'help: "Rename SOURCE to DEST, or move SOURCE(s) to DIRECTORY."
        (flag 'force "-f" "--force"
          'help: "do not prompt before overwriting")
        (flag 'interactive "-i" "--interactive"
          'help: "prompt before overwrite")
        (flag 'no-clobber "-n" "--no-clobber"
          'help: "do not overwrite an existing file")
        (flag 'verbose "-v" "--verbose"
          'help: "explain what is being done")
        (rest-arguments 'rest))))

  ) ;; end library
