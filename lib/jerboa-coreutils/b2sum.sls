#!chezscheme
;;; b2sum.sls -- Compute and check BLAKE2 message digest (delegates to openssl)

(library (jerboa-coreutils b2sum)
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

  (def (string-index-of str ch)
    (let loop ((i 0))
      (cond
        ((>= i (string-length str)) #f)
        ((eqv? (string-ref str i) ch) i)
        (else (loop (+ i 1))))))

  (def (b2sum-file path)
    (with-catch
      (lambda (e)
        (warn "~a: ~a" path (error-message e))
        #f)
      (lambda ()
        (let* ((cmd (if (string=? path "-")
                      "openssl dgst -blake2b512 -r 2>/dev/null"
                      (string-append "openssl dgst -blake2b512 -r " (shell-quote path) " 2>/dev/null")))
               (input-data (if (string=? path "-")
                             (let loop ((acc '()))
                               (let ((byte (get-u8 (standard-input-port))))
                                 (if (eof-object? byte)
                                   (u8-list->bytevector (reverse acc))
                                   (loop (cons byte acc)))))
                             #f)))
          (let-values (((to-stdin from-stdout from-stderr pid)
                        (open-process-ports cmd (buffer-mode block) (native-transcoder))))
            (when input-data
              (let ((len (bytevector-length input-data)))
                (let loop ((i 0))
                  (when (< i len)
                    (put-char to-stdin (integer->char (bytevector-u8-ref input-data i)))
                    (loop (+ i 1))))))
            (close-port to-stdin)
            (let ((line (get-line from-stdout)))
              (close-port from-stdout)
              (close-port from-stderr)
              (when (and line (not (eof-object? line)))
                (let ((space-pos (string-index-of line #\space)))
                  (if space-pos
                    (let ((hex (substring line 0 space-pos))
                          (name (if (string=? path "-") "-" path)))
                      (displayln hex "  " name))
                    (displayln line))))))))))

  ;; Check mode: verify checksums from file
  (def (b2sum-check checkfile)
    (let* ((port (if (string=? checkfile "-")
                   (current-input-port)
                   (with-catch
                     (lambda (e) (die "~a: ~a" checkfile (error-message e)))
                     (lambda () (open-input-file checkfile)))))
           (ok #t))
      (let loop ()
        (let ((line (get-line port)))
          (unless (eof-object? line)
            (unless (string=? line "")
              (let ((parts (split-checksum-line line)))
                (when (and (pair? parts) (pair? (cdr parts)))
                  (let* ((expected (car parts))
                         (filename (cadr parts))
                         (actual (compute-b2-hex filename)))
                    (if (and actual (string=? expected actual))
                      (displayln filename ": OK")
                      (begin
                        (displayln filename ": FAILED")
                        (set! ok #f)))))))
            (loop))))
      (unless (string=? checkfile "-") (close-port port))
      (unless ok (exit 1))))

  ;; Compute b2 hex for a file, return hex string or #f
  (def (compute-b2-hex path)
    (with-catch
      (lambda (e) #f)
      (lambda ()
        (let ((cmd (string-append "openssl dgst -blake2b512 -r " (shell-quote path) " 2>/dev/null")))
          (let-values (((to-stdin from-stdout from-stderr pid)
                        (open-process-ports cmd (buffer-mode block) (native-transcoder))))
            (close-port to-stdin)
            (let ((line (get-line from-stdout)))
              (close-port from-stdout)
              (close-port from-stderr)
              (if (and line (not (eof-object? line)))
                (let ((space-pos (string-index-of line #\space)))
                  (if space-pos
                    (substring line 0 space-pos)
                    #f))
                #f)))))))

  (def (split-checksum-line line)
    (let ((len (string-length line)))
      (let loop ((i 0))
        (cond
          ((>= (+ i 1) len) (list line))
          ((and (eqv? (string-ref line i) #\space)
                (eqv? (string-ref line (+ i 1)) #\space))
           (list (substring line 0 i)
                 (substring line (+ i 2) len)))
          (else (loop (+ i 1)))))))

  (def (main . args)
    (parameterize ((program-name "b2sum"))
      (init-security!)
      (install-readonly-seccomp!)
      (call-with-getopt
        (lambda (_ opt)
          (let ((files (if (null? (hash-ref opt 'rest)) '("-") (hash-ref opt 'rest))))
            (if (hash-get opt 'check)
              (for-each b2sum-check files)
              (for-each b2sum-file files))))
        args
        'program: "b2sum"
        'help: "Print or check BLAKE2 (512-bit) checksums."
        (flag 'check "-c" "--check"
          'help: "read BLAKE2 sums from the FILEs and check them")
        (flag 'tag "--tag"
          'help: "create a BSD-style checksum")
        (rest-arguments 'rest))))

  ) ;; end library
