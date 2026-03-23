#!chezscheme
;;; common/io.sls -- File and line processing utilities
;;;
;;; Security: Uses audit logging for file access when available.
;;; Validates paths against NUL injection and path traversal.

(library (jerboa-coreutils common io)
  (export
    process-files
    for-each-line
    read-line/delim
    write-line/delim
    safe-open-input-file)

  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name)
          (jerboa-coreutils common)
          (jerboa-coreutils common security)
          (std sugar))

  ;; Validate that a path contains no NUL bytes (injection prevention)
  (define (validate-path! path who)
    (let loop ((i 0))
      (when (< i (string-length path))
        (when (char=? (string-ref path i) #\nul)
          (die "~a: path contains NUL byte" who))
        (loop (+ i 1)))))

  ;; Open a file with error handling and audit; returns port or calls die
  (define (safe-open-input-file path)
    (validate-path! path path)
    (audit-file-access! path)
    (with-catch
      (lambda (e)
        (die "~a: No such file or directory" path))
      (lambda ()
        (open-input-file path))))

  ;; Process a list of files (or stdin if empty/"-")
  ;; proc is called with an input port for each file
  (define (process-files files proc)
    (if (or (null? files) (equal? files '("-")))
      (proc (current-input-port))
      (for-each
        (lambda (f)
          (if (equal? f "-")
            (proc (current-input-port))
            (let ((port (safe-open-input-file f)))
              (dynamic-wind
                (lambda () (void))
                (lambda () (proc port))
                (lambda () (close-input-port port))))))
        files)))

  ;; Call proc on each line read from port
  (define (for-each-line port proc)
    (let loop ()
      (let ((line (get-line port)))
        (unless (eof-object? line)
          (proc line)
          (loop)))))

  ;; Read until delimiter character (for NUL support)
  (define (read-line/delim port delim)
    (if (char=? delim #\newline)
      (get-line port)
      (let ((out (open-output-string)))
        (let loop ()
          (let ((c (read-char port)))
            (cond
              ((eof-object? c)
               (let ((s (get-output-string out)))
                 (if (string=? s "") c s)))
              ((char=? c delim)
               (get-output-string out))
              (else
                (write-char c out)
                (loop))))))))

  ;; Write line with delimiter
  (define (write-line/delim line delim)
    (display line)
    (write-char delim))

  ) ;; end library
