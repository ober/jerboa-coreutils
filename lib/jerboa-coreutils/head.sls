#!chezscheme
;;; head.sls -- Print first N lines/bytes of files

(library (jerboa-coreutils head)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name
            with-input-from-string with-output-to-string)
          (jerboa core)
          (only (std sugar) with-catch try catch finally)
          (only (std format) eprintf format)
          (std cli getopt)
          (std misc ports)
          (jerboa-coreutils common)
          (jerboa-coreutils common version))

  (define exit-status 0)

  (def (main . args)
    (parameterize ((program-name "head"))
      (call-with-getopt
        (lambda (_ opt)
            (let ((files (hash-ref opt 'rest))
                  (count (or (hash-get opt 'lines) "10"))
                  (byte-count (hash-get opt 'bytes))
                  (quiet? (hash-get opt 'quiet))
                  (verbose? (hash-get opt 'verbose)))
              (let* ((files (if (null? files) '("-") files))
                     (multi? (> (length files) 1))
                     (first-file? #t))
                (for-each
                  (lambda (file)
                    (when (or verbose? (and multi? (not quiet?)))
                      (unless first-file? (newline))
                      (displayln "==> " (if (equal? file "-") "standard input" file) " <=="))
                    (set! first-file? #f)
                    (if byte-count
                      (head-bytes file (parse-head-count byte-count))
                      (head-lines file (parse-head-count count))))
                  files))))
        args
        'program: "head"
        'help: "Print the first 10 lines of each FILE to standard output."
        (option 'lines "-n" "--lines"
          'help: "print the first NUM lines instead of the first 10"
          'default: #f)
        (option 'bytes "-c" "--bytes"
          'help: "print the first NUM bytes"
          'default: #f)
        (flag 'quiet "-q" "--quiet"
          'help: "never print headers giving file names")
        (flag 'verbose "-v" "--verbose"
          'help: "always print headers giving file names")
        (rest-arguments 'rest))
      (unless (= exit-status 0) (exit exit-status))))

  (def (parse-head-count str)
    (let* ((s (string->number str)))
      (if s s
        (begin (die "invalid number of lines: '~a'" str) 0))))

  ;; Fast line-counting head using block reads
  (def (head-lines file n)
    (let ((proc
      (lambda (port)
        (if (>= n 0)
          ;; Positive: output first n lines using block I/O
          (let ((buf (make-bytevector 65536))
                (out (standard-output-port)))
            (let loop ((remaining n))
              (when (> remaining 0)
                (let ((got (get-bytevector-n! port buf 0 65536)))
                  (unless (eof-object? got)
                    ;; Scan for newlines in this block
                    (let scan ((i 0) (rem remaining))
                      (cond
                        ((<= rem 0)
                         ;; Done — already wrote enough
                         (void))
                        ((>= i got)
                         ;; Exhausted this block, need more
                         (put-bytevector out buf 0 got)
                         (loop rem))
                        ((= (bytevector-u8-ref buf i) 10) ;; newline
                         (if (= rem 1)
                           ;; This is the last newline we need — write up to here and stop
                           (begin (put-bytevector out buf 0 (+ i 1))
                                  (flush-output-port out))
                           (scan (+ i 1) (- rem 1))))
                        (else (scan (+ i 1) rem)))))))))
          ;; Negative: output all but last |n| lines (keep existing approach)
          (let* ((skip (- n))
                 (all-lines (read-all-lines-raw port))
                 (total (length all-lines))
                 (to-print (max 0 (- total skip))))
            (let loop ((rest all-lines) (i 0))
              (when (and (pair? rest) (< i to-print))
                (display (car (car rest)))
                (when (cdr (car rest)) (newline))
                (loop (cdr rest) (+ i 1)))))))))
      (if (equal? file "-")
        (proc (standard-input-port))
        (with-catch
          (lambda (e)
            (warn "cannot open '~a' for reading: No such file or directory" file)
            (set! exit-status 1))
          (lambda ()
            (let ((port (open-file-input-port file)))
              (try (proc port)
                (finally (close-port port)))))))))

  ;; Read all lines preserving trailing newline info
  ;; Returns list of (content . has-newline?)
  (def (read-all-lines-raw port)
    (let loop ((acc '()))
      (let ((line (read-line-raw port)))
        (if (not line) (reverse acc)
          (loop (cons line acc))))))

  ;; Read a line as (content . has-newline?), or #f at EOF
  (def (read-line-raw port)
    (let ((c (read-char port)))
      (if (eof-object? c) #f
        (let loop ((buf (open-output-string)))
          (cond
            ((eof-object? c)
             (let ((s (get-output-string buf)))
               (if (= (string-length s) 0) #f
                 (cons s #f))))
            ((eqv? c #\newline)
             (cons (get-output-string buf) #t))
            (else
             (write-char c buf)
             (set! c (read-char port))
             (loop buf)))))))

  ;; Fast byte-count head using block I/O
  (def (head-bytes file n)
    (let ((proc
      (lambda (port)
        (if (>= n 0)
          ;; Positive: output first n bytes
          (let ((buf (make-bytevector 65536))
                (out (standard-output-port)))
            (let loop ((remaining n))
              (when (> remaining 0)
                (let* ((to-read (min remaining 65536))
                       (got (get-bytevector-n! port buf 0 to-read)))
                  (unless (eof-object? got)
                    (put-bytevector out buf 0 got)
                    (loop (- remaining got))))))
            (flush-output-port out))
          ;; Negative: output all but last |n| bytes
          (let* ((content (read-all-as-string port))
                 (len (string-length content))
                 (to-print (max 0 (- len (- n)))))
            (display (substring content 0 to-print)))))))
      (if (equal? file "-")
        (proc (standard-input-port))
        (with-catch
          (lambda (e)
            (warn "cannot open '~a' for reading: No such file or directory" file)
            (set! exit-status 1))
          (lambda ()
            (let ((port (open-file-input-port file)))
              (try (proc port)
                (finally (close-port port)))))))))

  ) ;; end library
