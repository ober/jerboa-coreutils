#!chezscheme
;;; comm.sls -- Compare two sorted files line by line

(library (jerboa-coreutils comm)
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

  (def (main . args)
    (parameterize ((program-name "comm"))
      ;; Pre-process combined flags: -12 -> -1 -2, -23 -> -2 -3, etc.
      ;; Also handle --output-delimiter=X
      (let ((args (split-long-opts (expand-comm-flags args))))
      (call-with-getopt
        (lambda (_ opt)
          (let ((files (hash-ref opt 'rest))
                (suppress-1? (hash-get opt 'suppress-1))
                (suppress-2? (hash-get opt 'suppress-2))
                (suppress-3? (hash-get opt 'suppress-3))
                (zero-term? (hash-get opt 'zero-terminated))
                (out-delim (or (hash-get opt 'output-delimiter) "\t")))
            (when (not (= (length files) 2))
              (die "requires exactly two files"))
            (comm-files (car files) (cadr files)
                        suppress-1? suppress-2? suppress-3?
                        out-delim zero-term?)))
        args
        'program: "comm"
        'help: "Compare two sorted files line by line."
        (flag 'suppress-1 "-1" "-1"
          'help: "suppress column 1 (lines unique to FILE1)")
        (flag 'suppress-2 "-2" "-2"
          'help: "suppress column 2 (lines unique to FILE2)")
        (flag 'suppress-3 "-3" "-3"
          'help: "suppress column 3 (lines that appear in both files)")
        (option 'output-delimiter "--output-delimiter" "--output-delimiter"
          'help: "separate columns with STR" 'default: #f)
        (flag 'zero-terminated "-z" "--zero-terminated"
          'help: "line delimiter is NUL, not newline")
        (rest-arguments 'rest)))))

  (def (open-comm-port file)
    (if (equal? file "-")
      (current-input-port)
      (open-input-file file)))

  ;; Read line with custom delimiter
  (def (read-line/delim port delim)
    (if (eqv? delim #\newline)
      (get-line port)
      (let ((out (open-output-string)))
        (let loop ()
          (let ((c (read-char port)))
            (cond
              ((eof-object? c)
               (let ((s (get-output-string out)))
                 (if (string=? s "") c s)))
              ((eqv? c delim)
               (get-output-string out))
              (else
                (write-char c out)
                (loop))))))))

  (def (comm-files file1 file2 suppress-1? suppress-2? suppress-3? out-delim zero-term?)
    (let ((port1 (open-comm-port file1))
          (port2 (open-comm-port file2))
          (line-delim (if zero-term? #\nul #\newline))
          (eol (if zero-term? "\x0;" "\n")))
      (let loop ((l1 (read-line/delim port1 line-delim))
                 (l2 (read-line/delim port2 line-delim)))
        (cond
          ((and (eof-object? l1) (eof-object? l2))
           'done)
          ((eof-object? l1)
           ;; Rest of file2
           (unless suppress-2?
             (display (col2-prefix suppress-1? out-delim))
             (display l2)
             (display eol))
           (loop l1 (read-line/delim port2 line-delim)))
          ((eof-object? l2)
           ;; Rest of file1
           (unless suppress-1?
             (display l1)
             (display eol))
           (loop (read-line/delim port1 line-delim) l2))
          ((string<? l1 l2)
           (unless suppress-1?
             (display l1)
             (display eol))
           (loop (read-line/delim port1 line-delim) l2))
          ((string<? l2 l1)
           (unless suppress-2?
             (display (col2-prefix suppress-1? out-delim))
             (display l2)
             (display eol))
           (loop l1 (read-line/delim port2 line-delim)))
          (else
           ;; Equal
           (unless suppress-3?
             (display (col3-prefix suppress-1? suppress-2? out-delim))
             (display l1)
             (display eol))
           (loop (read-line/delim port1 line-delim)
                 (read-line/delim port2 line-delim)))))
      (unless (equal? file1 "-") (close-input-port port1))
      (unless (equal? file2 "-") (close-input-port port2))))

  (def (col2-prefix suppress-1? delim)
    (if suppress-1? "" delim))

  (def (col3-prefix suppress-1? suppress-2? delim)
    (string-append
      (if suppress-1? "" delim)
      (if suppress-2? "" delim)))

  ;; Expand combined flags like -12 -> -1 -2, -23 -> -2 -3, -123 -> -1 -2 -3
  (def (expand-comm-flags args)
    (let loop ((rest args) (acc '()))
      (if (null? rest)
        (reverse acc)
        (let ((arg (car rest)))
          (if (and (> (string-length arg) 1)
                   (eqv? (string-ref arg 0) #\-)
                   (not (eqv? (string-ref arg 1) #\-))
                   (comm-digits-only? arg 1))
            ;; Expand: -12 -> -1 -2
            (let expand ((i 1) (expanded acc))
              (if (>= i (string-length arg))
                (loop (cdr rest) expanded)
                (expand (+ i 1)
                        (cons (string-append "-" (string (string-ref arg i)))
                              expanded))))
            (loop (cdr rest) (cons arg acc)))))))

  (def (comm-digits-only? str start)
    (let loop ((i start))
      (cond
        ((>= i (string-length str)) #t)
        ((memv (string-ref str i) '(#\1 #\2 #\3)) (loop (+ i 1)))
        (else #f))))

  ) ;; end library
