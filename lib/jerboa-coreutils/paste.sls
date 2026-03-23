#!chezscheme
;;; paste.sls -- Merge lines of files side by side

(library (jerboa-coreutils paste)
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

  (def (main . args)
    (parameterize ((program-name "paste"))
      (init-security!)
      (install-readonly-seccomp!)
      ;; Insert "--" before any bare "-" that getopt would misparse as a flag
      (let ((args (insert-dashdash-for-stdin args)))
      (call-with-getopt
        (lambda (_ opt)
            (let ((files (hash-ref opt 'rest))
                  (delimiters (parse-delimiters (or (hash-get opt 'delimiters) "\t")))
                  (serial? (hash-get opt 'serial))
                  (zero-term? (hash-get opt 'zero-terminated)))
              (let ((files (if (null? files) '("-") files)))
                (if serial?
                  (paste-serial files delimiters zero-term?)
                  (paste-parallel files delimiters zero-term?)))))
        args
        'program: "paste"
        'help: "Merge lines of files."
        (option 'delimiters "-d" "--delimiters"
          'help: "reuse characters from LIST instead of TABs" 'default: #f)
        (flag 'serial "-s" "--serial"
          'help: "paste one file at a time instead of in parallel")
        (flag 'zero-terminated "-z" "--zero-terminated"
          'help: "line delimiter is NUL, not newline")
        (rest-arguments 'rest)))))

  ;; Insert "--" before bare "-" args that getopt would misparse
  (def (insert-dashdash-for-stdin args)
    (let loop ((rest args) (acc '()) (seen-dashdash? #f))
      (if (null? rest)
        (reverse acc)
        (let ((arg (car rest)))
          (cond
            ;; Already seen --, pass everything through
            (seen-dashdash?
             (loop (cdr rest) (cons arg acc) #t))
            ;; -- marker
            ((equal? arg "--")
             (loop (cdr rest) (cons arg acc) #t))
            ;; Bare "-" means stdin -- insert "--" before it
            ((equal? arg "-")
             (loop (cdr rest) (cons arg (cons "--" acc)) #t))
            ;; Known options with argument: skip the next arg
            ((member arg '("-d" "--delimiters"))
             (if (pair? (cdr rest))
               (loop (cddr rest) (cons (cadr rest) (cons arg acc)) #f)
               (loop (cdr rest) (cons arg acc) #f)))
            (else
             (loop (cdr rest) (cons arg acc) #f)))))))

  (def (parse-delimiters str)
    (let ((len (string-length str)))
      (let loop ((i 0) (acc '()))
        (if (>= i len)
          (list->string (reverse acc))
          (let ((c (string-ref str i)))
            (if (and (eqv? c #\\) (< (+ i 1) len))
              (let ((next (string-ref str (+ i 1))))
                (case next
                  ((#\n) (loop (+ i 2) (cons #\newline acc)))
                  ((#\t) (loop (+ i 2) (cons #\tab acc)))
                  ((#\0) (loop (+ i 2) (cons #\nul acc)))
                  ((#\\) (loop (+ i 2) (cons #\\ acc)))
                  (else (loop (+ i 2) (cons next acc)))))
              (loop (+ i 1) (cons c acc))))))))

  (def (open-paste-port file)
    (if (equal? file "-")
      (current-input-port)
      (open-input-file file)))

  (def (paste-parallel files delimiters zero-term?)
    (let* ((ports (map open-paste-port files))
           (ndelims (string-length delimiters))
           (line-delim (if zero-term? #\nul #\newline)))
      (let loop ()
        (let* ((lines (map (lambda (p) (read-line/delim p line-delim)) ports))
               (all-eof? (every-eof? lines)))
          (unless all-eof?
            (let lp ((ls lines) (idx 0) (first? #t))
              (unless (null? ls)
                (unless first?
                  (write-char (string-ref delimiters (modulo (- idx 1) ndelims))))
                (let ((line (car ls)))
                  (unless (eof-object? line)
                    (display line)))
                (lp (cdr ls) (+ idx 1) #f)))
            (display (if zero-term? "\x0;" "\n"))
            (loop))))
      ;; Close non-stdin ports
      (for-each (lambda (f p)
                  (unless (equal? f "-") (close-input-port p)))
                files ports)))

  (def (every-eof? lst)
    (cond
      ((null? lst) #t)
      ((eof-object? (car lst)) (every-eof? (cdr lst)))
      (else #f)))

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

  (def (paste-serial files delimiters zero-term?)
    (let ((ndelims (string-length delimiters))
          (line-delim (if zero-term? #\nul #\newline)))
      (for-each
        (lambda (file)
          (let ((port (open-paste-port file)))
            (let loop ((first? #t) (delim-idx 0))
              (let ((line (read-line/delim port line-delim)))
                (unless (eof-object? line)
                  (unless first?
                    (write-char (string-ref delimiters (modulo delim-idx ndelims))))
                  (display line)
                  (loop #f (+ delim-idx 1)))))
            (display (if zero-term? "\x0;" "\n"))
            (unless (equal? file "-")
              (close-input-port port))))
        files)))

  ) ;; end library
