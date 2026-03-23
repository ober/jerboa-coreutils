#!chezscheme
;;; uniq.sls -- Filter adjacent duplicates

(library (jerboa-coreutils uniq)
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
    (parameterize ((program-name "uniq"))
      (init-security!)
      (install-readonly-seccomp!)
      (call-with-getopt
        (lambda (_ opt)
          (let ((files (hash-ref opt 'rest))
                (count? (hash-get opt 'count))
                (repeated? (hash-get opt 'repeated))
                (unique? (hash-get opt 'unique))
                (ignore-case? (hash-get opt 'ignore-case))
                (skip-fields (if (hash-get opt 'skip-fields) (string->number (hash-get opt 'skip-fields)) 0))
                (skip-chars (if (hash-get opt 'skip-chars) (string->number (hash-get opt 'skip-chars)) 0))
                (check-chars (if (hash-get opt 'check-chars) (string->number (hash-get opt 'check-chars)) #f))
                (zero-term? (hash-get opt 'zero-terminated)))
              (let* ((input (if (or (null? files) (equal? (car files) "-"))
                              "-" (car files)))
                     (output (if (and (pair? files) (pair? (cdr files)))
                               (cadr files) #f)))
                (uniq-process input output count? repeated? unique?
                              ignore-case? skip-fields skip-chars
                              check-chars zero-term?))))
        args
        'program: "uniq"
        'help: "Filter adjacent matching lines from INPUT, writing to OUTPUT."
        (flag 'count "-c" "--count"
          'help: "prefix lines by the number of occurrences")
        (flag 'repeated "-d" "--repeated"
          'help: "only print duplicate lines, one for each group")
        (flag 'unique "-u" "--unique"
          'help: "only print unique lines")
        (option 'skip-fields "-f" "--skip-fields"
          'help: "avoid comparing the first N fields" 'default: #f)
        (option 'skip-chars "-s" "--skip-chars"
          'help: "avoid comparing the first N characters" 'default: #f)
        (option 'check-chars "-w" "--check-chars"
          'help: "compare no more than N characters" 'default: #f)
        (flag 'ignore-case "-i" "--ignore-case"
          'help: "ignore differences in case when comparing")
        (flag 'zero-terminated "-z" "--zero-terminated"
          'help: "line delimiter is NUL, not newline")
        (rest-arguments 'rest))))

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

  (def (uniq-process input output count? repeated? unique?
                      ignore-case? skip-fields skip-chars check-chars zero-term?)
    (let ((in-port (if (equal? input "-")
                     (current-input-port)
                     (open-input-file input)))
          (out-port (if output
                      (open-output-file output)
                      (current-output-port)))
          (line-delim (if zero-term? #\nul #\newline))
          (eol (if zero-term? "\x0;" "\n")))
      (let loop ((prev #f) (count 0))
        (let ((line (read-line/delim in-port line-delim)))
          (cond
            ((eof-object? line)
             ;; Flush last group
             (when prev
               (output-line out-port prev count count? repeated? unique? eol)))
            ((not prev)
             (loop line 1))
            ((lines-equal? prev line ignore-case? skip-fields skip-chars check-chars)
             (loop prev (+ count 1)))
            (else
             (output-line out-port prev count count? repeated? unique? eol)
             (loop line 1)))))
      (when output (close-output-port out-port))
      (unless (equal? input "-") (close-input-port in-port))))

  (def (output-line port line count count? repeated? unique? eol)
    (let ((show? (cond
                   ((and repeated? unique?) #f)
                   (repeated? (> count 1))
                   (unique? (= count 1))
                   (else #t))))
      (when show?
        (when count?
          (display (format "~7d " count) port))
        (display line port)
        (display eol port))))

  (def (lines-equal? a b ignore-case? skip-fields skip-chars check-chars)
    (let* ((a (skip-line a skip-fields skip-chars))
           (b (skip-line b skip-fields skip-chars))
           (a (if check-chars
                (substring a 0 (min check-chars (string-length a)))
                a))
           (b (if check-chars
                (substring b 0 (min check-chars (string-length b)))
                b)))
      (if ignore-case?
        (string-ci=? a b)
        (string=? a b))))

  (def (skip-line line skip-fields skip-chars)
    (let* ((pos 0)
           (len (string-length line))
           ;; Skip fields (sequences of blanks then non-blanks)
           (pos (let loop ((p pos) (fields skip-fields))
                  (if (or (<= fields 0) (>= p len)) p
                    (let ((p (skip-blanks line p len)))
                      (let ((p (skip-non-blanks line p len)))
                        (loop p (- fields 1)))))))
           ;; Skip chars
           (pos (min (+ pos skip-chars) len)))
      (substring line pos len)))

  (def (skip-blanks line pos len)
    (if (and (< pos len)
             (let ((c (string-ref line pos)))
               (or (eqv? c #\space) (eqv? c #\tab))))
      (skip-blanks line (+ pos 1) len)
      pos))

  (def (skip-non-blanks line pos len)
    (if (and (< pos len)
             (let ((c (string-ref line pos)))
               (not (or (eqv? c #\space) (eqv? c #\tab)))))
      (skip-non-blanks line (+ pos 1) len)
      pos))

  ) ;; end library
