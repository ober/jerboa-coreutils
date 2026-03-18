#!chezscheme
;;; sort.sls -- Full-featured sorting

(library (jerboa-coreutils sort)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name
            sort sort!)
          (jerboa core)
          (only (std sugar) with-catch)
          (only (std format) eprintf format)
          (std cli getopt)
          (only (std misc string) string-trim string-prefix?)
          (std sort)
          (jerboa-coreutils common)
          (jerboa-coreutils common version))

  (def (main . args)
    (parameterize ((program-name "sort"))
      ;; Pre-process: replace -h with --human-numeric-sort (getopt reserves -h for --help)
      (let ((args (map (lambda (a) (if (equal? a "-h") "--human-numeric-sort" a)) args)))
      (call-with-getopt
        (lambda (_ opt)
            (let ((files (hash-ref opt 'rest))
                  (reverse? (hash-get opt 'reverse))
                  (unique? (hash-get opt 'unique))
                  (numeric? (hash-get opt 'numeric))
                  (general-numeric? (hash-get opt 'general-numeric))
                  (human-numeric? (hash-get opt 'human-numeric))
                  (ignore-case? (hash-get opt 'ignore-case))
                  (dictionary? (hash-get opt 'dictionary))
                  (ignore-blanks? (hash-get opt 'ignore-blanks))
                  (ignore-nonprinting? (hash-get opt 'ignore-nonprinting))
                  (month? (hash-get opt 'month))
                  (stable? (hash-get opt 'stable))
                  (check? (hash-get opt 'check))
                  (output (hash-get opt 'output))
                  (separator (hash-get opt 'separator))
                  (key-defs (hash-get opt 'key))
                  (zero-term? (hash-get opt 'zero-terminated)))
              (let ((files (if (null? files) '("-") files))
                    (sep (if separator (string-ref separator 0) #f)))
                (let ((lines (read-all-sort-lines files zero-term?)))
                  (if check?
                    (check-sorted lines reverse? numeric? ignore-case? sep)
                    (let* ((cmp (make-comparator
                                  numeric? general-numeric? human-numeric?
                                  month? dictionary? ignore-case?
                                  ignore-blanks? ignore-nonprinting?
                                  reverse? sep key-defs))
                           (sorted (stable-sort lines cmp))
                           (result (if unique?
                                     (dedup sorted (lambda (a b) (not (cmp a b))))
                                     sorted)))
                      (output-lines result output zero-term?)))))))
        args
        'program: "sort"
        'help: "Sort lines of text files."
        (flag 'reverse "-r" "--reverse"
          'help: "reverse the result of comparisons")
        (flag 'unique "-u" "--unique"
          'help: "output only the first of an equal run")
        (flag 'numeric "-n" "--numeric-sort"
          'help: "compare according to string numerical value")
        (flag 'general-numeric "-g" "--general-numeric-sort"
          'help: "compare according to general numerical value")
        (flag 'human-numeric "--human-numeric-sort" "--human-numeric-sort"
          'help: "compare human readable numbers (e.g., 2K 1G)")
        (flag 'ignore-case "-f" "--ignore-case"
          'help: "fold lower case to upper case characters")
        (flag 'dictionary "-d" "--dictionary-order"
          'help: "consider only blanks and alphanumeric characters")
        (flag 'ignore-blanks "-b" "--ignore-leading-blanks"
          'help: "ignore leading blanks")
        (flag 'ignore-nonprinting "-i" "--ignore-nonprinting"
          'help: "consider only printable characters")
        (flag 'month "-M" "--month-sort"
          'help: "compare (unknown) < 'JAN' < ... < 'DEC'")
        (flag 'stable "-s" "--stable"
          'help: "stabilize sort by disabling last-resort comparison")
        (flag 'check "-c" "--check"
          'help: "check for sorted input; do not sort")
        (option 'output "-o" "--output"
          'help: "write result to FILE instead of standard output" 'default: #f)
        (option 'separator "-t" "--field-separator"
          'help: "use SEP instead of non-blank to blank transition" 'default: #f)
        (option 'key "-k" "--key"
          'help: "sort via a key (KEYDEF)" 'default: #f)
        (flag 'zero-terminated "-z" "--zero-terminated"
          'help: "line delimiter is NUL, not newline")
        (rest-arguments 'rest)))))

  (def (read-all-sort-lines files zero-term?)
    (let ((delim (if zero-term? #\nul #\newline)))
      (apply append
        (map (lambda (file)
               (let ((port (if (equal? file "-")
                             (current-input-port)
                             (open-input-file file))))
                 (let loop ((acc '()))
                   (let ((line (read-line/delim port delim)))
                     (if (eof-object? line)
                       (begin
                         (unless (equal? file "-") (close-input-port port))
                         (reverse acc))
                       (loop (cons line acc)))))))
             files))))

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

  (def months
    '("JAN" "FEB" "MAR" "APR" "MAY" "JUN"
      "JUL" "AUG" "SEP" "OCT" "NOV" "DEC"))

  (def (month-index str)
    (let ((s (string-upcase (string-trim str))))
      (let loop ((ms months) (i 1))
        (cond
          ((null? ms) 0)
          ((string-prefix? (car ms) s) i)
          (else (loop (cdr ms) (+ i 1)))))))

  (def (parse-human-number str)
    (let* ((s (string-trim str))
           (len (string-length s)))
      (if (= len 0) 0
        (let ((last-c (char-upcase (string-ref s (- len 1)))))
          (let ((mult (case last-c
                        ((#\K) 1024)
                        ((#\M) 1048576)
                        ((#\G) 1073741824)
                        ((#\T) 1099511627776)
                        ((#\P) 1125899906842624)
                        ((#\E) 1152921504606846976)
                        (else #f))))
            (if mult
              (let ((num (string->number (substring s 0 (- len 1)))))
                (if num (* num mult) 0))
              (let ((num (string->number s)))
                (if num num 0))))))))

  (def (extract-number str)
    (let* ((s (string-trim str))
           (num (string->number s)))
      (or num 0)))

  (def (extract-general-number str)
    (let* ((s (string-trim str))
           (num (string->number s)))
      (if num (inexact num) 0.0)))

  (def (strip-for-dictionary str)
    (let loop ((i 0) (acc '()))
      (if (>= i (string-length str))
        (list->string (reverse acc))
        (let ((c (string-ref str i)))
          (if (or (char-alphabetic? c) (char-numeric? c)
                  (eqv? c #\space) (eqv? c #\tab))
            (loop (+ i 1) (cons c acc))
            (loop (+ i 1) acc))))))

  (def (strip-nonprinting str)
    (let loop ((i 0) (acc '()))
      (if (>= i (string-length str))
        (list->string (reverse acc))
        (let ((c (string-ref str i)))
          (if (and (>= (char->integer c) 32) (<= (char->integer c) 126))
            (loop (+ i 1) (cons c acc))
            (loop (+ i 1) acc))))))

  (def (make-comparator numeric? general-numeric? human-numeric?
                         month? dictionary? ignore-case?
                         ignore-blanks? ignore-nonprinting?
                         reverse? sep key-defs)
    (lambda (a b)
      (let* ((ka (prepare-key a ignore-blanks? dictionary? ignore-nonprinting? ignore-case?))
             (kb (prepare-key b ignore-blanks? dictionary? ignore-nonprinting? ignore-case?))
             (result (cond
                       (numeric?
                        (< (extract-number ka) (extract-number kb)))
                       (general-numeric?
                        (< (extract-general-number ka) (extract-general-number kb)))
                       (human-numeric?
                        (< (parse-human-number ka) (parse-human-number kb)))
                       (month?
                        (< (month-index ka) (month-index kb)))
                       (else
                        (string<? ka kb)))))
        (if reverse? (not result) result))))

  (def (prepare-key str ignore-blanks? dictionary? ignore-nonprinting? ignore-case?)
    (let* ((s (if ignore-blanks? (string-trim str) str))
           (s (if dictionary? (strip-for-dictionary s) s))
           (s (if ignore-nonprinting? (strip-nonprinting s) s))
           (s (if ignore-case? (string-upcase s) s)))
      s))

  (def (dedup sorted same?)
    (if (null? sorted) '()
      (let loop ((rest (cdr sorted)) (prev (car sorted)) (acc (list (car sorted))))
        (cond
          ((null? rest) (reverse acc))
          ((same? prev (car rest))
           (loop (cdr rest) prev acc))
          (else
           (loop (cdr rest) (car rest) (cons (car rest) acc)))))))

  (def (check-sorted lines reverse? numeric? ignore-case? sep)
    (let loop ((prev #f) (rest lines) (line-num 1))
      (when (pair? rest)
        (let ((cur (car rest)))
          (when (and prev
                     (let* ((a (if ignore-case? (string-upcase prev) prev))
                            (b (if ignore-case? (string-upcase cur) cur)))
                       (if numeric?
                         (if reverse?
                           (< (extract-number a) (extract-number b))
                           (> (extract-number a) (extract-number b)))
                         (if reverse?
                           (string<? a b)
                           (string>? a b)))))
            (die "~a:~a: disorder: ~a" "-" line-num cur))
          (loop cur (cdr rest) (+ line-num 1))))))

  (def (output-lines lines output zero-term?)
    (let ((port (if output (open-output-file output) (current-output-port)))
          (eol (if zero-term? "\x0;" "\n")))
      (for-each (lambda (line)
                  (display line port)
                  (display eol port))
                lines)
      (when output (close-output-port port))))

  ) ;; end library
