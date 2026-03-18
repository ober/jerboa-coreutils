#!chezscheme
;;; join.sls -- Join lines from two sorted files on a common field

(library (jerboa-coreutils join)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name)
          (jerboa core)
          (only (std sugar) with-catch)
          (only (std format) eprintf format)
          (std cli getopt)
          (only (std misc string) string-split)
          (std srfi srfi-13)
          (jerboa-coreutils common)
          (jerboa-coreutils common version))

  (def (main . args)
    (parameterize ((program-name "join"))
      (call-with-getopt
        (lambda (_ opt)
            (let ((files (hash-ref opt 'rest))
                  (field1 (if (hash-get opt 'field1) (string->number (hash-get opt 'field1)) 1))
                  (field2 (if (hash-get opt 'field2) (string->number (hash-get opt 'field2)) 1))
                  (sep (if (hash-get opt 'separator) (string-ref (hash-get opt 'separator) 0) #f))
                  (empty (or (hash-get opt 'empty) ""))
                  (print-unpaired (hash-get opt 'print-unpaired))
                  (ignore-case? (hash-get opt 'ignore-case))
                  (header? (hash-get opt 'header))
                  (zero-term? (hash-get opt 'zero-terminated)))
              (when (not (= (length files) 2))
                (die "requires exactly two file arguments"))
              (join-files (car files) (cadr files)
                          field1 field2 sep empty
                          print-unpaired ignore-case?
                          header? zero-term?)))
        args
        'program: "join"
        'help: "Join lines of two files on a common field."
        (option 'field1 "-1" "-1"
          'help: "join on this FIELD of file 1" 'default: #f)
        (option 'field2 "-2" "-2"
          'help: "join on this FIELD of file 2" 'default: #f)
        (option 'separator "-t" "-t"
          'help: "use CHAR as input and output field separator" 'default: #f)
        (option 'empty "-e" "-e"
          'help: "replace missing input fields with EMPTY" 'default: #f)
        (option 'print-unpaired "-a" "-a"
          'help: "also print unpairable lines from file FILENUM (1 or 2)" 'default: #f)
        (flag 'ignore-case "-i" "--ignore-case"
          'help: "ignore differences in case when comparing fields")
        (flag 'header "--header" "--header"
          'help: "treat the first line in each file as field headers")
        (flag 'zero-terminated "-z" "--zero-terminated"
          'help: "line delimiter is NUL, not newline")
        (rest-arguments 'rest))))

  (def (open-join-port file)
    (if (equal? file "-")
      (current-input-port)
      (open-input-file file)))

  (def (split-fields line sep)
    (if sep
      (string-split line sep)
      ;; Default: split on runs of whitespace
      (let ((tokens (string-tokenize line)))
        (if (null? tokens) '("") tokens))))

  (def (get-field fields n)
    (if (and (>= n 1) (<= n (length fields)))
      (list-ref fields (- n 1))
      ""))

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

  (def (join-files file1 file2 field1 field2 sep empty
                    print-unpaired ignore-case? header? zero-term?)
    (let ((port1 (open-join-port file1))
          (port2 (open-join-port file2))
          (delim (if zero-term? #\nul #\newline))
          (eol (if zero-term? "\x0;" "\n"))
          (out-sep (if sep (string sep) " "))
          (unpaired-1? (and print-unpaired (member print-unpaired '("1" "0"))))
          (unpaired-2? (and print-unpaired (member print-unpaired '("2" "0")))))

      ;; Handle header line
      (when header?
        (let ((h1 (read-line/delim port1 delim))
              (h2 (read-line/delim port2 delim)))
          (unless (or (eof-object? h1) (eof-object? h2))
            (let ((f1 (split-fields h1 sep))
                  (f2 (split-fields h2 sep)))
              (output-joined-line (get-field f1 field1) f1 f2 field1 field2 out-sep empty)
              (display eol)))))

      (let loop ((l1 (read-line/delim port1 delim))
                 (l2 (read-line/delim port2 delim)))
        (cond
          ((and (eof-object? l1) (eof-object? l2))
           'done)
          ((eof-object? l1)
           (when unpaired-2?
             (display l2) (display eol))
           (loop l1 (read-line/delim port2 delim)))
          ((eof-object? l2)
           (when unpaired-1?
             (display l1) (display eol))
           (loop (read-line/delim port1 delim) l2))
          (else
           (let* ((f1 (split-fields l1 sep))
                  (f2 (split-fields l2 sep))
                  (k1 (get-field f1 field1))
                  (k2 (get-field f2 field2))
                  (cmp (if ignore-case?
                         (string-ci<=> k1 k2)
                         (string<=> k1 k2))))
             (cond
               ((< cmp 0)
                (when unpaired-1?
                  (display l1) (display eol))
                (loop (read-line/delim port1 delim) l2))
               ((> cmp 0)
                (when unpaired-2?
                  (display l2) (display eol))
                (loop l1 (read-line/delim port2 delim)))
               (else
                ;; Match - output joined line
                (output-joined-line k1 f1 f2 field1 field2 out-sep empty)
                (display eol)
                (loop (read-line/delim port1 delim)
                      (read-line/delim port2 delim))))))))

      (unless (equal? file1 "-") (close-input-port port1))
      (unless (equal? file2 "-") (close-input-port port2))))

  (def (string<=> a b)
    (cond ((string<? a b) -1)
          ((string>? a b) 1)
          (else 0)))

  (def (string-ci<=> a b)
    (cond ((string-ci<? a b) -1)
          ((string-ci>? a b) 1)
          (else 0)))

  (def (output-joined-line key f1 f2 field1 field2 sep empty)
    (display key)
    ;; Output fields from file1 (except join field)
    (let loop ((i 1) (fields f1))
      (when (pair? fields)
        (unless (= i field1)
          (display sep)
          (let ((v (car fields)))
            (display (if (string=? v "") empty v))))
        (loop (+ i 1) (cdr fields))))
    ;; Output fields from file2 (except join field)
    (let loop ((i 1) (fields f2))
      (when (pair? fields)
        (unless (= i field2)
          (display sep)
          (let ((v (car fields)))
            (display (if (string=? v "") empty v))))
        (loop (+ i 1) (cdr fields)))))

  ) ;; end library
