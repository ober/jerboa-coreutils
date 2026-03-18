#!chezscheme
;;; cut.sls -- Extract byte/character/field ranges from input

(library (jerboa-coreutils cut)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name)
          (jerboa core)
          (only (std sugar) with-catch try catch finally)
          (only (std format) eprintf format)
          (std cli getopt)
          (only (std misc string) string-split string-contains string-index)
          (jerboa-coreutils common)
          (jerboa-coreutils common version))

  ;; Parse LIST format: N, N-M, N-, -M, comma-separated
  (def (parse-list str)
    (let ((parts (string-split str #\,)))
      (apply append (map parse-range parts))))

  (def (parse-range str)
    (cond
      ((string-contains str "-")
       (let ((idx (string-index str #\-)))
         (let ((before (substring str 0 idx))
               (after (substring str (+ idx 1) (string-length str))))
           (cond
             ((and (string=? before "") (string=? after ""))
              (die "invalid range with no endpoint: ~a" str)
              '())
             ((string=? before "")
              ;; -M means 1 to M
              (let ((m (string->number after)))
                (if m (list (cons 1 m))
                  (begin (die "invalid field value: ~a" after) '()))))
             ((string=? after "")
              ;; N- means N to infinity (use very large number)
              (let ((n (string->number before)))
                (if n (list (cons n 999999))
                  (begin (die "invalid field value: ~a" before) '()))))
             (else
              ;; N-M
              (let ((n (string->number before))
                    (m (string->number after)))
                (if (and n m)
                  (list (cons n m))
                  (begin (die "invalid range: ~a" str) '()))))))))
      (else
       ;; Single number
       (let ((n (string->number str)))
         (if n (list (cons n n))
           (begin (die "invalid field value: ~a" str) '()))))))

  (def (in-ranges? pos ranges complement?)
    (let ((result (any-range? pos ranges)))
      (if complement? (not result) result)))

  (def (any-range? pos ranges)
    (cond
      ((null? ranges) #f)
      ((and (>= pos (car (car ranges))) (<= pos (cdr (car ranges)))) #t)
      (else (any-range? pos (cdr ranges)))))

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

  (def (cut-bytes-chars line ranges complement?)
    (let ((len (string-length line)))
      (let loop ((i 0))
        (when (< i len)
          (when (in-ranges? (+ i 1) ranges complement?)
            (write-char (string-ref line i)))
          (loop (+ i 1))))))

  (def (cut-fields line ranges delim only-delimited? out-delim complement? zero-term?)
    (if (not (string-index line delim))
      ;; No delimiter found
      (unless only-delimited?
        (display line)
        (display (if zero-term? (string #\nul) "\n")))
      ;; Split on delimiter
      (let* ((fields (string-split line delim))
             (od (or out-delim (string delim)))
             (nfields (length fields))
             (selected (let loop ((i 1) (acc '()))
                         (if (> i nfields) (reverse acc)
                           (if (in-ranges? i ranges complement?)
                             (loop (+ i 1) (cons (list-ref fields (- i 1)) acc))
                             (loop (+ i 1) acc))))))
        (let loop ((sel selected) (first? #t))
          (unless (null? sel)
            (unless first? (display od))
            (display (car sel))
            (loop (cdr sel) #f)))
        (display (if zero-term? (string #\nul) "\n")))))

  (def (cut-file file mode ranges delim only-delimited? out-delim complement? zero-term?)
    (let ((proc (lambda (port)
                  (let ((line-delim (if zero-term? #\nul #\newline)))
                    (let loop ()
                      (let ((line (read-line/delim port line-delim)))
                        (unless (eof-object? line)
                          (case mode
                            ((bytes chars)
                             (cut-bytes-chars line ranges complement?)
                             (display (if zero-term? (string #\nul) "\n")))
                            ((fields)
                             (cut-fields line ranges delim only-delimited?
                                         out-delim complement? zero-term?)))
                          (loop))))))))
      (if (equal? file "-")
        (proc (current-input-port))
        (with-catch
          (lambda (e) (warn "~a: No such file or directory" file))
          (lambda ()
            (let ((port (open-input-file file)))
              (try (proc port)
                (finally (close-input-port port)))))))))

  (def (main . args)
    (parameterize ((program-name "cut"))
      ;; Pre-process --key=value into --key value
      (let ((args (split-long-opts args)))
      (call-with-getopt
        (lambda (_ opt)
          (let ((files (hash-ref opt 'rest))
                (bytes-list (hash-get opt 'bytes))
                (chars-list (hash-get opt 'characters))
                (fields-list (hash-get opt 'fields))
                (delim (if (hash-get opt 'delimiter)
                         (string-ref (hash-get opt 'delimiter) 0)
                         #\tab))
                (only-delimited? (hash-get opt 'only-delimited))
                (out-delim (hash-get opt 'output-delimiter))
                (complement? (hash-get opt 'complement))
                (zero-term? (hash-get opt 'zero-terminated)))
              ;; Exactly one of -b/-c/-f required
              (let ((mode-count (+ (if bytes-list 1 0)
                                   (if chars-list 1 0)
                                   (if fields-list 1 0))))
                (when (= mode-count 0)
                  (die "you must specify a list of bytes, characters, or fields"))
                (when (> mode-count 1)
                  (die "only one type of list may be specified")))
              (let* ((list-str (or bytes-list chars-list fields-list))
                     (ranges (parse-list list-str))
                     (mode (cond (bytes-list 'bytes)
                                 (chars-list 'chars)
                                 (else 'fields)))
                     (files (if (null? files) '("-") files)))
                (for-each
                  (lambda (file)
                    (cut-file file mode ranges delim only-delimited?
                              out-delim complement? zero-term?))
                  files))))
        args
        'program: "cut"
        'help: "Print selected parts of lines from each FILE to standard output."
        (option 'bytes "-b" "--bytes"
          'help: "select only these bytes" 'default: #f)
        (option 'characters "-c" "--characters"
          'help: "select only these characters" 'default: #f)
        (option 'fields "-f" "--fields"
          'help: "select only these fields" 'default: #f)
        (option 'delimiter "-d" "--delimiter"
          'help: "use DELIM instead of TAB for field delimiter" 'default: #f)
        (flag 'only-delimited "-s" "--only-delimited"
          'help: "do not print lines not containing delimiters")
        (option 'output-delimiter "--output-delimiter" "--output-delimiter"
          'help: "use STRING as the output delimiter" 'default: #f)
        (flag 'complement "--complement" "--complement"
          'help: "complement the set of selected bytes, characters or fields")
        (flag 'zero-terminated "-z" "--zero-terminated"
          'help: "line delimiter is NUL, not newline")
        (rest-arguments 'rest)))))

  ) ;; end library
