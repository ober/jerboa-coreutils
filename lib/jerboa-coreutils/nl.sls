#!chezscheme
;;; nl.sls -- Number lines

(library (jerboa-coreutils nl)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name)
          (jerboa core)
          (only (std sugar) with-catch try catch finally)
          (only (std format) eprintf format)
          (std cli getopt)
          (jerboa-coreutils common)
          (jerboa-coreutils common version))

  (def (main . args)
    (parameterize ((program-name "nl"))
      (call-with-getopt
        (lambda (_ opt)
            (let ((files (hash-ref opt 'rest))
                  (body-style (or (hash-get opt 'body-numbering) "t"))
                  (width (string->number (or (hash-get opt 'number-width) "6")))
                  (sep (or (hash-get opt 'number-separator) "\t"))
                  (start (string->number (or (hash-get opt 'starting-line-number) "1")))
                  (incr (string->number (or (hash-get opt 'line-increment) "1")))
                  (format-type (or (hash-get opt 'number-format) "rn"))
                  (blank-join (string->number (or (hash-get opt 'join-blank-lines) "1"))))
              (let ((line-num start)
                    (blank-count 0))
                (process-nl-files
                  (if (null? files) '("-") files)
                  (lambda (port)
                    (let loop ()
                      (let ((line (get-line port)))
                        (unless (eof-object? line)
                          (let* ((blank? (string=? line ""))
                                 (number? (should-number? body-style line blank?
                                                           blank-count blank-join)))
                            (if blank? (set! blank-count (+ blank-count 1))
                              (set! blank-count 0))
                            (if number?
                              (begin
                                (display (format-number line-num width format-type))
                                (display sep)
                                (displayln line)
                                (set! line-num (+ line-num incr)))
                              (begin
                                (display (make-string (+ width (string-length sep)) #\space))
                                (displayln line))))
                          (loop)))))))))
        args
        'program: "nl"
        'help: "Write each FILE to standard output, with line numbers added."
        (option 'body-numbering "-b" "--body-numbering"
          'help: "use STYLE for numbering body lines (a/t/n)" 'default: #f)
        (option 'number-width "-w" "--number-width"
          'help: "use NUMBER columns for line numbers" 'default: #f)
        (option 'number-separator "-s" "--number-separator"
          'help: "add STRING after (possible) line number" 'default: #f)
        (option 'starting-line-number "-v" "--starting-line-number"
          'help: "first line number on each logical page" 'default: #f)
        (option 'line-increment "-i" "--line-increment"
          'help: "line number increment at each line" 'default: #f)
        (option 'number-format "-n" "--number-format"
          'help: "insert line numbers according to FORMAT (ln/rn/rz)" 'default: #f)
        (option 'join-blank-lines "-l" "--join-blank-lines"
          'help: "group of NUMBER empty lines counted as one" 'default: #f)
        (rest-arguments 'rest))))

  (def (process-nl-files files proc)
    (for-each
      (lambda (f)
        (if (equal? f "-")
          (proc (current-input-port))
          (with-catch
            (lambda (e) (warn "~a: No such file or directory" f))
            (lambda ()
              (let ((port (open-input-file f)))
                (try (proc port)
                  (finally (close-input-port port))))))))
      files))

  (def (should-number? style line blank? blank-count blank-join)
    (cond
      ((string=? style "a") #t)
      ((string=? style "t")
       (not blank?))
      ((string=? style "n") #f)
      (else (not blank?))))

  (def (format-number n width fmt)
    (let ((s (number->string n)))
      (cond
        ((string=? fmt "rn")
         (string-append (make-string (max 0 (- width (string-length s))) #\space) s))
        ((string=? fmt "ln")
         (string-append s (make-string (max 0 (- width (string-length s))) #\space)))
        ((string=? fmt "rz")
         (string-append (make-string (max 0 (- width (string-length s))) #\0) s))
        (else
         (string-append (make-string (max 0 (- width (string-length s))) #\space) s)))))

  ) ;; end library
