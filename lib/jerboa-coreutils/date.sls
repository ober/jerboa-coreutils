#!chezscheme
;;; date.sls -- Display or set date/time (delegates to /bin/date)

(library (jerboa-coreutils date)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name)
          (jerboa core)
          (only (std sugar) with-catch)
          (only (std format) eprintf format)
          (jerboa-coreutils common)
          (jerboa-coreutils common version))

  ;; date delegates entirely to /bin/date for full compatibility
  ;; since date parsing is incredibly complex

  (def (run-date date-args)
    (with-catch
      (lambda (e)
        (die "cannot run /bin/date: ~a" (error-message e)))
      (lambda ()
        (let ((cmd (string-append "/bin/date"
                     (let loop ((args date-args) (acc ""))
                       (if (null? args) acc
                         (loop (cdr args)
                               (string-append acc " " (shell-quote (car args)))))))))
          (let-values (((to-stdin from-stdout from-stderr pid)
                        (open-process-ports cmd (buffer-mode block) (native-transcoder))))
            (close-port to-stdin)
            (let ((output (let loop ((acc '()))
                            (let ((line (get-line from-stdout)))
                              (if (eof-object? line)
                                (reverse acc)
                                (loop (cons line acc)))))))
              (close-port from-stdout)
              (close-port from-stderr)
              (for-each displayln output)))))))

  (def (main . args)
    (parameterize ((program-name "date"))
      ;; We use a simple manual arg parser to pass args directly to /bin/date
      (let loop ((rest args) (date-args '()))
        (cond
          ((null? rest)
           (run-date (reverse date-args)))
          (else
           (let ((arg (car rest)))
             (cond
               ((string=? arg "--help")
                (displayln "Usage: date [OPTION]... [+FORMAT]")
                (displayln "  or:  date [-u|--utc|--universal] [MMDDhhmm[[CC]YY][.ss]]")
                (displayln "Display the current time in the given FORMAT, or set the system date.")
                (displayln "")
                (displayln "  -d, --date=STRING         display time described by STRING, not 'now'")
                (displayln "  -f, --file=DATEFILE       like --date; once for each line of DATEFILE")
                (displayln "  -I, --iso-8601[=TIMESPEC] output date/time in ISO 8601 format")
                (displayln "  -R, --rfc-2822            output date and time in RFC 2822 format")
                (displayln "  -r, --reference=FILE      display the last modification time of FILE")
                (displayln "  -s, --set=STRING          set time described by STRING")
                (displayln "  -u, --utc, --universal    print or set Coordinated Universal Time (UTC)")
                (exit 0))
               ((string=? arg "--version")
                (version-info "date")
                (exit 0))
               (else
                (loop (cdr rest) (cons arg date-args))))))))))

  ) ;; end library
