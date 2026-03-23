#!chezscheme
;;; unexpand.sls -- Convert spaces to tabs

(library (jerboa-coreutils unexpand)
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
          (jerboa-coreutils common version)
          (jerboa-coreutils common security))

  (def (main . args)
    (parameterize ((program-name "unexpand"))
      (init-security!)
      (install-readonly-seccomp!)
      (call-with-getopt
        (lambda (_ opt)
          (let ((files (hash-ref opt 'rest))
                (all? (hash-get opt 'all))
                (first-only? (hash-get opt 'first-only))
                (tabstop (parse-tabstop (or (hash-get opt 'tabs) "8"))))
            (let ((files (if (null? files) '("-") files)))
              (for-each
                (lambda (file)
                  (unexpand-file file (and all? (not first-only?)) tabstop))
                files))))
        args
        'program: "unexpand"
        'help: "Convert blanks in each FILE to tabs, writing to standard output."
        (flag 'all "-a" "--all"
          'help: "convert all blanks, instead of just initial blanks")
        (flag 'first-only "--first-only"
          'help: "convert only leading sequences of blanks")
        (option 'tabs "-t" "--tabs"
          'help: "have tabs N characters apart, not 8" 'default: #f)
        (rest-arguments 'rest))))

  (def (parse-tabstop str)
    (let ((n (string->number str)))
      (if n n 8)))

  (def (unexpand-file file all? tabstop)
    (let ((proc
      (lambda (port)
        (let loop ()
          (let ((line (get-line port)))
            (unless (eof-object? line)
              (displayln (unexpand-line line all? tabstop))
              (loop)))))))
      (if (equal? file "-")
        (proc (current-input-port))
        (with-catch
          (lambda (e) (warn "~a: No such file or directory" file))
          (lambda ()
            (let ((port (open-input-file file)))
              (try (proc port)
                (finally (close-input-port port)))))))))

  (def (unexpand-line line all? tabstop)
    (let ((out (open-output-string))
          (len (string-length line)))
      (let loop ((i 0) (col 0) (space-start #f) (space-col 0) (initial? #t))
        (cond
          ((>= i len)
           ;; Flush any pending spaces
           (when space-start
             (flush-spaces out space-col col tabstop))
           (get-output-string out))
          (else
            (let ((c (string-ref line i)))
              (cond
                ((eqv? c #\space)
                 (if (or initial? all?)
                   (if space-start
                     (loop (+ i 1) (+ col 1) space-start space-col initial?)
                     (loop (+ i 1) (+ col 1) i col initial?))
                   (begin
                     (write-char c out)
                     (loop (+ i 1) (+ col 1) #f 0 #f))))
                ((eqv? c #\tab)
                 (let ((next-col (* (+ (quotient col tabstop) 1) tabstop)))
                   (when space-start
                     (flush-spaces out space-col col tabstop))
                   (write-char #\tab out)
                   (loop (+ i 1) next-col #f 0 initial?)))
                (else
                  (when space-start
                    (flush-spaces out space-col col tabstop))
                  (write-char c out)
                  (loop (+ i 1) (+ col 1) #f 0 #f)))))))))

  (def (flush-spaces out start-col end-col tabstop)
    (let loop ((col start-col))
      (if (>= col end-col)
        (void)
        (let ((next-tab (* (+ (quotient col tabstop) 1) tabstop)))
          (if (<= next-tab end-col)
            (begin (write-char #\tab out) (loop next-tab))
            (begin (write-char #\space out) (loop (+ col 1))))))))

  ) ;; end library
