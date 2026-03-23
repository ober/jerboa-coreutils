#!chezscheme
;;; tail.sls -- Print last N lines/bytes of files

(library (jerboa-coreutils tail)
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
          (jerboa-coreutils common version)
          (jerboa-coreutils common security))

  (def (main . args)
    (parameterize ((program-name "tail"))
      (init-security!)
      (install-readonly-seccomp!)
      (call-with-getopt
        (lambda (_ opt)
          (let ((files (hash-ref opt 'rest))
                (count-str (or (hash-get opt 'lines) "10"))
                (byte-count (hash-get opt 'bytes))
                (quiet? (hash-get opt 'quiet))
                (verbose? (hash-get opt 'verbose))
                (follow? (hash-get opt 'follow)))
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
                      (tail-bytes file (parse-tail-count byte-count))
                      (tail-lines file (parse-tail-count count-str))))
                  files)
                ;; Follow mode: poll the last file for new content
                (when (and follow? (not (null? files)))
                  (let ((file (car (reverse files))))
                    (unless (equal? file "-")
                      (with-catch
                        (lambda (e) (void))
                        (lambda ()
                          (let ((port (open-input-file file)))
                            ;; Seek to end by reading everything
                            (let drain ()
                              (let ((c (read-char port)))
                                (unless (eof-object? c)
                                  (drain))))
                            ;; Poll loop
                            (let loop ()
                              (thread-sleep! 1)
                              (let read-new ()
                                (let ((c (read-char port)))
                                  (unless (eof-object? c)
                                    (write-char c)
                                    (read-new))))
                              (flush-output-port (current-output-port))
                              (loop)))))))))))
        args
        'program: "tail"
        'help: "Print the last 10 lines of each FILE to standard output."
        (option 'lines "-n" "--lines"
          'help: "output the last NUM lines instead of the last 10"
          'default: #f)
        (option 'bytes "-c" "--bytes"
          'help: "output the last NUM bytes"
          'default: #f)
        (flag 'follow "-f" "--follow"
          'help: "output appended data as the file grows")
        (flag 'quiet "-q" "--quiet"
          'help: "never output headers giving file names")
        (flag 'verbose "-v" "--verbose"
          'help: "always output headers giving file names")
        (rest-arguments 'rest))))

  (def (parse-tail-count str)
    ;; +N means start from line N (1-indexed)
    ;; -N or N means last N lines
    (cond
      ((and (> (string-length str) 0)
            (eqv? (string-ref str 0) #\+))
       (let ((n (string->number (substring str 1 (string-length str)))))
         (if n (cons 'from n)
           (begin (die "invalid number of lines: '~a'" str) (cons 'last 10)))))
      (else
        (let* ((s (if (and (> (string-length str) 0)
                           (eqv? (string-ref str 0) #\-))
                    (substring str 1 (string-length str))
                    str))
               (n (string->number s)))
          (if n (cons 'last n)
            (begin (die "invalid number of lines: '~a'" str) (cons 'last 10)))))))

  (def (tail-lines file spec)
    (let ((proc
      (lambda (port)
        (case (car spec)
          ((from)
           ;; Start from line N (1-indexed), char-by-char to preserve trailing newline
           (let ((start (cdr spec)))
             ;; Skip lines by counting newlines
             (let skip ((line-num 1))
               (when (< line-num start)
                 (let ((c (read-char port)))
                   (unless (eof-object? c)
                     (if (eqv? c #\newline)
                       (skip (+ line-num 1))
                       (skip line-num))))))
             ;; Output the rest char by char
             (let loop ()
               (let ((c (read-char port)))
                 (unless (eof-object? c)
                   (write-char c)
                   (loop))))))
          ((last)
           ;; Last N lines - use a circular buffer of (content . has-newline?) pairs
           (let* ((n (cdr spec))
                  (buf (make-vector n #f))
                  (count 0))
             ;; Read all lines preserving newline info
             (let loop-read ()
               (let ((entry (read-line-raw port)))
                 (when entry
                   (vector-set! buf (modulo count n) entry)
                   (set! count (+ count 1))
                   (loop-read))))
             ;; Output
             (let* ((total (min count n))
                    (start-idx (if (< count n) 0 (modulo count n))))
               (let loop ((i 0))
                 (when (< i total)
                   (let ((entry (vector-ref buf (modulo (+ start-idx i) n))))
                     (display (car entry))
                     (when (cdr entry) (newline)))
                   (loop (+ i 1)))))))))))
      (if (equal? file "-")
        (proc (current-input-port))
        (with-catch
          (lambda (e) (warn "cannot open '~a' for reading: No such file or directory" file))
          (lambda ()
            (let ((port (open-input-file file)))
              (try (proc port)
                (finally (close-input-port port)))))))))

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

  (def (tail-bytes file spec)
    (let ((proc
      (lambda (port)
        (let* ((content (read-all-as-string port))
               (len (string-length content)))
          (case (car spec)
            ((from)
             (let ((start (min (- (cdr spec) 1) len)))
               (display (substring content start len))))
            ((last)
             (let ((start (max 0 (- len (cdr spec)))))
               (display (substring content start len)))))))))
      (if (equal? file "-")
        (proc (current-input-port))
        (with-catch
          (lambda (e) (warn "cannot open '~a' for reading: No such file or directory" file))
          (lambda ()
            (let ((port (open-input-file file)))
              (try (proc port)
                (finally (close-input-port port)))))))))

  ) ;; end library
