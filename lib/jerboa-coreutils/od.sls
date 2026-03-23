#!chezscheme
;;; od.sls -- Dump files in octal and other formats

(library (jerboa-coreutils od)
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

  ;; Format an address in the given radix
  (def (format-address addr radix)
    (cond
      ((eqv? radix #\o) (pad-string (number->string addr 8) 7 #\0))
      ((eqv? radix #\x) (pad-string (number->string addr 16) 6 #\0))
      ((eqv? radix #\d) (pad-string (number->string addr 10) 7 #\0))
      ((eqv? radix #\n) "")
      (else (pad-string (number->string addr 8) 7 #\0))))

  ;; Pad string on the left
  (def (pad-string str width ch)
    (let loop ((s str))
      (if (>= (string-length s) width) s
        (loop (string-append (string ch) s)))))

  ;; Parse type spec
  (def (parse-type-spec spec)
    (let ((len (string-length spec)))
      (if (= len 0)
        (cons #\o 2)
        (let ((tc (string-ref spec 0)))
          (if (and (> len 1) (char-numeric? (string-ref spec 1)))
            (cons tc (- (char->integer (string-ref spec 1)) (char->integer #\0)))
            (cond
              ((eqv? tc #\c) (cons #\c 1))
              ((eqv? tc #\x) (cons #\x 1))
              (else (cons tc 2))))))))

  (def (format-byte-octal b)
    (pad-string (number->string b 8) 3 #\0))

  (def (format-byte-hex b)
    (pad-string (number->string b 16) 2 #\0))

  (def (format-byte-decimal b)
    (pad-string (number->string b 10) 3 #\space))

  (def (format-byte-char b)
    (cond
      ((= b 0)                          "  \\0")
      ((= b (char->integer #\alarm))    "  \\a")
      ((= b (char->integer #\backspace)) "  \\b")
      ((= b (char->integer #\tab))      "  \\t")
      ((= b (char->integer #\newline))  "  \\n")
      ((= b (char->integer #\return))   "  \\r")
      ((= b #x0b)                       "  \\v")
      ((= b #x0c)                       "  \\f")
      ((and (>= b 32) (< b 127))
       (string-append "   " (string (integer->char b))))
      (else
       (string-append " " (pad-string (number->string b 8) 3 #\0)))))

  (def (format-word-octal b0 b1)
    (let ((val (+ b0 (* b1 256))))
      (string-append " " (pad-string (number->string val 8) 6 #\0))))

  (def (format-word-hex b0 b1)
    (let ((val (+ b0 (* b1 256))))
      (string-append " " (pad-string (number->string val 16) 4 #\0))))

  (def (format-word-decimal b0 b1)
    (let ((val (+ b0 (* b1 256))))
      (string-append " " (pad-string (number->string val 10) 5 #\space))))

  (def (format-bytes bytes type-spec)
    (let* ((parsed (parse-type-spec type-spec))
           (type-char (car parsed))
           (group-size (cdr parsed))
           (out (open-output-string)))
      (if (= group-size 1)
        (for-each
          (lambda (b)
            (cond
              ((eqv? type-char #\o) (display (string-append " " (format-byte-octal b)) out))
              ((eqv? type-char #\x) (display (string-append " " (format-byte-hex b)) out))
              ((eqv? type-char #\d) (display (string-append " " (format-byte-decimal b)) out))
              ((eqv? type-char #\c) (display (format-byte-char b) out))
              (else (display (string-append " " (format-byte-octal b)) out))))
          bytes)
        (let loop ((bs bytes))
          (unless (null? bs)
            (let ((b0 (car bs))
                  (b1 (if (pair? (cdr bs)) (cadr bs) 0)))
              (cond
                ((eqv? type-char #\o) (display (format-word-octal b0 b1) out))
                ((eqv? type-char #\x) (display (format-word-hex b0 b1) out))
                ((eqv? type-char #\d) (display (format-word-decimal b0 b1) out))
                (else (display (format-word-octal b0 b1) out)))
              (loop (if (pair? (cdr bs)) (cddr bs) '()))))))
      (get-output-string out)))

  (def (read-n-bytes port n)
    (let loop ((i 0) (acc '()))
      (if (>= i n)
        (reverse acc)
        (let ((b (get-u8 port)))
          (if (eof-object? b)
            (reverse acc)
            (loop (+ i 1) (cons b acc)))))))

  (def (od-dump port addr-radix type-spec skip-bytes limit-bytes)
    (let ((type (or type-spec "o")))
      ;; Skip bytes
      (when (and skip-bytes (> skip-bytes 0))
        (let loop ((n skip-bytes))
          (when (> n 0)
            (let ((b (get-u8 port)))
              (unless (eof-object? b)
                (loop (- n 1)))))))
      ;; Dump
      (let ((bytes-per-line 16)
            (total-read 0))
        (let loop ((offset (or skip-bytes 0)))
          (let* ((to-read (if limit-bytes
                             (min bytes-per-line (- limit-bytes total-read))
                             bytes-per-line))
                 (bytes (if (and limit-bytes (<= to-read 0))
                          '()
                          (read-n-bytes port to-read))))
            (unless (null? bytes)
              (let ((addr-str (format-address offset addr-radix))
                    (data-str (format-bytes bytes type)))
                (if (string=? addr-str "")
                  (displayln data-str)
                  (displayln addr-str data-str)))
              (let ((n (length bytes)))
                (set! total-read (+ total-read n))
                (when (and (= n bytes-per-line)
                           (or (not limit-bytes) (< total-read limit-bytes)))
                  (loop (+ offset n)))))))
        ;; Print final address
        (unless (eqv? addr-radix #\n)
          (displayln (format-address (+ (or skip-bytes 0) total-read) addr-radix))))))

  (def (parse-skip str)
    (if (not str) #f
      (let ((n (string->number str)))
        (if n (inexact->exact n) 0))))

  (def (parse-limit str)
    (if (not str) #f
      (let ((n (string->number str)))
        (if n (inexact->exact n) #f))))

  ;; Expand traditional od short flags to long-form equivalents
  ;; -x => -t x2, -c => -t c, -o => -t o2, -d => -t d2
  (def (expand-od-args args)
    (let loop ((rest args) (acc '()))
      (if (null? rest)
        (reverse acc)
        (let ((a (car rest)))
          (cond
            ((string=? a "-x") (loop (cdr rest) (append (reverse '("-t" "x2")) acc)))
            ((string=? a "-c") (loop (cdr rest) (append (reverse '("-t" "c"))  acc)))
            ((string=? a "-o") (loop (cdr rest) (append (reverse '("-t" "o2")) acc)))
            ((string=? a "-d") (loop (cdr rest) (append (reverse '("-t" "d2")) acc)))
            (else (loop (cdr rest) (cons a acc))))))))

  (def (main . args)
    (parameterize ((program-name "od"))
      (init-security!)
      (install-readonly-seccomp!)
      (let ((args (expand-od-args args)))
      (call-with-getopt
        (lambda (_ opt)
            (let* ((addr-radix (if (hash-get opt 'address-radix)
                                 (string-ref (hash-ref opt 'address-radix) 0)
                                 #\o))
                   (type-spec (or (hash-get opt 'type) "o"))
                   (skip (parse-skip (hash-get opt 'skip)))
                   (limit (parse-limit (hash-get opt 'read-bytes)))
                   (files (hash-ref opt 'rest))
                   (port (if (or (null? files) (string=? (car files) "-"))
                           (standard-input-port)
                           (with-catch
                             (lambda (e) (die "~a: ~a" (car files) (error-message e)))
                             (lambda () (open-file-input-port (car files)))))))
              (od-dump port addr-radix type-spec skip limit)
              (unless (or (null? files) (string=? (car files) "-"))
                (close-port port))))
        args
        'program: "od"
        'help: "Write an unambiguous representation, octal bytes by default, of FILE to standard output."
        (option 'address-radix "-A" "--address-radix"
          'help: "output format for file offsets: d, o, x, or n for none"
          'default: #f)
        (option 'type "-t" "--format"
          'help: "select output format: o (octal), x (hex), d (decimal), c (char)"
          'default: #f)
        (option 'skip "-j" "--skip-bytes"
          'help: "skip BYTES input bytes first"
          'default: #f)
        (option 'read-bytes "-N" "--read-bytes"
          'help: "limit dump to BYTES input bytes"
          'default: #f)
        (rest-arguments 'rest)))))

  ) ;; end library
