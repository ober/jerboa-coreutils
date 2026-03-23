#!chezscheme
;;; split.sls -- Split a file into pieces

(library (jerboa-coreutils split)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name
            string-downcase)
          (jerboa core)
          (only (std sugar) with-catch)
          (only (std format) eprintf format)
          (std cli getopt)
          (jerboa-coreutils common)
          (jerboa-coreutils common version)
          (jerboa-coreutils common security))

  ;; Generate suffix: aa, ab, ac, ... az, ba, bb, ...
  (def (make-suffix n numeric)
    (if numeric
      ;; Numeric suffix: 00, 01, 02, ...
      (let loop ((s (number->string n)))
        (if (>= (string-length s) 2) s
          (loop (string-append "0" s))))
      ;; Alphabetic suffix
      (let loop ((n n) (acc '()))
        (let ((ch (integer->char (+ (char->integer #\a) (modulo n 26)))))
          (let ((new-acc (cons ch acc)))
            (let ((q (quotient n 26)))
              (if (= q 0)
                (let ((s (list->string new-acc)))
                  (if (< (length new-acc) 2)
                    (string-append "a" s)
                    s))
                (loop (- q 1) new-acc))))))))

  (def (parse-size str)
    (let* ((s (string-downcase str))
           (len (string-length s)))
      (if (zero? len)
        #f
        (let* ((suffix (string-ref s (- len 1)))
               (num-part (if (char-alphabetic? suffix)
                           (substring s 0 (- len 1))
                           s))
               (n (string->number num-part)))
          (if (not n)
            #f
            (let ((multiplier
                   (if (char-alphabetic? suffix)
                     (cond
                       ((eqv? suffix #\k) 1024)
                       ((eqv? suffix #\m) (* 1024 1024))
                       ((eqv? suffix #\g) (* 1024 1024 1024))
                       (else 1))
                     1)))
              (* (inexact->exact n) multiplier)))))))

  (def (string-downcase str)
    (list->string (map char-downcase (string->list str))))

  (def (open-output-piece prefix suffix-num numeric add-suffix verbose)
    (let* ((suf (make-suffix suffix-num numeric))
           (name (string-append prefix suf (or add-suffix ""))))
      (when verbose
        (eprintf "creating file '~a'\n" name))
      (open-output-file name)))

  (def (split-by-lines port prefix lines-per numeric add-suffix verbose)
    (let loop ((piece-num 0) (line-count 0) (out #f))
      (let ((line (get-line port)))
        (if (eof-object? line)
          (when out (close-port out))
          (let* ((need-new (or (not out) (>= line-count lines-per)))
                 (new-out (if need-new
                            (begin
                              (when out (close-port out))
                              (open-output-piece prefix piece-num numeric add-suffix verbose))
                            out))
                 (new-piece-num (if need-new (+ piece-num 1) piece-num))
                 (new-line-count (if need-new 1 (+ line-count 1))))
            (display line new-out)
            (newline new-out)
            (loop new-piece-num new-line-count new-out))))))

  (def (split-by-bytes port prefix bytes-per numeric add-suffix verbose)
    (let loop ((piece-num 0) (byte-count 0) (out #f))
      (let ((ch (read-char port)))
        (if (eof-object? ch)
          (when out (close-port out))
          (let* ((need-new (or (not out) (>= byte-count bytes-per)))
                 (new-out (if need-new
                            (begin
                              (when out (close-port out))
                              (open-output-piece prefix piece-num numeric add-suffix verbose))
                            out))
                 (new-piece-num (if need-new (+ piece-num 1) piece-num))
                 (new-byte-count (if need-new 1 (+ byte-count 1))))
            (write-char ch new-out)
            (loop new-piece-num new-byte-count new-out))))))

  (def (split-into-chunks port prefix n-chunks numeric add-suffix verbose)
    (let* ((content (let loop ((acc '()))
                      (let ((ch (read-char port)))
                        (if (eof-object? ch)
                          (list->string (reverse acc))
                          (loop (cons ch acc))))))
           (total (string-length content))
           (chunk-size (if (> n-chunks 0) (quotient total n-chunks) total))
           (chunks (let loop ((i 0) (piece 0))
                     (if (>= i total)
                       '()
                       (let* ((end (if (= piece (- n-chunks 1))
                                     total
                                     (min total (+ i chunk-size))))
                              (sub (substring content i end)))
                         (cons sub (loop end (+ piece 1))))))))
      (let write-loop ((i 0) (cs chunks))
        (unless (null? cs)
          (let ((out (open-output-piece prefix i numeric add-suffix verbose)))
            (display (car cs) out)
            (close-port out)
            (write-loop (+ i 1) (cdr cs)))))))

  ;; Pre-process args to protect lone "-" (stdin) from getopt option parsing
  (def (protect-stdin-arg args)
    (let loop ((rest args) (acc '()))
      (cond
        ((null? rest) (reverse acc))
        ((and (string=? (car rest) "-")
              (not (member "--" acc)))
         (append (reverse acc) (list "--" "-") (cdr rest)))
        (else
         (loop (cdr rest) (cons (car rest) acc))))))

  (def (main . args)
    (parameterize ((program-name "split"))
      (init-security!)
      (install-readonly-seccomp!)
      (call-with-getopt
        (lambda (_ opt)
          (let* ((files (hash-ref opt 'rest))
                 (input-file (if (pair? files) (car files) "-"))
                 (prefix (if (and (pair? files) (pair? (cdr files)))
                           (cadr files)
                           "x"))
                 (port (if (or (string=? input-file "-") (string=? input-file ""))
                         (current-input-port)
                         (with-catch
                           (lambda (e) (die "cannot open '~a'" input-file))
                           (lambda () (open-input-file input-file)))))
                 (numeric (hash-get opt 'numeric))
                 (add-suffix (if (hash-get opt 'additional-suffix) (hash-ref opt 'additional-suffix) #f))
                 (verbose (hash-get opt 'verbose)))
            (cond
              ((hash-get opt 'bytes)
               (let ((sz (parse-size (hash-ref opt 'bytes))))
                 (unless sz (die "invalid number of bytes: '~a'" (hash-ref opt 'bytes)))
                 (split-by-bytes port prefix sz numeric add-suffix verbose)))
              ((hash-get opt 'chunks)
               (let ((n (string->number (hash-ref opt 'chunks))))
                 (unless n (die "invalid number of chunks: '~a'" (hash-ref opt 'chunks)))
                 (split-into-chunks port prefix (inexact->exact n) numeric add-suffix verbose)))
              (else
               (let* ((lines-str (or (hash-get opt 'lines) "1000"))
                      (n (string->number lines-str)))
                 (unless n (die "invalid number of lines: '~a'" lines-str))
                 (split-by-lines port prefix (inexact->exact n) numeric add-suffix verbose))))
            (unless (eq? port (current-input-port))
              (close-port port))))
        (protect-stdin-arg args)
        'program: "split"
        'help: "Output pieces of FILE to PREFIXaa, PREFIXab, ...; default size is 1000 lines."
        (option 'lines "-l" "--lines"
          'help: "put NUMBER lines/records per output file"
          'default: #f)
        (option 'bytes "-b" "--bytes"
          'help: "put SIZE bytes per output file"
          'default: #f)
        (option 'chunks "-n" "--number"
          'help: "generate CHUNKS output files"
          'default: #f)
        (flag 'numeric "-d" "--numeric-suffixes"
          'help: "use numeric suffixes instead of alphabetic")
        (option 'additional-suffix "--additional-suffix"
          'help: "append an additional SUFFIX to file names"
          'default: #f)
        (flag 'verbose "--verbose"
          'help: "print a diagnostic just before each output file is opened")
        (rest-arguments 'rest))))

  ) ;; end library
