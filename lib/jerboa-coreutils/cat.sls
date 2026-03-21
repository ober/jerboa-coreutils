#!chezscheme
;;; cat.sls -- Concatenate files to standard output

(library (jerboa-coreutils cat)
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

  (define exit-status 0)

  (def (process-cat-files files proc)
    (for-each
      (lambda (f)
        (if (equal? f "-")
          (proc (current-input-port))
          (with-catch
            (lambda (e)
              (warn "~a: No such file or directory" f)
              (set! exit-status 1))
            (lambda ()
              (let ((port (open-input-file f)))
                (try (proc port)
                  (finally (close-input-port port))))))))
      files))

  (def (display-right-aligned n width)
    (let* ((s (number->string n))
           (pad (- width (string-length s))))
      (let loop ((i 0))
        (when (< i pad) (display " ") (loop (+ i 1))))
      (display s)))

  (def (transform-line line show-tabs? show-nonprinting?)
    (if (and (not show-tabs?) (not show-nonprinting?))
      line
      (let ((out (open-output-string)))
        (let loop ((i 0))
          (if (>= i (string-length line))
            (get-output-string out)
            (let ((c (string-ref line i)))
              (cond
                ((and show-tabs? (eqv? c #\tab))
                 (display "^I" out))
                ((and show-nonprinting? (< (char->integer c) 32) (not (eqv? c #\tab)))
                 (write-char #\^ out)
                 (write-char (integer->char (+ (char->integer c) 64)) out))
                ((and show-nonprinting? (= (char->integer c) 127))
                 (display "^?" out))
                ((and show-nonprinting? (> (char->integer c) 127))
                 (display "M-" out)
                 (let ((c2 (- (char->integer c) 128)))
                   (cond
                     ((< c2 32)
                      (write-char #\^ out)
                      (write-char (integer->char (+ c2 64)) out))
                     ((= c2 127)
                      (display "^?" out))
                     (else
                      (write-char (integer->char c2) out)))))
                (else
                  (write-char c out)))
              (loop (+ i 1))))))))

  ;; Fast binary block copy — no character decoding overhead
  (def (cat-fast port)
    (let ((buf (make-bytevector 65536))
          (out (standard-output-port)))
      (let loop ()
        (let ((n (get-bytevector-n! port buf 0 65536)))
          (unless (eof-object? n)
            (put-bytevector out buf 0 n)
            (loop))))
      (flush-output-port out)))

  ;; Line-oriented cat with flags (get-line instead of read-char)
  (def (cat-lines port number-lines? number-nonblank? squeeze?
                  show-ends? show-tabs? show-nonprinting?)
    (let loop ((line-num 1) (prev-blank? #f))
      (let ((line (get-line port)))
        (unless (eof-object? line)
          (let* ((blank? (string=? line ""))
                 (skip? (and squeeze? prev-blank? blank?)))
            (unless skip?
              (when number-lines?
                (if (and number-nonblank? blank?)
                  (void)
                  (begin
                    (display-right-aligned line-num 6)
                    (display "\t"))))
              (display (transform-line line show-tabs? show-nonprinting?))
              (when show-ends? (display "$"))
              (newline))
            (loop (if (and number-lines? (not skip?)
                          (not (and number-nonblank? blank?)))
                    (+ line-num 1) line-num)
                  blank?))))))

  (def (main . args)
    (parameterize ((program-name "cat"))
      (call-with-getopt
        (lambda (_ opt)
          (let ((files (hash-ref opt 'rest))
                (number-lines? (or (hash-get opt 'number) (hash-get opt 'number-nonblank)))
                (number-nonblank? (hash-get opt 'number-nonblank))
                (squeeze? (hash-get opt 'squeeze-blank))
                (show-ends? (or (hash-get opt 'show-ends) (hash-get opt 'show-all) (hash-get opt 've)))
                (show-tabs? (or (hash-get opt 'show-tabs) (hash-get opt 'show-all) (hash-get opt 'vt)))
                (show-nonprinting? (or (hash-get opt 'show-nonprinting) (hash-get opt 'show-all) (hash-get opt 've) (hash-get opt 'vt))))
            (let ((fancy? (or number-lines? squeeze? show-ends?
                              show-tabs? show-nonprinting?)))
              (process-cat-files
                (if (null? files) '("-") files)
                (lambda (port)
                  (if fancy?
                    (cat-lines port number-lines? number-nonblank? squeeze?
                               show-ends? show-tabs? show-nonprinting?)
                    ;; Plain cat: fast binary block copy
                    (cat-fast (if (equal? port (current-input-port))
                               (standard-input-port)
                               (open-file-input-port
                                 (port-name port))))))))))
        args
        'program: "cat"
        'help: "Concatenate FILE(s) to standard output."
        (flag 'show-all "-A" "--show-all"
          'help: "equivalent to -vET")
        (flag 'number-nonblank "-b" "--number-nonblank"
          'help: "number nonempty output lines, overrides -n")
        (flag 've "-e"
          'help: "equivalent to -vE")
        (flag 'show-ends "-E" "--show-ends"
          'help: "display $ at end of each line")
        (flag 'number "-n" "--number"
          'help: "number all output lines")
        (flag 'squeeze-blank "-s" "--squeeze-blank"
          'help: "suppress repeated empty output lines")
        (flag 'vt "-t"
          'help: "equivalent to -vT")
        (flag 'show-tabs "-T" "--show-tabs"
          'help: "display TAB characters as ^I")
        (flag 'show-nonprinting "-v" "--show-nonprinting"
          'help: "use ^ and M- notation, except for LFD and TAB")
        (rest-arguments 'rest))
      (unless (= exit-status 0) (exit exit-status))))

  ) ;; end library
