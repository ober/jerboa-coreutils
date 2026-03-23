#!chezscheme
;;; wc.sls -- Count lines, words, bytes, chars

(library (jerboa-coreutils wc)
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
    (parameterize ((program-name "wc"))
      (init-security!)
      (install-readonly-seccomp!)
      (call-with-getopt
        (lambda (_ opt)
          (let* ((files (hash-ref opt 'rest))
                 (files (if (null? files) '("-") files))
                 (show-lines? (hash-get opt 'lines))
                 (show-words? (hash-get opt 'words))
                 (show-bytes? (hash-get opt 'bytes))
                 (show-chars? (hash-get opt 'chars))
                 (show-max-line? (hash-get opt 'max-line-length))
                   ;; Default: show lines, words, bytes
                   (default? (not (or show-lines? show-words? show-bytes?
                                      show-chars? show-max-line?)))
                   ;; Count number of visible columns
                   (ncols (+ (if (or default? show-lines?) 1 0)
                             (if (or default? show-words?) 1 0)
                             (if (or default? show-bytes?) 1 0)
                             (if show-chars? 1 0)
                             (if show-max-line? 1 0)))
                   ;; Collect all counts first
                   (all-counts
                     (map (lambda (file)
                            (let-values (((lines words bytes chars max-line) (count-file file)))
                              (list lines words bytes chars max-line file)))
                          files))
                   ;; Compute totals
                   (totals (fold-counts all-counts))
                   ;; Determine if we're stdin-only (single stdin, no filename)
                   (stdin-only? (and (= (length files) 1)
                                     (equal? (car files) "-")))
                   ;; Compute display width
                   (width (compute-width all-counts totals stdin-only? ncols
                                         show-lines? show-words? show-bytes?
                                         show-chars? show-max-line? default?)))
              ;; Print each file
              (for-each
                (lambda (entry)
                  (let ((lines (list-ref entry 0))
                        (words (list-ref entry 1))
                        (bytes (list-ref entry 2))
                        (chars (list-ref entry 3))
                        (max-line (list-ref entry 4))
                        (file (list-ref entry 5)))
                    (print-counts lines words bytes chars max-line
                                  show-lines? show-words? show-bytes?
                                  show-chars? show-max-line? default?
                                  width
                                  (if (equal? file "-") "" file))))
                all-counts)
              ;; Print totals if multiple files
              (when (> (length files) 1)
                (print-counts (list-ref totals 0) (list-ref totals 1)
                              (list-ref totals 2) (list-ref totals 3)
                              (list-ref totals 4)
                              show-lines? show-words? show-bytes?
                              show-chars? show-max-line? default?
                              width "total"))))
        args
        'program: "wc"
        'help: "Print newline, word, and byte counts for each FILE."
        (flag 'lines "-l" "--lines" 'help: "print the newline counts")
        (flag 'words "-w" "--words" 'help: "print the word counts")
        (flag 'bytes "-c" "--bytes" 'help: "print the byte counts")
        (flag 'chars "-m" "--chars" 'help: "print the character counts")
        (flag 'max-line-length "-L" "--max-line-length"
          'help: "print the maximum display width")
        (rest-arguments 'rest))))

  (def (fold-counts all-counts)
    (let loop ((rest all-counts) (tl 0) (tw 0) (tb 0) (tc 0) (tm 0))
      (if (null? rest)
        (list tl tw tb tc tm)
        (let ((e (car rest)))
          (loop (cdr rest)
                (+ tl (list-ref e 0))
                (+ tw (list-ref e 1))
                (+ tb (list-ref e 2))
                (+ tc (list-ref e 3))
                (max tm (list-ref e 4)))))))

  (def (compute-width all-counts totals stdin-only? ncols
                       show-lines? show-words? show-bytes?
                       show-chars? show-max-line? default?)
    (if stdin-only?
      ;; Stdin-only: single column = 1, multiple = 7
      (if (= ncols 1) 1 7)
      ;; Files: width = digits of max displayed value (including totals)
      (let ((vals (if (> (length all-counts) 1) totals
                    ;; Single file: use its own values
                    (let ((e (car all-counts)))
                      (list (list-ref e 0) (list-ref e 1) (list-ref e 2)
                            (list-ref e 3) (list-ref e 4))))))
        (let ((max-val 0))
          (when (or default? show-lines?)
            (set! max-val (max max-val (list-ref vals 0))))
          (when (or default? show-words?)
            (set! max-val (max max-val (list-ref vals 1))))
          (when (or default? show-bytes?)
            (set! max-val (max max-val (list-ref vals 2))))
          (when show-chars?
            (set! max-val (max max-val (list-ref vals 3))))
          (when show-max-line?
            (set! max-val (max max-val (list-ref vals 4))))
          (max 1 (num-digits max-val))))))

  (def (num-digits n)
    (if (= n 0) 1
      (let loop ((v n) (d 0))
        (if (<= v 0) d
          (loop (quotient v 10) (+ d 1))))))

  ;; Byte classification for whitespace detection
  (define (ws-byte? b)
    (or (= b 10) (= b 32) (= b 9) (= b 13) (= b 12) (= b 11)))

  ;; Count UTF-8 lead bytes (chars = bytes that aren't continuation bytes 10xxxxxx)
  (define (utf8-lead? b)
    (not (= (fxlogand b #xC0) #x80)))

  ;; Fast block-based counting
  (def (count-file file)
    (let ((proc
      (lambda (port)
        (let ((buf (make-bytevector 65536)))
          (let loop ((lines 0) (words 0) (bytes 0) (chars 0)
                     (max-line 0) (line-len 0) (in-word? #f))
            (let ((n (get-bytevector-n! port buf 0 65536)))
              (if (eof-object? n)
                (values lines words bytes chars (max max-line line-len))
                ;; Scan the block
                (let scan ((i 0) (l lines) (w words) (c chars)
                           (ml max-line) (ll line-len) (iw? in-word?))
                  (if (>= i n)
                    (loop l w (+ bytes n) c ml ll iw?)
                    (let ((b (bytevector-u8-ref buf i)))
                      (let* ((nl? (= b 10))
                             (ws? (ws-byte? b))
                             (word-start? (and (not iw?) (not ws?)))
                             (new-w (if word-start? (+ w 1) w))
                             (new-c (if (utf8-lead? b) (+ c 1) c)))
                        (scan (+ i 1)
                              (if nl? (+ l 1) l)
                              new-w
                              new-c
                              (if nl? (max ml ll) ml)
                              (if nl? 0 (+ ll 1))
                              (not ws?)))))))))))))
      (if (equal? file "-")
        (proc (standard-input-port))
        (with-catch
          (lambda (e)
            (warn "~a: No such file or directory" file)
            (values 0 0 0 0 0))
          (lambda ()
            (let ((port (open-file-input-port file)))
              (try (proc port)
                (finally (close-port port)))))))))

  (def (print-counts lines words bytes chars max-line
                      show-lines? show-words? show-bytes?
                      show-chars? show-max-line? default?
                      width name)
    (let ((first? #t))
      (define (print-col val)
        (if first?
          (begin (display-padded val width) (set! first? #f))
          (begin (display " ") (display-padded val width))))
      (when (or default? show-lines?)   (print-col lines))
      (when (or default? show-words?)   (print-col words))
      (when (or default? show-bytes?)   (print-col bytes))
      (when show-chars?                 (print-col chars))
      (when show-max-line?              (print-col max-line)))
    (when (> (string-length name) 0)
      (display " ")
      (display name))
    (newline))

  (def (display-padded n width)
    (let* ((s (number->string n))
           (pad (- width (string-length s))))
      (let loop ((i 0))
        (when (< i pad) (display " ") (loop (+ i 1))))
      (display s)))

  ) ;; end library
