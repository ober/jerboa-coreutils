#!chezscheme
;;; md5sum.sls -- MD5 hash

(library (jerboa-coreutils md5sum)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name)
          (jerboa core)
          (only (std sugar) with-catch)
          (only (std format) eprintf format)
          (std cli getopt)
          (std crypto digest)
          (jerboa-coreutils common)
          (jerboa-coreutils common version))

  (def (read-all-bytes port)
    (let loop ((acc '()))
      (let ((byte (get-u8 port)))
        (if (eof-object? byte)
          (u8-list->bytevector (reverse acc))
          (loop (cons byte acc))))))

  (def (bytevector->hex bv)
    (let* ((len (bytevector-length bv))
           (out (open-output-string)))
      (let loop ((i 0))
        (when (< i len)
          (let* ((b (bytevector-u8-ref bv i))
                 (hi (arithmetic-shift b -4))
                 (lo (bitwise-and b #xF)))
            (write-char (integer->char (+ (if (< hi 10) (char->integer #\0) (- (char->integer #\a) 10)) hi)) out)
            (write-char (integer->char (+ (if (< lo 10) (char->integer #\0) (- (char->integer #\a) 10)) lo)) out))
          (loop (+ i 1))))
      (get-output-string out)))

  (def (process-file path binary-mode tag-mode)
    (let* ((port (if (string=? path "-")
                   (standard-input-port)
                   (with-catch
                     (lambda (e) (die "~a: ~a" path (error-message e)))
                     (lambda () (open-file-input-port path)))))
           (data (read-all-bytes port))
           (hex (md5 data))
           (sep (if binary-mode "*" " ")))
      (unless (string=? path "-") (close-port port))
      (if tag-mode
        (format #t "MD5 (~a) = ~a\n" path hex)
        (format #t "~a ~a~a\n" hex sep path))))

  (def (parse-check-line line)
    (let ((len (string-length line)))
      (cond
        ((and (>= len 4) (string-prefix? "MD5 (" line))
         (let ((paren-pos (let lp ((i 5))
                            (cond ((>= i len) #f)
                                  ((eqv? (string-ref line i) #\)) i)
                                  (else (lp (+ i 1)))))))
           (if (and paren-pos (> (string-length line) (+ paren-pos 4)))
             (let ((filename (substring line 5 paren-pos))
                   (hex (substring line (+ paren-pos 4) len)))
               (list hex filename))
             #f)))
        (else
         (let loop ((i 0))
           (cond
             ((>= i len) #f)
             ((and (< (+ i 1) len)
                   (eqv? (string-ref line i) #\space)
                   (or (eqv? (string-ref line (+ i 1)) #\space)
                       (eqv? (string-ref line (+ i 1)) #\*)))
              (list (substring line 0 i)
                    (substring line (+ i 2) len)))
             (else (loop (+ i 1)))))))))

  (def (do-check checkfile warn-missing)
    (let* ((port (if (string=? checkfile "-")
                   (current-input-port)
                   (with-catch
                     (lambda (e) (die "~a: ~a" checkfile (error-message e)))
                     (lambda () (open-input-file checkfile)))))
           (ok #t)
           (n-failed 0)
           (n-missing 0))
      (let loop ((line (get-line port)))
        (unless (eof-object? line)
          (unless (or (string=? line "") (eqv? (string-ref line 0) #\#))
            (let ((parsed (parse-check-line line)))
              (if (not parsed)
                (warn "improperly formatted checksum line")
                (let* ((expected-hex (car parsed))
                       (filename (cadr parsed))
                       (fport (with-catch
                                (lambda (e) #f)
                                (lambda () (open-file-input-port filename)))))
                  (if (not fport)
                    (begin
                      (when warn-missing
                        (eprintf "~a: ~a: No such file or directory\n"
                                 (program-name) filename))
                      (format #t "~a: FAILED open or read\n" filename)
                      (set! n-missing (+ n-missing 1))
                      (set! ok #f))
                    (let* ((data (read-all-bytes fport))
                           (actual-hex (md5 data)))
                      (close-port fport)
                      (if (string=? expected-hex actual-hex)
                        (format #t "~a: OK\n" filename)
                        (begin
                          (format #t "~a: FAILED\n" filename)
                          (set! n-failed (+ n-failed 1))
                          (set! ok #f)))))))))
          (loop (get-line port))))
      (unless (string=? checkfile "-") (close-port port))
      (when (> n-failed 0)
        (eprintf "~a: WARNING: ~a computed checksum~a did NOT match\n"
                 (program-name) n-failed (if (= n-failed 1) "" "s")))
      (unless ok (exit 1))))

  (def (string-prefix? prefix str)
    (let ((plen (string-length prefix))
          (slen (string-length str)))
      (and (<= plen slen)
           (string=? prefix (substring str 0 plen)))))

  (def (main . args)
    (parameterize ((program-name "md5sum"))
      (call-with-getopt
        (lambda (_ opt)
            (let ((files (if (null? (hash-ref opt 'rest)) '("-") (hash-ref opt 'rest))))
              (if (hash-get opt 'check)
                (for-each
                  (lambda (f) (do-check f (not (hash-get opt 'quiet))))
                  files)
                (for-each
                  (lambda (f) (process-file f (hash-get opt 'binary) (hash-get opt 'tag)))
                  files))))
        args
        'program: "md5sum"
        'help: "Print or check MD5 message digests."
        (flag 'binary "-b" "--binary"
          'help: "read in binary mode")
        (flag 'check "-c" "--check"
          'help: "read MD5 sums from the FILEs and check them")
        (flag 'tag "--tag"
          'help: "create a BSD-style checksum")
        (flag 'quiet "--quiet"
          'help: "don't print OK for each verified file")
        (rest-arguments 'rest))))

  ) ;; end library
