#!chezscheme
;;; sha512sum.sls -- SHA512 hash

(library (jerboa-coreutils sha512sum)
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
          (jerboa-coreutils common version)
          (jerboa-coreutils common security))

  (def (read-all-bytes port)
    (let loop ((acc '()))
      (let ((byte (get-u8 port)))
        (if (eof-object? byte)
          (u8-list->bytevector (reverse acc))
          (loop (cons byte acc))))))

  (def (process-file path binary-mode tag-mode)
    (let* ((port (if (string=? path "-")
                   (standard-input-port)
                   (with-catch
                     (lambda (e) (die "~a: ~a" path (error-message e)))
                     (lambda () (open-file-input-port path)))))
           (data (read-all-bytes port))
           (hex (sha512 data))
           (sep (if binary-mode "*" " ")))
      (unless (string=? path "-") (close-port port))
      (if tag-mode
        (format #t "SHA512 (~a) = ~a\n" path hex)
        (format #t "~a ~a~a\n" hex sep path))))

  (def (parse-check-line line)
    (let ((len (string-length line)))
      (cond
        ((and (>= len 7) (string-prefix? "SHA512 (" line))
         (let ((paren-pos (let lp ((i 8))
                            (cond ((>= i len) #f)
                                  ((eqv? (string-ref line i) #\)) i)
                                  (else (lp (+ i 1)))))))
           (if (and paren-pos (> (string-length line) (+ paren-pos 4)))
             (list (substring line (+ paren-pos 4) len)
                   (substring line 8 paren-pos))
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
           (n-failed 0))
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
                      (format #t "~a: FAILED open or read\n" filename)
                      (set! n-failed (+ n-failed 1))
                      (set! ok #f))
                    (let* ((data (read-all-bytes fport))
                           (actual-hex (sha512 data)))
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
    (parameterize ((program-name "sha512sum"))
      (init-security!)
      (install-readonly-seccomp!)
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
        'program: "sha512sum"
        'help: "Print or check SHA512 message digests."
        (flag 'binary "-b" "--binary"
          'help: "read in binary mode")
        (flag 'check "-c" "--check"
          'help: "read SHA512 sums from the FILEs and check them")
        (flag 'tag "--tag"
          'help: "create a BSD-style checksum")
        (flag 'quiet "--quiet"
          'help: "don't print OK for each verified file")
        (rest-arguments 'rest))))

  ) ;; end library
