#!chezscheme
;;; cksum.sls -- CRC checksum utility

(library (jerboa-coreutils cksum)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name
            string-upcase)
          (jerboa core)
          (only (std sugar) with-catch)
          (only (std format) eprintf format)
          (std cli getopt)
          (std crypto digest)
          (jerboa-coreutils common)
          (jerboa-coreutils common version))

  ;; POSIX CRC-32 table (unreflected, polynomial 0x04C11DB7)
  (def crc32-table
    (let ((tbl (make-vector 256 0)))
      (let loop ((i 0))
        (when (< i 256)
          (let ((crc-val
                 (let loop2 ((c (arithmetic-shift i 24)) (k 0))
                   (if (< k 8)
                     (loop2 (bitwise-and
                              (if (> (bitwise-and c #x80000000) 0)
                                (bitwise-xor (arithmetic-shift c 1) #x04C11DB7)
                                (arithmetic-shift c 1))
                              #xFFFFFFFF)
                            (+ k 1))
                     c))))
            (vector-set! tbl i crc-val))
          (loop (+ i 1))))
      tbl))

  (def (crc32-update crc byte)
    (let ((idx (bitwise-and (arithmetic-shift crc -24) #xFF)))
      (bitwise-and
        (bitwise-xor (arithmetic-shift crc 8)
                     (vector-ref crc32-table (bitwise-xor idx byte)))
        #xFFFFFFFF)))

  ;; POSIX cksum: CRC with length octets appended, final complement
  (def (compute-posix-cksum port)
    (let loop ((crc 0) (nbytes 0))
      (let ((byte (get-u8 port)))
        (if (eof-object? byte)
          (let len-loop ((n nbytes) (crc crc))
            (if (> n 0)
              (len-loop (arithmetic-shift n -8)
                        (crc32-update crc (bitwise-and n #xFF)))
              (values (bitwise-and (bitwise-xor crc #xFFFFFFFF) #xFFFFFFFF)
                      nbytes)))
          (loop (crc32-update crc byte) (+ nbytes 1))))))

  ;; Read all bytes from port into bytevector
  (def (read-all-bytes port)
    (let loop ((acc '()))
      (let ((byte (get-u8 port)))
        (if (eof-object? byte)
          (u8-list->bytevector (reverse acc))
          (loop (cons byte acc))))))

  ;; Compute hash digest using std/crypto
  (def (compute-hash algo data)
    (case algo
      ((md5)    (md5 data))
      ((sha1)   (sha1 data))
      ((sha224) (sha224 data))
      ((sha256) (sha256 data))
      ((sha384) (sha384 data))
      ((sha512) (sha512 data))
      (else (error "unknown algorithm" algo))))

  ;; Process one file, output checksum
  (def (process-file path algo tag-mode)
    (let* ((port (if (string=? path "-")
                   (standard-input-port)
                   (with-catch
                     (lambda (e) (die "~a: ~a" path (error-message e)))
                     (lambda () (open-file-input-port path)))))
           (display-name (if (string=? path "-") "" path)))
      (if (eq? algo 'crc)
        ;; POSIX CRC
        (call-with-values
          (lambda () (compute-posix-cksum port))
          (lambda (crc nbytes)
            (unless (string=? path "-") (close-port port))
            (if (string=? path "-")
              (format #t "~a ~a\n" crc nbytes)
              (format #t "~a ~a ~a\n" crc nbytes path))))
        ;; Hash algorithm
        (let* ((data (read-all-bytes port))
               (hex (compute-hash algo data)))
          (unless (string=? path "-") (close-port port))
          (if tag-mode
            (format #t "~a (~a) = ~a\n"
                    (string-upcase (symbol->string algo)) path hex)
            (format #t "~a  ~a\n" hex path))))))

  ;; Check mode: read lines like "hexdigest  filename" and verify
  (def (check-file checkfile algo)
    (let* ((port (if (string=? checkfile "-")
                   (current-input-port)
                   (with-catch
                     (lambda (e) (die "~a: ~a" checkfile (error-message e)))
                     (lambda () (open-input-file checkfile)))))
           (ok #t))
      (let loop ((line (get-line port)))
        (unless (eof-object? line)
          (unless (or (string=? line "") (eqv? (string-ref line 0) #\#))
            (let* ((parts (string-split-2spaces line))
                   (expected-hex (if (pair? parts) (car parts) #f))
                   (filename (if (and (pair? parts) (pair? (cdr parts)))
                               (cadr parts)
                               #f)))
              (when (and expected-hex filename)
                (let ((fport (with-catch
                               (lambda (e) #f)
                               (lambda () (open-file-input-port filename)))))
                  (if (not fport)
                    (begin
                      (eprintf "~a: ~a: No such file or directory\n"
                               (program-name) filename)
                      (set! ok #f))
                    (let* ((data (read-all-bytes fport))
                           (actual-hex (compute-hash algo data)))
                      (close-port fport)
                      (if (string=? expected-hex actual-hex)
                        (format #t "~a: OK\n" filename)
                        (begin
                          (format #t "~a: FAILED\n" filename)
                          (set! ok #f)))))))))
          (loop (get-line port))))
      (unless (string=? checkfile "-") (close-port port))
      (unless ok (exit 1))))

  ;; Split "hexdigest  filename" - two spaces separate digest from name
  (def (string-split-2spaces line)
    (let ((len (string-length line)))
      (let loop ((i 0))
        (cond
          ((>= (+ i 1) len) (list line))
          ((and (eqv? (string-ref line i) #\space)
                (eqv? (string-ref line (+ i 1)) #\space))
           (list (substring line 0 i)
                 (substring line (+ i 2) len)))
          (else (loop (+ i 1)))))))

  (def (string-upcase str)
    (list->string (map char-upcase (string->list str))))

  (def (main . args)
    (parameterize ((program-name "cksum"))
      (call-with-getopt
        (lambda (_ opt)
          (let* ((algo-str (or (hash-get opt 'algorithm) "crc"))
                 (algo (cond
                         ((string=? algo-str "crc")    'crc)
                         ((string=? algo-str "md5")    'md5)
                         ((string=? algo-str "sha1")   'sha1)
                         ((string=? algo-str "sha224") 'sha224)
                         ((string=? algo-str "sha256") 'sha256)
                         ((string=? algo-str "sha384") 'sha384)
                         ((string=? algo-str "sha512") 'sha512)
                         (else (die "invalid algorithm '~a'" algo-str))))
                 (check-mode (hash-get opt 'check))
                 (tag-mode (hash-get opt 'tag))
                 (files (if (null? (hash-ref opt 'rest)) '("-") (hash-ref opt 'rest))))
            (if check-mode
              (for-each (lambda (f) (check-file f algo)) files)
              (for-each (lambda (f) (process-file f algo tag-mode)) files))))
        args
        'program: "cksum"
        'help: "Print CRC checksum and byte counts of each FILE."
        (option 'algorithm "-a" "--algorithm"
          'help: "digest type to use: md5, sha1, sha224, sha256, sha384, sha512, crc ('default: crc)"
          'default: #f)
        (flag 'check "-c" "--check"
          'help: "read checksums from file and check them")
        (flag 'tag "--tag"
          'help: "create a BSD-style checksum")
        (rest-arguments 'rest))))

  ) ;; end library
