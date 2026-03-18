#!chezscheme
;;; sum.sls -- Checksum and count the blocks in a file

(library (jerboa-coreutils sum)
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
          (jerboa-coreutils common version))

  ;; BSD checksum: 16-bit rotate-right checksum
  (def (bsd-checksum port)
    (let loop ((checksum 0) (nbytes 0))
      (let ((byte (get-u8 port)))
        (if (eof-object? byte)
          (values (bitwise-and checksum #xFFFF) nbytes)
          (let* ((rotated (bitwise-ior
                            (bitwise-arithmetic-shift-right checksum 1)
                            (bitwise-arithmetic-shift-left (bitwise-and checksum 1) 15)))
                 (new-cksum (bitwise-and (+ rotated byte) #xFFFF)))
            (loop new-cksum (+ nbytes 1)))))))

  ;; SysV checksum: sum all bytes mod 65535
  (def (sysv-checksum port)
    (let loop ((sum 0) (nbytes 0))
      (let ((byte (get-u8 port)))
        (if (eof-object? byte)
          (let* ((r (modulo sum 65535)))
            (values r nbytes))
          (loop (+ sum byte) (+ nbytes 1))))))

  (def (process-sum-file path sysv-mode)
    (let ((port (if (string=? path "-")
                  (standard-input-port)
                  (with-catch
                    (lambda (e) (die "~a: ~a" path (error-message e)))
                    (lambda () (open-file-input-port path))))))
      (call-with-values
        (lambda ()
          (if sysv-mode
            (sysv-checksum port)
            (bsd-checksum port)))
        (lambda (cksum nbytes)
          (unless (string=? path "-") (close-port port))
          (let ((blocks (if sysv-mode
                          (inexact->exact (ceiling (/ nbytes 512)))
                          (inexact->exact (ceiling (/ nbytes 1024))))))
            (if sysv-mode
              (if (string=? path "-")
                (displayln cksum " " blocks)
                (displayln cksum " " blocks " " path))
              (if (string=? path "-")
                (displayln (pad-checksum cksum sysv-mode)
                           (rjust-num blocks 6))
                (displayln (pad-checksum cksum sysv-mode)
                           (rjust-num blocks 6) " " path))))))))

  ;; Right-justify a number in a field width
  (def (rjust-num n width)
    (let loop ((s (number->string n)))
      (if (>= (string-length s) width) s
        (loop (string-append " " s)))))

  ;; BSD checksum is displayed as 5-digit zero-padded decimal
  (def (pad-checksum cksum sysv-mode)
    (if sysv-mode
      (number->string cksum)
      (let ((s (number->string cksum)))
        (let loop ((result s))
          (if (>= (string-length result) 5) result
            (loop (string-append "0" result)))))))

  (def (main . args)
    (parameterize ((program-name "sum"))
      (call-with-getopt
        (lambda (_ opt)
          (let ((files (if (null? (hash-ref opt 'rest)) '("-") (hash-ref opt 'rest))))
            (for-each
              (lambda (f) (process-sum-file f (hash-get opt 'sysv)))
              files)))
        args
        'program: "sum"
        'help: "Print checksum and block counts for each FILE."
        (flag 'sysv "-s" "--sysv"
          'help: "use System V sum algorithm, print 512 byte blocks")
        (flag 'bsd "-r"
          'help: "use BSD sum algorithm (default)")
        (rest-arguments 'rest))))

  ) ;; end library
