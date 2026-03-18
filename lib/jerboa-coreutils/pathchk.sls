#!chezscheme
;;; pathchk.sls -- Check path validity

(library (jerboa-coreutils pathchk)
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

  ;; POSIX limits
  (def PATH_MAX 4096)
  (def NAME_MAX 255)
  ;; POSIX portable character set
  (def PORTABLE_CHARS
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789._-")

  (def (portable-char? c)
    (let loop ((i 0))
      (cond
        ((>= i (string-length PORTABLE_CHARS)) #f)
        ((eqv? c (string-ref PORTABLE_CHARS i)) #t)
        (else (loop (+ i 1))))))

  (def (string-split-char str ch)
    (let loop ((i 0) (start 0) (parts '()))
      (cond
        ((>= i (string-length str))
         (reverse (cons (substring str start i) parts)))
        ((eqv? (string-ref str i) ch)
         (loop (+ i 1) (+ i 1) (cons (substring str start i) parts)))
        (else
         (loop (+ i 1) start parts)))))

  ;; Check a pathname; return list of error strings (empty = ok)
  (def (check-path path posix-portable extra-checks)
    (let ((errors '()))
      (define (add-err msg)
        (set! errors (cons msg errors)))

      ;; Empty path
      (when (string=? path "")
        (add-err (format "~a: empty file name" path)))

      (unless (string=? path "")
        ;; Path too long
        (when (>= (string-length path) PATH_MAX)
          (add-err (format "~a: path name is too long" path)))

        (let ((components (string-split-char path #\/)))
          (for-each
            (lambda (comp)
              (unless (string=? comp "")
                ;; Component too long
                (when (> (string-length comp) NAME_MAX)
                  (add-err (format "~a: name too long" path)))

                ;; Extra checks (-P): leading hyphen
                (when extra-checks
                  (when (and (> (string-length comp) 0)
                             (eqv? (string-ref comp 0) #\-))
                    (add-err (format "~a: leading '-' in a component of file name" path))))

                ;; Portable character check (-p or --portability)
                (when posix-portable
                  (let loop ((i 0))
                    (when (< i (string-length comp))
                      (let ((c (string-ref comp i)))
                        (unless (portable-char? c)
                          (add-err (format "~a: nonportable character '~a' in file name"
                                           path c))))
                      (loop (+ i 1)))))))
            components)))

      (reverse errors)))

  (def (main . args)
    (parameterize ((program-name "pathchk"))
      (call-with-getopt
        (lambda (_ opt)
            (when (null? (hash-ref opt 'rest))
              (die "missing operand"))
            (let* ((posix-portable (or (hash-get opt 'posix) (hash-get opt 'portability)))
                   (extra-checks (or (hash-get opt 'extra) (hash-get opt 'portability)))
                   (ok #t))
              (for-each
                (lambda (path)
                  (let ((errs (check-path path posix-portable extra-checks)))
                    (for-each
                      (lambda (e)
                        (eprintf "~a\n" e)
                        (set! ok #f))
                      errs)))
                (hash-ref opt 'rest))
              (unless ok
                (exit 1))))
        args
        'program: "pathchk"
        'help: "Diagnose invalid or unportable file names."
        (flag 'posix "-p"
          'help: "check for most POSIX systems")
        (flag 'extra "-P"
          'help: "check for empty names and leading '-'")
        (flag 'portability "--portability"
          'help: "check for all POSIX systems (equivalent to -p -P)")
        (rest-arguments 'rest))))

  ) ;; end library
