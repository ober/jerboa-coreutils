#!chezscheme
;;; realpath.sls -- Print resolved absolute path

(library (jerboa-coreutils realpath)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name)
          (jerboa core)
          (only (std sugar) with-catch)
          (only (std format) eprintf)
          (std cli getopt)
          (jerboa-coreutils common)
          (jerboa-coreutils common version))

  (def (ffi-realpath-c path)
    ;; Use system realpath command
    (with-catch
      (lambda (e) #f)
      (lambda ()
        (let-values (((to-stdin from-stdout from-stderr pid)
                      (open-process-ports
                        (string-append "/usr/bin/realpath -- " (shell-quote path))
                        (buffer-mode block)
                        (native-transcoder))))
          (close-port to-stdin)
          (let ((result (get-line from-stdout)))
            (close-port from-stdout)
            (close-port from-stderr)
            (if (eof-object? result) #f result))))))

  (def (shell-quote s)
    (string-append "'" (string-replace-all s "'" "'\\''") "'"))

  (def (string-replace-all str old new)
    (let ((olen (string-length old))
          (out (open-output-string)))
      (let loop ((i 0))
        (cond
          ((> (+ i olen) (string-length str))
           (display (substring str i (string-length str)) out)
           (get-output-string out))
          ((string=? (substring str i (+ i olen)) old)
           (display new out)
           (loop (+ i olen)))
          (else
           (write-char (string-ref str i) out)
           (loop (+ i 1)))))))

  (def (main . args)
    (parameterize ((program-name "realpath"))
      (call-with-getopt
        (lambda (_ opt)
            (when (null? (hash-ref opt 'rest))
              (die "missing operand"))
            (let ((had-error #f)
                  (delim (if (hash-get opt 'zero) #\nul #\newline))
                  (relative-to (hash-get opt 'relative-to))
                  (relative-base (hash-get opt 'relative-base)))
              (for-each
                (lambda (file)
                  (with-catch
                    (lambda (e)
                      (unless (hash-get opt 'quiet)
                        (warn "'~a': ~a" file (error-message e)))
                      (set! had-error #t))
                    (lambda ()
                      (let ((resolved (or (and (not (hash-get opt 'no-symlinks))
                                              (ffi-realpath-c file))
                                          (strip-trailing-slash
                                            (path-expand file)))))
                        (let ((result (cond
                                        (relative-to
                                         (make-relative resolved
                                           (path-normalize relative-to)))
                                        (relative-base
                                         (let ((base (path-normalize relative-base)))
                                           (if (string-prefix? base resolved)
                                             (make-relative resolved base)
                                             resolved)))
                                        (else resolved))))
                          (display result)
                          (write-char delim))))))
                (hash-ref opt 'rest))
              (when had-error
                (exit EXIT_FAILURE))))
        args
        'program: "realpath"
        'help: "Print the resolved absolute file name."
        (flag 'canonicalize-existing "-e" "--canonicalize-existing"
          'help: "all components of the path must exist")
        (flag 'canonicalize-missing "-m" "--canonicalize-missing"
          'help: "no path components need exist or be a directory")
        (flag 'no-symlinks "-s" "--no-symlinks"
          'help: "don't expand symlinks")
        (flag 'quiet "-q" "--quiet"
          'help: "suppress most error messages")
        (option 'relative-to "--relative-to"
          'help: "print the resolved path relative to DIR"
          'default: #f)
        (option 'relative-base "--relative-base"
          'help: "print abs paths unless paths below DIR"
          'default: #f)
        (flag 'zero "-z" "--zero"
          'help: "end each output line with NUL, not newline")
        (rest-arguments 'rest))))

  (def (strip-trailing-slash path)
    (let ((len (string-length path)))
      (if (and (> len 1)
               (eqv? (string-ref path (- len 1)) #\/))
        (substring path 0 (- len 1))
        path)))

  (def (string-prefix? prefix str)
    (and (>= (string-length str) (string-length prefix))
         (string=? (substring str 0 (string-length prefix)) prefix)))

  (def (make-relative path base)
    ;; Make path relative to base
    (let* ((base (if (and (> (string-length base) 1)
                          (eqv? (string-ref base (- (string-length base) 1)) #\/))
                   base
                   (string-append base "/")))
           (plen (string-length path))
           (blen (string-length base)))
      (if (and (>= plen blen)
               (string=? (substring path 0 blen) base))
        (substring path blen plen)
        path)))

  ) ;; end library
