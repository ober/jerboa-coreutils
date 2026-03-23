#!chezscheme
;;; readlink.sls -- Print symlink target

(library (jerboa-coreutils readlink)
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

  ;; readlink via foreign-procedure: readlink fills a buffer, returns length.
  ;; Since we can't easily use static buffers with Chez foreign-procedure,
  ;; we use Chez's built-in symlink support or shell out.

  ;; Chez has no built-in readlink, so we use a C helper via load-shared-object.
  ;; Alternative: shell out to /usr/bin/readlink
  (def (ffi-readlink path)
    (with-catch
      (lambda (e) #f)
      (lambda ()
        (let-values (((to-stdin from-stdout from-stderr pid)
                      (open-process-ports
                        (string-append "/usr/bin/readlink -n " (shell-quote path))
                        (buffer-mode block)
                        (native-transcoder))))
          (close-port to-stdin)
          (let ((result (get-string-all from-stdout)))
            (close-port from-stdout)
            (close-port from-stderr)
            (if (or (eof-object? result) (string=? result ""))
              #f
              result))))))

  (def (ffi-realpath path)
    (with-catch
      (lambda (e) #f)
      (lambda ()
        (let-values (((to-stdin from-stdout from-stderr pid)
                      (open-process-ports
                        (string-append "/usr/bin/realpath -s " (shell-quote path))
                        (buffer-mode block)
                        (native-transcoder))))
          (close-port to-stdin)
          (let ((result (get-line from-stdout)))
            (close-port from-stdout)
            (close-port from-stderr)
            (if (eof-object? result) #f result))))))


  (def (main . args)
    (parameterize ((program-name "readlink"))
      (call-with-getopt
        (lambda (_ opt)
            (when (null? (hash-ref opt 'rest))
              (die "missing operand"))
            (let ((had-error #f)
                  (delim (if (hash-get opt 'zero) #\nul #\newline)))
              (for-each
                (lambda (file)
                  (cond
                    ((or (hash-get opt 'canonicalize) (hash-get opt 'canonicalize-existing) (hash-get opt 'canonicalize-missing))
                     (let ((resolved (ffi-realpath file)))
                       (if resolved
                         (begin (display resolved) (write-char delim))
                         (begin
                           (unless (hash-get opt 'quiet)
                             (warn "'~a': No such file or directory" file))
                           (set! had-error #t)))))
                    (else
                     (let ((target (ffi-readlink file)))
                       (if target
                         (begin (display target) (write-char delim))
                         (begin
                           (unless (hash-get opt 'quiet)
                             (warn "'~a': Invalid argument" file))
                           (set! had-error #t)))))))
                (hash-ref opt 'rest))
              (when had-error
                (exit EXIT_FAILURE))))
        args
        'program: "readlink"
        'help: "Print resolved symbolic links or canonical file names."
        (flag 'canonicalize "-f" "--canonicalize"
          'help: "canonicalize by following every symlink; all but last component must exist")
        (flag 'canonicalize-existing "-e" "--canonicalize-existing"
          'help: "canonicalize, all components must exist")
        (flag 'canonicalize-missing "-m" "--canonicalize-missing"
          'help: "canonicalize without requirements on components")
        (flag 'no-newline "-n" "--no-newline"
          'help: "do not output the trailing delimiter")
        (flag 'quiet "-q" "--quiet"
          'help: "suppress most error messages")
        (flag 'zero "-z" "--zero"
          'help: "end each output line with NUL, not newline")
        (rest-arguments 'rest))))

  ) ;; end library
