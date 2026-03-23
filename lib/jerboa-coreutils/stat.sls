#!chezscheme
;;; stat.sls -- Display file status (delegates to system stat command)

(library (jerboa-coreutils stat)
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

  (define _load-ffi (begin (load-shared-object #f) (void)))

  (define ffi-chmod (foreign-procedure "chmod" (string int) int))

  ;; Run system stat and capture output
  (def (run-stat-cmd args)
    (with-catch
      (lambda (e) #f)
      (lambda ()
        (let ((cmd (string-append "stat " (string-join-spaces args))))
          (let-values (((to-stdin from-stdout from-stderr pid)
                        (open-process-ports cmd (buffer-mode block) (native-transcoder))))
            (close-port to-stdin)
            (let loop ((acc '()))
              (let ((line (get-line from-stdout)))
                (if (eof-object? line)
                  (begin
                    (close-port from-stdout)
                    (close-port from-stderr)
                    (reverse acc))
                  (loop (cons line acc))))))))))

  (def (string-join-spaces lst)
    (if (null? lst) ""
      (let loop ((rest (cdr lst)) (acc (car lst)))
        (if (null? rest) acc
          (loop (cdr rest) (string-append acc " " (car rest)))))))

  (def (main . args)
    (parameterize ((program-name "stat"))
      (call-with-getopt
        (lambda (_ opt)
          (when (null? (hash-ref opt 'rest))
            (die "missing operand"))
          ;; Build stat command arguments
          (let ((stat-args
                 (append
                   (if (hash-get opt 'no-dereference) '("-L") '())
                   (if (hash-get opt 'terse) '("-t") '())
                   (if (hash-get opt 'format) (list "-c" (hash-ref opt 'format)) '())
                   (if (hash-get opt 'printf) (list (string-append "--printf=" (hash-ref opt 'printf))) '())
                   (map shell-quote (hash-ref opt 'rest)))))
            (let ((output (run-stat-cmd stat-args)))
              (if output
                (for-each displayln output)
                (die "cannot stat '~a': No such file or directory" (car (hash-ref opt 'rest)))))))
        args
        'program: "stat"
        'help: "Display file or file system status."
        (flag 'no-dereference "-L" "--dereference"
          'help: "follow links (default is to not follow)")
        (flag 'terse "-t" "--terse"
          'help: "print the information in terse form")
        (option 'format "-c" "--format"
          'help: "use the specified FORMAT instead of the default"
          'default: #f)
        (option 'printf "--printf"
          'help: "like --format, but interpret backslash escapes, no newline at end"
          'default: #f)
        (rest-arguments 'rest))))

  ) ;; end library
