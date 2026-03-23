#!chezscheme
;;; pinky.sls -- Lightweight finger information lookup (delegates to /usr/bin/pinky)

(library (jerboa-coreutils pinky)
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
          (jerboa-coreutils common version)
          (jerboa-coreutils common security))

  (def (run-pinky pinky-args)
    (with-catch
      (lambda (e)
        (die "cannot run /usr/bin/pinky: ~a" (error-message e)))
      (lambda ()
        (let ((cmd (string-append "/usr/bin/pinky"
                     (let loop ((args pinky-args) (acc ""))
                       (if (null? args) acc
                         (loop (cdr args)
                               (string-append acc " " (shell-quote (car args)))))))))
          (let-values (((to-stdin from-stdout from-stderr pid)
                        (open-process-ports cmd (buffer-mode block) (native-transcoder))))
            (close-port to-stdin)
            (let ((lines (let loop ((acc '()))
                           (let ((line (get-line from-stdout)))
                             (if (eof-object? line)
                               (reverse acc)
                               (loop (cons line acc)))))))
              (close-port from-stdout)
              (close-port from-stderr)
              (for-each displayln lines)))))))

  (def (main . args)
    (init-security!)
    (parameterize ((program-name "pinky"))
      (call-with-getopt
        (lambda (_ opt)
            (let ((pinky-args
                   (append
                     (if (hash-get opt 'long) '("-l") '())
                     (if (hash-get opt 'short) '("-s") '())
                     (if (hash-get opt 'no-name) '("-f") '())
                     (if (hash-get opt 'no-heading) '("-h") '())
                     (if (hash-get opt 'no-plan) '("-p") '())
                     (if (hash-get opt 'no-project) '("-b") '())
                     (hash-ref opt 'rest))))
              (run-pinky pinky-args)))
        args
        'program: "pinky"
        'help: "Print user information (lightweight finger)."
        (flag 'long "-l"
          'help: "produce long format output for specified users")
        (flag 'short "-s"
          'help: "do short format output (default)")
        (flag 'no-name "-f"
          'help: "omit the user's full name in short format")
        (flag 'no-heading "--no-heading"
          'help: "omit the user's full name and remote host in short format")
        (flag 'no-plan "-p"
          'help: "omit the user's home directory and shell in long format")
        (flag 'no-project "-b"
          'help: "omit the user's home directory and shell in long format, and also the project file")
        (rest-arguments 'rest))))

  ) ;; end library
