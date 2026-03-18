#!chezscheme
;;; tty.sls -- Print the file name of the terminal connected to stdin

(library (jerboa-coreutils tty)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name)
          (jerboa core)
          (only (std sugar) with-catch)
          (std cli getopt)
          (jerboa-coreutils common)
          (jerboa-coreutils common version))

  (def (string-prefix? prefix str)
    (and (>= (string-length str) (string-length prefix))
         (string=? (substring str 0 (string-length prefix)) prefix)))

  (def (read-link-path path)
    (with-catch
      (lambda (e) #f)
      (lambda ()
        (let ((p (process "/usr/bin/readlink" "-f" path)))
          (let ((result (get-line (car p))))
            (close-input-port (car p))
            (close-output-port (cadr p))
            (if (and (string? result) (not (eof-object? result)))
              result
              #f))))))

  ;; Get terminal name via /proc/self/fd/0
  (def (get-tty-name)
    (with-catch
      (lambda (e) #f)
      (lambda ()
        (let ((link (read-link-path "/proc/self/fd/0")))
          (if (and link (string-prefix? "/dev/" link))
            link
            #f)))))

  (def (isatty? port)
    ;; Check if stdin is connected to a terminal by resolving /proc/self/fd/0
    (let ((target (read-link-path "/proc/self/fd/0")))
      (and target
           (string-prefix? "/dev/" target)
           (or (string-prefix? "/dev/pts/" target)
               (string-prefix? "/dev/tty" target)))))

  (def (main . args)
    (parameterize ((program-name "tty"))
      (call-with-getopt
        (lambda (_ opt)
          (let ((tty-name (get-tty-name))
                (is-tty (isatty? (current-input-port))))
            (unless (hash-get opt 'silent)
              (if is-tty
                (displayln (or tty-name "not a tty"))
                (displayln "not a tty")))
            (exit (if is-tty 0 1))))
        args
        'program: "tty"
        'help: "Print the file name of the terminal connected to standard input."
        (flag 'silent "-s" "--silent"
          'help: "print nothing, only return an exit status"))))

  ) ;; end library
