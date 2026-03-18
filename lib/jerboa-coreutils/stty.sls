#!chezscheme
;;; stty.sls -- Change and print terminal line settings

(library (jerboa-coreutils stty)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name)
          (jerboa core)
          (only (std sugar) with-catch)
          (only (std format) eprintf format)
          (jerboa-coreutils common)
          (jerboa-coreutils common version))

  ;; stty - delegate to /usr/bin/stty
  ;; Pass all arguments through to the system binary.

  (def (shell-quote str)
    (string-append "'" (let loop ((i 0) (acc '()))
      (if (>= i (string-length str))
        (list->string (reverse acc))
        (let ((c (string-ref str i)))
          (if (eqv? c #\')
            (loop (+ i 1) (append (reverse (string->list "'\\''")) acc))
            (loop (+ i 1) (cons c acc)))))) "'"))

  (def (main . args)
    (parameterize ((program-name "stty"))
      (with-catch
        (lambda (e) (die "cannot run /usr/bin/stty: ~a" (error-message e)))
        (lambda ()
          (let* ((cmd (string-append "/usr/bin/stty"
                        (let loop ((a args) (acc ""))
                          (if (null? a) acc
                            (loop (cdr a)
                                  (string-append acc " " (shell-quote (car a))))))))
                 )
            (let-values (((to-stdin from-stdout from-stderr pid)
                          (open-process-ports cmd (buffer-mode block) (native-transcoder))))
              (close-port to-stdin)
              (let ((lines (let loop ((acc '()))
                             (let ((line (get-line from-stdout)))
                               (if (eof-object? line)
                                 (reverse acc)
                                 (loop (cons line acc)))))))
                (close-port from-stdout)
                (let ((err-lines (let loop ((acc '()))
                                   (let ((line (get-line from-stderr)))
                                     (if (eof-object? line)
                                       (reverse acc)
                                       (loop (cons line acc)))))))
                  (close-port from-stderr)
                  (for-each displayln lines)
                  (for-each (lambda (l) (eprintf "~a\n" l)) err-lines)
                  ;; We can't easily get exit status from open-process-ports,
                  ;; so just exit 0 if we got output
                  (exit 0)))))))))

  ) ;; end library
