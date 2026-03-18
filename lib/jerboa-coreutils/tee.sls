#!chezscheme
;;; tee.sls -- Copy stdin to stdout and files

(library (jerboa-coreutils tee)
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

  (def (main . args)
    (parameterize ((program-name "tee"))
      (call-with-getopt
        (lambda (_ opt)
          (let* ((files (hash-ref opt 'rest))
                 (append? (hash-get opt 'append))
                 (ports (map (lambda (f)
                                (with-catch
                                  (lambda (e)
                                    (warn "~a: cannot open for writing" f)
                                    #f)
                                  (lambda ()
                                    (if append?
                                      (open-file-output-port f
                                        (file-options no-fail no-truncate)
                                        (buffer-mode block)
                                        (make-transcoder (utf-8-codec)))
                                      (open-file-output-port f
                                        (file-options no-fail)
                                        (buffer-mode block)
                                        (make-transcoder (utf-8-codec)))))))
                              files)))
              ;; When appending, seek to end of each port
              (when append?
                (for-each
                  (lambda (p)
                    (when (and p (output-port? p))
                      (with-catch void
                        (lambda ()
                          (set-port-position! p (port-length p))))))
                  ports))
              ;; Copy stdin to stdout and all file ports
              (let loop ()
                (let ((c (read-char)))
                  (unless (eof-object? c)
                    (write-char c)
                    (for-each
                      (lambda (p)
                        (when p
                          (with-catch void (lambda () (write-char c p)))))
                      ports)
                    (loop))))
              ;; Close all ports
              (for-each
                (lambda (p) (when p (close-output-port p)))
                ports)))
        args
        'program: "tee"
        'help: "Copy standard input to each FILE, and also to standard output."
        (flag 'append "-a" "--append"
          'help: "append to the given FILEs, do not overwrite")
        (flag 'ignore-interrupts "-i" "--ignore-interrupts"
          'help: "ignore interrupt signals")
        (rest-arguments 'rest))))

  ) ;; end library
