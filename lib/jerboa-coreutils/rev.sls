#!chezscheme
;;; rev.sls -- Reverse lines character by character

(library (jerboa-coreutils rev)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name)
          (jerboa core)
          (only (std sugar) with-catch)
          (jerboa-coreutils common)
          (jerboa-coreutils common version))

  (def (reverse-string s)
    (list->string (reverse (string->list s))))

  (def (rev-port port)
    (let loop ((line (get-line port)))
      (unless (eof-object? line)
        (display (reverse-string line))
        (newline)
        (loop (get-line port)))))

  (def (main . args)
    (parameterize ((program-name "rev"))
      (if (null? args)
        (rev-port (current-input-port))
        (for-each
          (lambda (file)
            (if (string=? file "-")
              (rev-port (current-input-port))
              (with-catch
                (lambda (e) (die "~a: ~a" file (error-message e)))
                (lambda ()
                  (let ((port (open-input-file file)))
                    (rev-port port)
                    (close-port port))))))
          args))))

) ;; end library
