#!chezscheme
;;; tac.sls -- Print lines in reverse order

(library (jerboa-coreutils tac)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name
            with-input-from-string with-output-to-string)
          (jerboa core)
          (only (std sugar) with-catch try catch finally)
          (only (std format) eprintf format)
          (std cli getopt)
          (std misc ports)
          (jerboa-coreutils common)
          (jerboa-coreutils common version)
          (jerboa-coreutils common security))

  (def (main . args)
    (parameterize ((program-name "tac"))
      (init-security!)
      (install-readonly-seccomp!)
      (call-with-getopt
        (lambda (_ opt)
          (let ((files (hash-ref opt 'rest))
                (separator (or (hash-get opt 'separator) "\n"))
                (before? (hash-get opt 'before)))
            (let ((files (if (null? files) '("-") files)))
              (for-each
                (lambda (file)
                  (tac-file file separator before?))
                files))))
        args
        'program: "tac"
        'help: "Write each FILE to standard output, last line first."
        (flag 'before "-b" "--before"
          'help: "attach the separator before instead of after")
        (option 'separator "-s" "--separator"
          'help: "use STRING as the separator instead of newline"
          'default: #f)
        (rest-arguments 'rest))))

  (def (tac-file file separator before?)
    (let ((proc
      (lambda (port)
        (let* ((content (read-all-as-string port))
               (len (string-length content))
               (seplen (string-length separator)))
          (when (> len 0)
            (let* ((ends-with-sep? (and (>= len seplen)
                                         (string=? (substring content (- len seplen) len)
                                                   separator)))
                   (lines (string-split-sep content separator))
                   ;; If content ends with separator, last element is empty - remove it
                   (lines (if (and ends-with-sep?
                                   (pair? lines)
                                   (string=? (last-element lines) ""))
                            (drop-last lines)
                            lines))
                   (reversed (reverse lines))
                   ;; If content doesn't end with separator, merge first two reversed elements
                   (reversed (if (and (not ends-with-sep?)
                                      (pair? reversed)
                                      (pair? (cdr reversed)))
                               (cons (string-append (car reversed) (cadr reversed))
                                     (cddr reversed))
                               reversed)))
              (let loop ((rest reversed) (first? #t))
                (when (pair? rest)
                  (if before?
                    (begin
                      (display separator)
                      (display (car rest)))
                    (begin
                      (display (car rest))
                      (display separator)))
                  (loop (cdr rest) #f)))))))))
      (if (equal? file "-")
        (proc (current-input-port))
        (with-catch
          (lambda (e) (warn "~a: No such file or directory" file))
          (lambda ()
            (let ((port (open-input-file file)))
              (try (proc port)
                (finally (close-input-port port)))))))))

  (def (last-element lst)
    (if (null? (cdr lst)) (car lst) (last-element (cdr lst))))

  (def (drop-last lst)
    (if (null? (cdr lst)) '()
      (cons (car lst) (drop-last (cdr lst)))))

  (def (string-split-sep str sep)
    (let ((slen (string-length str))
          (seplen (string-length sep)))
      (if (= seplen 0)
        (list str)
        (let loop ((i 0) (start 0) (acc '()))
          (cond
            ((> (+ i seplen) slen)
             (reverse (cons (substring str start slen) acc)))
            ((string=? (substring str i (+ i seplen)) sep)
             (loop (+ i seplen) (+ i seplen)
                   (cons (substring str start i) acc)))
            (else
             (loop (+ i 1) start acc)))))))

  ) ;; end library
