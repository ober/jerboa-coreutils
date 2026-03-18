#!chezscheme
;;; fold.sls -- Wrap lines to specified width

(library (jerboa-coreutils fold)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name)
          (jerboa core)
          (only (std sugar) with-catch try catch finally)
          (only (std format) eprintf format)
          (std cli getopt)
          (jerboa-coreutils common)
          (jerboa-coreutils common version))

  (def (fold-line line width bytes? spaces?)
    (let ((len (string-length line)))
      (let loop ((i 0) (col 0) (last-space #f) (line-start 0))
        (cond
          ((>= i len)
           ;; Output remaining
           (display (substring line line-start len)))
          (else
            (let* ((c (string-ref line i))
                   (new-col (if bytes?
                              (+ col 1)
                              (cond
                                ((eqv? c #\tab)
                                 (* (+ (quotient col 8) 1) 8))
                                ((eqv? c #\backspace)
                                 (max 0 (- col 1)))
                                ((eqv? c #\return) 0)
                                (else (+ col 1)))))
                   (new-last-space (if (eqv? c #\space) i last-space)))
              (if (> new-col width)
                (if (and spaces? new-last-space (> new-last-space line-start))
                  ;; Break at last space
                  (begin
                    (display (substring line line-start (+ new-last-space 1)))
                    (newline)
                    (let ((new-start (+ new-last-space 1)))
                      (loop new-start 0 #f new-start)))
                  ;; Hard break before current char
                  (begin
                    (display (substring line line-start i))
                    (newline)
                    (loop i 0 #f i)))
                (loop (+ i 1) new-col new-last-space line-start))))))))

  (def (fold-file file width bytes? spaces?)
    (let ((proc
      (lambda (port)
        ;; Read the entire line, then fold it
        (let loop ()
          (let ((line (get-line port)))
            (unless (eof-object? line)
              (fold-line line width bytes? spaces?)
              (newline)
              (loop)))))))
      (if (equal? file "-")
        (proc (current-input-port))
        (with-catch
          (lambda (e) (warn "~a: No such file or directory" file))
          (lambda ()
            (let ((port (open-input-file file)))
              (try (proc port)
                (finally (close-input-port port)))))))))

  (def (main . args)
    (parameterize ((program-name "fold"))
      (call-with-getopt
        (lambda (_ opt)
          (let ((files (hash-ref opt 'rest))
                (width (string->number (or (hash-get opt 'width) "80")))
                (bytes? (hash-get opt 'bytes))
                (spaces? (hash-get opt 'spaces)))
              (let ((files (if (null? files) '("-") files)))
                (for-each
                  (lambda (file)
                    (fold-file file width bytes? spaces?))
                  files))))
        args
        'program: "fold"
        'help: "Wrap input lines in each FILE, writing to standard output."
        (option 'width "-w" "--width"
          'help: "use WIDTH columns instead of 80" 'default: #f)
        (flag 'bytes "-b" "--bytes"
          'help: "count bytes rather than columns")
        (flag 'spaces "-s" "--spaces"
          'help: "break at spaces")
        (rest-arguments 'rest))))

  ) ;; end library
